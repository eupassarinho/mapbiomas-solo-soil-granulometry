
# Required modules --------------------------------------------------------

require(terra)       # For handling geospatial data
require(dplyr)       # For data wrangling
require(mpspline2)   # For spline soil data harmonizing
require(leaflet)     # For geospatial data viz
require(leaflet.esri)# For geospatial data viz
require(readr)
require(dplyr)
require(here)
require(tidyr)
require(writexl)

require(doParallel)
cl <- makeCluster(detectCores()-2)
registerDoParallel(cl)

# Locating files ----------------------------------------------------------
# Oculting personal path:

remove_tail <- function(x, sep = "/", del = 1){
  sapply(strsplit(x, split = sep, fixed = TRUE),
         function(i) paste(head(i, -del), collapse = sep))
}

# Directory were PronaSolos rasters are stored:
raw_data_path <- paste0(
  remove_tail(here(), del = 6), 
  "/Área de Trabalho/2024-03-25-dados_granulometria_PronaSolos")

list_of_interest_files <- list.files(raw_data_path, pattern = ".tif", full.names = T)

# PronaSolos granulometry -------------------------------------------------

PronaSolos_granulometry_rasters <- lapply(list_of_interest_files, rast)
PronaSolos_granulometry_rasters <- rast(PronaSolos_granulometry_rasters)

# Test
plot(PronaSolos_granulometry_rasters$`br_clay_content_0-5cm_pred_g_kg`)

# Matching Geodetic Reference System of PronaSolos and MapBiomas S --------
# "febr-superconjunto_processed_granulometry.shp" data path (in the local machine):
data_path <- dirname(here())

spatial_points <- vect(
  paste0(here(),
         "/project_products/01-febr_superconjunto-processed_granulometry.shp"))
crs(spatial_points)

spatial_points <- project(spatial_points, crs(PronaSolos_granulometry_rasters))

crs(spatial_points)

# Test
plot(spatial_points)

# Collecting PronaSolos samples -------------------------------------------

FEBR_and_PronaS_samples <- terra::extract(
  PronaSolos_granulometry_rasters, spatial_points, na.rm = TRUE,
  method = "simple", touches = TRUE, bind = T) %>% as_tibble()

# Don't need for clustering from here:
stopCluster(cl)

# Harmonizing PronaSolos 1 (weighted average) -----------------------------

FEBR_and_PronaS_samples <- FEBR_and_PronaS_samples %>% 
  mutate(`PronaS_00_30cm_clay_g_kg` = (`br_clay_content_0-5cm_pred_g_kg`*(1/6))+
           (`br_clay_content_5-15cm_pred_g_kg`*(2/6))+
           (`br_clay_content_15-30cm_pred_g_kg`*(3/6))) %>% 
  mutate(`PronaS_00_30cm_sand_g_kg` = (`br_sand_content_0-5cm_pred_g_kg`*(1/6))+
           (`br_sand_content_5-15cm_pred_g_kg`*(2/6))+
           (`br_sand_content_15-30cm_pred_g_kg`*(3/6))) %>% 
  mutate(`PronaS_00_30cm_silt_g_kg` = (`br_silt_content_0-5cm_pred_g_kg`*(1/6))+
           (`br_silt_content_5-15cm_pred_g_kg`*(2/6))+
           (`br_silt_content_15-30cm_pred_g_kg`*(3/6)))

write_xlsx(FEBR_and_PronaS_samples,
           "./project_products/02-febr_and_PronaSolos_harmz_w_average.xlsx",
           col_names = T)

# Harmonizing PronaSolos (mass preserving method) -------------------------

# Harmonizing PronaSolos samples from layers (0 - 5 cm, 5 - 15, 15 - 30,
# 30 - 60, and 60 - 100) to single layer (0 - 30 cm):

preparing_data_for_mpspline2 <- function(
    input_dataset = FEBR_and_PronaS_samples, columns = contains("br_clay_content"),
    new_names_column = "br_soil_layer", new_values_column = "br_clay_content_g_kg") {
  
  # Default is for clay dataset:
  input_dataset %>% 
    pivot_longer(cols = columns, names_to = new_names_column,
                 values_to = new_values_column) %>% 
    mutate(Upper.Boundary = ifelse(
      grepl("0-5cm", .$br_soil_layer), 0,
      ifelse(grepl("5-15cm", .$br_soil_layer), 5,
             ifelse(grepl("15-30cm", .$br_soil_layer), 15,
                    ifelse(grepl("30-60cm", .$br_soil_layer), 30, 60))))) %>% 
    mutate(Lower.Boundary = ifelse(
      grepl("0-5cm", .$br_soil_layer), 5,
      ifelse(grepl("5-15cm", .$br_soil_layer), 15,
             ifelse(grepl("15-30cm", .$br_soil_layer), 30,
                    ifelse(grepl("30-60cm", .$br_soil_layer), 60, 100)))))
  
}

PronaS_clay <- preparing_data_for_mpspline2() %>% 
  select(c(soil_ID, contains("Boundary"), br_clay_content_g_kg))
PronaS_sand <- preparing_data_for_mpspline2(columns = contains("br_sand_content"),
                                            new_values_column = "br_sand_content_g_kg") %>% 
  select(c(soil_ID, contains("Boundary"), br_sand_content_g_kg))
PronaS_silt <- preparing_data_for_mpspline2(columns = contains("br_silt_content"),
                                            new_values_column = "br_silt_content_g_kg") %>% 
  select(c(soil_ID, contains("Boundary"), br_silt_content_g_kg))

# Function to harmonize soil data in standard depths. Harmonizing metho is
# mass preserving with equal-area quadratic smoothing splines. See method at:
# Modelling soil attribute depth functions with equal-area quadratic smoothing
# splines: https://doi.org/10.1016/S0016-7061(99)00003-8

harm_by_mpsspline <- function(
    # Input args: data.frame and soil property:
  object = PronaS_clay, soil_prop = "br_clay_content_g_kg") {
  
  # mpspline function to harmonize:
  res <- mpspline2::mpspline(
    obj = object, var_name = soil_prop,
    # Standard soil depth and mass conservation rule:
    d = c(0, 30, 100), vlow = 0, vhigh = 1000) # vlow and vhigh in g/kg [0, 1000]
  
  # Handling harmonized object:
  harmonized <- list()
  
  for (i in seq(along.with = res)) {
    
    # Retrieving only set standard depth harmonized soil property:
    harmonized[[i]] <- bind_cols(
      data.frame(Soil.ID = names(res[i])),
      res[[i]][["est_dcm"]]  %>% t() %>% as.data.frame(),
      res[[i]][["est_err"]] %>% t() %>% as.data.frame())
    
  }
  
  harmonized <- bind_rows(harmonized)
  
  # Output: simple data frame:
  return(harmonized)
  
}

# Applying harm_by_mpsspline function over three datasets:
argila_harmonized <- harm_by_mpsspline()
argila_harmonized <- argila_harmonized %>% 
  rename_at(vars(!Soil.ID), function(x) paste0(x, "_br_clay_content_g_kg"))

silte_harmonized <- harm_by_mpsspline(PronaS_silt, "br_silt_content_g_kg")
silte_harmonized <- silte_harmonized %>% 
  rename_at(vars(!Soil.ID), function(x) paste0(x, "_br_silt_content_g_kg"))

areia_harmonized <- harm_by_mpsspline(PronaS_sand, "br_sand_content_g_kg")
areia_harmonized <- areia_harmonized %>% 
  rename_at(vars(!Soil.ID), function(x) paste0(x, "_br_sand_content_g_kg"))

full_data_PronaS_harmd <- right_join(
  argila_harmonized, areia_harmonized, by = "Soil.ID")

full_data_PronaS_harmd <- right_join(
  full_data_PronaS_harmd, silte_harmonized, by = "Soil.ID")

remove(preparing_data_for_mpspline2, PronaS_clay, PronaS_sand, PronaS_silt,
       harm_by_mpsspline, argila_harmonized, silte_harmonized, areia_harmonized)

write_xlsx(full_data_PronaS_harmd,
           "./project_products/02-febr_and_PronaSolos_harmz_mpspline2.xlsx",
           col_names = T)
