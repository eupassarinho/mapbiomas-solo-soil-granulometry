
# Required modules --------------------------------------------------------

require(terra)       # For handling geospatial data
require(dplyr)       # For data wrangling
require(mpspline2)   # For spline soil data harmonizing
require(leaflet)     # For geospatial data viz
require(leaflet.esri)# For geospatial data viz

# Loading raw data from FEBR ----------------------------------------------

# Loading FEBR superconjunto:
original_data <- read.table("./febr-superconjunto.txt", header = T, sep = ";")

# Handling soil data ------------------------------------------------------

# Casting geographical coordinates to numeric vectors and filtering only
# samples with available granulometry:
data <- original_data %>% 
  mutate(coord_x = sub(",", ".", original_data$coord_x) %>% as.numeric(),
         coord_y = sub(",", ".", original_data$coord_y) %>% as.numeric()) %>% 
  filter(!is.na(.$argila), !is.na(.$coord_x)) %>% 
  ## Renaming variables:
  rename(Upper.Boundary = profund_sup, Lower.Boundary = profund_inf,
         Soil.ID = observacao_id)

# Casting datatype for numeric:
data[,"terrafina"] <- as.numeric(data[,"terrafina"])
data[,"argila"] <- as.numeric(data[,"argila"])
data[,"silte"] <- as.numeric(data[,"silte"])
data[,"areia"] <- as.numeric(data[,"areia"])
data[,"Upper.Boundary"] <- as.numeric(data[,"Upper.Boundary"])
data[,"Lower.Boundary"] <- as.numeric(data[,"Lower.Boundary"])

# Filtering samples with available soil layer depth information:
data <- data %>% filter(!is.na(Upper.Boundary), !is.na(Lower.Boundary))

# Filtering samples with soil layer thickness greater than zero:
data <- data %>% 
  mutate(thickness = Lower.Boundary - Upper.Boundary) %>%
  filter(!thickness <= 0)

# Removing Technosols and Anthroposols samples (from FEBR scripts):
data <- data %>% filter(!dataset_id %in% c("ctb0036", "ctb0599", "ctb0018"))

# Separating data into three dataframes for granulometry particle sizes:
argila_data <- data %>% select(c(Soil.ID, contains("Boundary"), argila))
silte_data <- data %>% select(c(Soil.ID, contains("Boundary"), silte))
areia_data <- data %>% select(c(Soil.ID, contains("Boundary"), areia))

# Harmonizing soil profiles -----------------------------------------------

# Function to harmonize soil data in standard depths:
harm_by_mpsspline <- function(
    # Input args: data.frame and soil property:
    object = argila_data, soil_prop = "argila") {
  
  # mpspline function to harmonize:
  res <- mpspline2::mpspline(
    obj = object, var_name = soil_prop,
    # Standard soil depth and mass conservation rule:
    d = c(0, 30, 100), vlow = 0, vhigh = 1000)
  
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
argila_harmonized <- argila_harmonized %>% filter(!is.na(`000_030_cm`))

silte_harmonized <- harm_by_mpsspline(silte_data, "silte")
silte_harmonized <- silte_harmonized %>% filter(!is.na(`000_030_cm`))

areia_harmonized <- harm_by_mpsspline(areia_data, "areia")
areia_harmonized <- areia_harmonized %>% filter(!is.na(`000_030_cm`))

# Renaming dataframe vars for convenience:
argila_harmonized <- argila_harmonized %>% 
  rename_with(function(x) paste0("argila_", x), !"Soil.ID")

silte_harmonized <- silte_harmonized %>% 
  rename_with(function(x) paste0("silte_", x), !"Soil.ID")

areia_harmonized <- areia_harmonized %>% 
  rename_with(function(x) paste0("areia_", x), !"Soil.ID")

harmonized <- full_join(argila_harmonized, silte_harmonized, by = "Soil.ID") %>% 
  full_join(areia_harmonized, by = "Soil.ID")

remove(argila_data, argila_harmonized, silte_data, silte_harmonized,
       areia_data, areia_harmonized)

# Gathering all data into one main dataframe:
full_data <- right_join(
  # Getting soil data (geographical coordinates, mainly) to fusion with harmonized data:
  data[,1:26] %>% arrange(Soil.ID) %>% distinct(Soil.ID, .keep_all = TRUE),
  harmonized,
  by = "Soil.ID", keep = FALSE, multiple = "first", relationship = "one-to-one") %>% 
  select(-c(contains("..."), contains("030_100")))

# Crating numerical var ID to each soil location:
full_data <- tibble::rowid_to_column(full_data, "ID")

# Preprocessing granulometry data for modeling process --------------------

full_data <- full_data %>%
  mutate(ln_clay_sand = log(.$argila_000_030_cm/.$areia_000_030_cm),
         ln_silt_sand = log(.$silte_000_030_cm/.$areia_000_030_cm)) %>% 
  rename(soil_ID = Soil.ID)

full_data %>% select(contains(c("sand", "argila", "areia", "silte"))) %>% head(1)

full_data_predito <- full_data %>% 
  mutate(sand_content_pred = (1/(exp(ln_clay_sand)+exp(ln_silt_sand) + 1))*100,
         silt_content_pred = (exp(ln_silt_sand)/(exp(ln_clay_sand)+exp(ln_silt_sand) + 1))*100,
         clay_content_pred = (exp(ln_clay_sand)/(exp(ln_clay_sand)+exp(ln_silt_sand)+1))*100,
         total_pred = sand_content_pred + silt_content_pred + clay_content_pred) %>% 
  select(ID, areia_000_030_cm, argila_000_030_cm, silte_000_030_cm, contains("pred"))

# Spatial dataset ---------------------------------------------------------

# Casting full_data object into SpatVect dataset
spatial_points <- vect(full_data ,
                       geom = c("coord_x", "coord_y"), # Lon and Lat (in this order)
                       crs = "EPSG:4326",              # WGS84 (original from file)
                       keep = TRUE)

# Viewing it:
plot(spatial_points)

leaflet(spatial_points) %>%
  addTiles(group = "OSM (default)",
           options = providerTileOptions(maxZoom = 20)) %>% 
  #addEsriBasemapLayer(key = esriBasemapLayers$Imagery, group = "ESRI Imagery") %>% 
  addCircleMarkers(color = "#FF551188", radius = 2)
  
# Exporting data as ESRI shapefile to ingest into GEE:
writeVector(x = spatial_points,
            filename = "febr-superconjunto_processed_granulometry.shp",
            filetype = "ESRI Shapefile", overwrite = TRUE)
