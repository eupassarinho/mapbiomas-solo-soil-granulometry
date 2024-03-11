
# Required modules --------------------------------------------------------

require(terra)       # For handling geospatial data
require(dplyr)       # For data wrangling
require(mpspline2)   # For spline soil data harmonizing
require(leaflet)     # For geospatial data viz
require(leaflet.esri)# For geospatial data viz
require(readr)
require(ggplot2)
require(hydroGOF)
require(patchwork)
require(ggpmisc)
require(dplyr)

# Loading raw data from FEBR ----------------------------------------------

# Loading FEBR superconjunto:
predito_matriz_final_v000 <- read_csv("predito_matriz_final_v000.csv")

predito_matriz_final_v000 <- predito_matriz_final_v000 %>% 
  rename(sand_soilgrids = sand, silt_soilgrids = silt, clay_soilgrids = clay) %>% 
  mutate(prediction_clay_0_30cm = prediction_clay_0_30cm*10,
         prediction_sand_0_30cm = prediction_sand_0_30cm*10,
         prediction_silt_0_30cm = prediction_silt_0_30cm*10)

source("./data_wrangling.R"); remove(data, full_data_predito, harmonized, original_data, harm_by_mpsspline, texture_scatterplot, spatial_points)

pred_vs_obs <- predito_matriz_final_v000 %>% full_join(full_data, by = "ID")

texture_scatterplot <- function(
    dataset = pred_vs_obs, aesthetic = aes(prediction_clay_0_30cm, argila_000_030_cm),
    observado = pred_vs_obs$argila_000_030_cm, predito = pred_vs_obs$prediction_clay_0_30cm) {
  
  df <- gof.data.frame(obs = observado, sim = predito, na.rm = T) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column() %>% 
    filter(rowname %in% c("ME", "MAE", "RMSE", "NSE", "R2")) %>% 
    rename(Stat = rowname, Value = V1)
  
  dataset %>% 
    ggplot(aesthetic)+
    geom_point(size = 3, alpha = 0.2)+
    geom_abline(intercept = 0, slope = 1, linewidth = 0.5)+
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 1, color = "#990000")+
    theme_bw()+
    coord_fixed(ratio = 1, xlim = c(0, 1000), ylim = c(0, 1000))+
    annotate(geom = "table", x = 0, y = 1000, label = list(df))
  
}


(texture_scatterplot() | texture_scatterplot(
  aesthetic = aes(prediction_sand_0_30cm, areia_000_030_cm),
  observado = pred_vs_obs$areia_000_030_cm, predito = pred_vs_obs$prediction_sand_0_30cm) |
  texture_scatterplot(
    aesthetic = aes(prediction_silt_0_30cm, silte_000_030_cm),
    observado = pred_vs_obs$silte_000_030_cm, predito = pred_vs_obs$prediction_silt_0_30cm))/
  (texture_scatterplot(
    aesthetic = aes(clay_soilgrids, argila_000_030_cm),
    observado = pred_vs_obs$prediction_clay_0_30cm, predito = pred_vs_obs$clay_soilgrids
    ) | texture_scatterplot(
    aesthetic = aes(sand_soilgrids, areia_000_030_cm),
    observado = pred_vs_obs$areia_000_030_cm, predito = pred_vs_obs$sand_soilgrids) |
     texture_scatterplot(
       aesthetic = aes(silt_soilgrids, silte_000_030_cm),
       observado = pred_vs_obs$silte_000_030_cm, predito = pred_vs_obs$silt_soilgrids))

ggsave("soil_texture_pred_vs_obs.png", width = 297, height = 210, units = "mm", dpi = 800)

