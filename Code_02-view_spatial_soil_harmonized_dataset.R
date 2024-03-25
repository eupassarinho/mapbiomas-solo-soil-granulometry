
# Required modules --------------------------------------------------------

require(terra)       # For handling geospatial data
require(dplyr)       # For data wrangling
require(leaflet)     # For geospatial data viz
require(leaflet.esri)# For geospatial data viz
require(here)

# Import data -------------------------------------------------------------
# "febr-superconjunto_processed_granulometry.shp" data path (in the local machine):

spatial_points <- vect(
  paste0(here(),"/project_products/01-febr_superconjunto-processed_granulometry.shp"))

# Spatial dataset ---------------------------------------------------------

# Viewing it:
plot(spatial_points)

leaflet(spatial_points[c(424, 552, 716, 902, 1358)] # Filtering by ID
        ) %>%
  addTiles(group = "OSM (default)",
           options = providerTileOptions(maxZoom = 20)) %>% 
  #addEsriBasemapLayer(key = esriBasemapLayers$Imagery, group = "ESRI Imagery") %>% 
  addCircleMarkers(color = "#FF551188", radius = 2)

data %>% 
  filter(Soil.ID %in% c("E-48", "E.064", "E.258", "GWDSM-5", "Perfil-001")) %>% 
  #pull(coord_x, coord_y)
  select(Soil.ID, coord_x, coord_y)
