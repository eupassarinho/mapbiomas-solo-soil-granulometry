# 2024-11-16

libraries <- c("readr", "dplyr", "ggplot2", "esquisse", "patchwork",
               "terra", "tidyterra", "geobr", "rworldmap", "ggtern")
lapply(libraries, require, character.only = T)
remove(libraries)

df <- read_csv("source_data/2024-11-12-clay_silt_sand.csv")
#
# Particle size distribution plots ----------------------------------------

hist_data <- hist(df$silt)
bin_edges <- hist_data$breaks

sand_plot <- df %>% 
  ggplot(aes(x = sand))+
  geom_histogram(colour = "black", fill = "gray", binwidth = 5, position = "identity",
                 boundary = 0, linewidth = 0.1)+
  labs(y = "Frequency", x = "Sand content (%)")+
  scale_x_continuous(breaks = seq(0, 100, 20))+
  scale_y_continuous(breaks = seq(0, 1500, 250), limits = c(0, 1500))+
  theme_classic()+
  guides(x = guide_axis(cap = "both"), # Cap both ends
         y = guide_axis(cap = "both")  # Cap the upper end
  )+
  theme(axis.text = element_text(family = "serif", size = 10, colour = "black"),
        axis.title = element_text(family = "serif", size = 10, colour = "black"),
        axis.ticks.length = unit(4, "pt"),
        axis.line = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3))

silt_plot <- df %>% 
  ggplot(aes(x = silt))+
  geom_histogram(colour = "black", fill = "gray", binwidth = 5, position = "identity",
                 boundary = 0, linewidth = 0.1)+
  labs(y = "Frequency", x = "Silt content (%)")+
  scale_x_continuous(breaks = c(seq(0, 95, 20), 95), limits = c(0, 95))+
  scale_y_continuous(breaks = seq(0, 4500, 750), limits = c(0, 4500))+
  theme_classic()+
  guides(x = guide_axis(cap = "both"), # Cap both ends
         y = guide_axis(cap = "both")  # Cap the upper end
  )+
  theme(axis.text = element_text(family = "serif", size = 10, colour = "black"),
        axis.title = element_text(family = "serif", size = 10, colour = "black"),
        axis.ticks.length = unit(4, "pt"),
        axis.line = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3))

clay_plot <- df %>% 
  ggplot(aes(x = clay))+
  geom_histogram(colour = "black", fill = "gray", binwidth = 5, position = "identity",
                 boundary = 0, linewidth = 0.1)+
  labs(y = "Frequency", x = "Clay content (%)")+
  scale_x_continuous(breaks = seq(0, 100, 20))+
  scale_y_continuous(breaks = seq(0, 2400, 300), limits = c(0, 2400))+
  theme_classic()+
  guides(x = guide_axis(cap = "both"), # Cap both ends
         y = guide_axis(cap = "both")  # Cap the upper end
  )+
  theme(axis.text = element_text(family = "serif", size = 10, colour = "black"),
        axis.title = element_text(family = "serif", size = 10, colour = "black"),
        axis.ticks.length = unit(4, "pt"),
        axis.line = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3))

(sand_plot | silt_plot | clay_plot)

ggsave("./project_products/2024-11-16-particle_size_distributions.png",
       (sand_plot | silt_plot | clay_plot), 
       width = 18, height = 10, units = "cm", dpi = 600)

# Map ---------------------------------------------------------------------

sampling_points <- vect(df,
                        geom = c("coord_x", "coord_y"), # Lon and Lat (in this order)
                        crs = "EPSG:4326",              # WGS84 (original from file)
                        keep = TRUE)   

brazil_boundaries <- read_country()
world <-  vect(getMap())

ggplot()+
  geom_sf(data = brazil_boundaries, colour = "black", fill = "#F9F9F9")+
  geom_spatvector(data = sampling_points, size = 1.5, alpha = 0.2)+
  theme_bw()+
  theme(axis.text = element_text(family = "serif", size = 10, colour = "black"),
        axis.title = element_text(family = "serif", size = 10, colour = "black"),
        axis.ticks.length = unit(4, "pt"),
        axis.line = element_line(linewidth = 0.3),
        axis.ticks = element_line(linewidth = 0.3))

ggsave("./project_products/2024-11-16-soil_granulometry_point_map.png",
       last_plot(), 
       width = 12, height = 12, units = "cm", dpi = 600)

# Soil texture description ------------------------------------------------

data(USDA)

ggtern(df, aes(x = sand, y = clay, z = silt))+
  theme_bw()+
  labs(x = "Sand (%)", y = "Clay (%)", z = "Silt (%)")+
  theme_showarrows()+
  theme_hidetitles()+
  theme_clockwise()+
  geom_point(data = df,
             aes(x = sand, y = clay, z = silt),
             size = 1, alpha = 0.3, linewidth = 1,
             shape = 21, fill = "#8c8c8c", colour = "#000000")+
  geom_polygon(
    data = USDA, aes(Sand, Clay, Silt, group = Label),
    fill = NA, linewidth = 0.3, alpha = 0.5, colour = "black")+
  theme(text = element_text(family = "serif", size = 10),
        axis.text = element_text(family = "serif", size = 10))

ggsave("./project_products/2024-11-17-soil_texture_plot.png",
       last_plot(), width = 12, height = 12, units = "cm", dpi = 600)

