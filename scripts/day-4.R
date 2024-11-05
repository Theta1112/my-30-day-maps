library(tidyverse)
library(tidycensus)
library(sf)
library(RColorBrewer)
library(ggtext)
library(glue)

############## 1. Import Data ####################################################

NYC_CRS = "EPSG:32118"

# Get data on brooklyn
brooklyn <- get_decennial(year = 2020,
                      geography = "county",
                      state = c("New York"),
                      county = c(047),
                      variables = c("H3_001N", "H3_003N"),
                      geometry = T,
                      sumfile = "dhc",
                      output="wide") %>%
  st_transform("EPSG:32118") %>%
  rename(total_housing = "H3_001N", 
         vacant = "H3_003N")

# Read pothole data
potholes <- st_read("data/potholes.geojson") %>%
  st_transform(NYC_CRS)

# Make hexgrid
hc_grid = st_make_grid(neighborhoods,
                       c(1000, 1000),
                       what = "polygons",
                       square = F)

# Make big hexagon for cropping
hex_crop = st_make_grid(brooklyn,
                        c(18000, 18000),
                        offset = c(-1000, 1000),
                        what = "polygons",
                        square = F) %>%
  st_sf() %>%
  st_transform(crs = NYC_CRS) %>%
  mutate(temp_id = 1:4) %>%
  filter(temp_id == 2)

# To sf and add grid ID
hc_grid_sf = st_sf(hc_grid) %>%
  st_transform(crs = NYC_CRS) %>% 
  mutate(grid_id = 1:length(lengths(hc_grid))) %>%
  rename(geometry = "hc_grid")


############## 2. Process Data ##################################################

# Convert point data into raster grid
rasterize <- function(data, hc = hc_grid_sf){
  return(data %>%
           st_join(hc, join=st_within) %>%
           st_drop_geometry() %>%
           group_by(grid_id, Legend) %>%
           summarize(count = n()) %>%
           spread(Legend, count, fill=0) %>%
           ungroup()) 
}

# Join into grid and crop using hex crop
pothole_hc <- left_join(hc_grid_sf, rasterize(potholes), by = "grid_id")

pothole_hc <- pothole_hc %>%
  st_centroid() %>%
  st_join(hex_crop) %>%
  filter(!is.na(temp_id)) %>%
  st_drop_geometry() %>%
  select(grid_id) %>%
  left_join(pothole_hc) %>%
  st_sf() %>% 
  st_intersection(hex_crop)


pothole_quantiles <- quantile(pothole_hc$Potholes, c(0.25, 0.5, 0.75), na.rm = T)

# Map data onto 4 regions based on quartiles
pothole_hc <- pothole_hc %>%
  mutate(pothole_cat = case_when(
    Potholes < pothole_quantiles[1] ~ "Bottom Quarter",
    Potholes < pothole_quantiles[2] ~ "Lower-Mid Quarter",
    Potholes < pothole_quantiles[3] ~ "Upper-Mid Quarter",
    Potholes >= pothole_quantiles[3] ~ "Top Quarter",
    T ~ NA
         ))

# Get border
hex_crop_border <- hex_crop %>%
  sf::st_cast("LINESTRING") 

# Get only hexgrids that do not intersect with crop border AND are not NA
pothole_hc_plot <- pothole_hc[!(st_intersects(pothole_hc, hex_crop_border) %>% lengths > 0) & !(is.na(pothole_hc$Potholes)),]


############## 3. Create Map ##################################################

title_text = glue("Pothole Hotspots in Brooklyn")

palette = brewer.pal(4, "BuPu")

p <- ggplot() + 
  geom_sf(data = hex_crop, fill = "grey", colour = "white", size = 10) + 
  geom_sf(data = pothole_hc_plot, aes(fill = pothole_cat), colour = "white", show.legend = FALSE) + 
  
  geom_segment(aes(x = 308800, xend = 310000, y = 60000, yend = 62000), color = palette[4], size = 1.2) + 
  geom_segment(aes(x = 310000, xend = 315000, y = 62000, yend = 62000), color = palette[4], size = 1.2) + 
  annotate("text", x = 310000, y = 63000, label = "Top Quartile", color = palette[4], size = 4, fontface = "bold", hjust = 0) + 
  
  geom_segment(aes(x = 300900, xend = 299600, y = 60000, yend = 62000), color = palette[2], size = 1.2) + 
  geom_segment(aes(x = 299600, xend = 294600, y = 62000, yend = 62000), color = palette[2], size = 1.2) +
  annotate("text", x = 294600, y = 63000, label = "Upper-Mid Quartile", color = "#87CEEB", size = 4, fontface = "bold", hjust = 0) + 
  
  geom_segment(aes(x = 301300, xend = 299600, y = 45200, yend = 43000), color = palette[3], size = 1.2) + 
  geom_segment(aes(x = 299600, xend = 294600, y = 43000, yend = 43000), color = palette[3], size = 1.2) + 
  annotate("text", x = 294600, y = 42000, label = "Lower-Mid Quartile", color = palette[3], size = 4, fontface = "bold", hjust = 0) + 
  
  geom_segment(aes(x = 306000, xend = 310000, y = 48400, yend = 43000), color = "black", size = 1.2) + 
  geom_segment(aes(x = 310000, xend = 315000, y = 43000, yend = 43000), color = "black", size = 1.2) + 
  annotate("text", x = 310000, y = 42000, label = "Bottom Quartile", color = "black", size = 4, fontface = "bold", hjust = 0) + 

  scale_fill_manual(values = palette) + 
  labs(title = title_text,
       caption = "Data from NYC Open Data, January 2024") + 
  theme_void() + 
  theme(plot.title = element_markdown(hjust = 0, face = "bold", size = 17))

p

ggsave("output/day-4.jpg", p, bg = "white", width = 8, height = 8)
