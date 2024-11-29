library(tidyverse)
library(tidycensus)
library(sf)
library(ggtext)
library(glue)
library(basemaps)

############## 1. Import Data ####################################################

data <- read_csv("data/2018_Central_Park_Squirrel_Census_-_Squirrel_Data_20241107.csv") %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
  st_transform("EPSG:3857") %>% 
  mutate(color = "squirrel")

############## 2. Process Data ##################################################

# Create hexagon raster grid
hc_grid = st_make_grid(data,
                       c(100, 100),
                       what = "polygons",
                       square = F) 

hc_grid_sf <- hc_grid %>%
  st_sf() %>%
  mutate(grid_id = 1:length(lengths(hc_grid)))


# Convert point data into raster grid
rasterize <- function(data, hc){
  return(data %>%
           st_join(hc, join=st_within) %>%
           st_drop_geometry() %>%
           group_by(color, grid_id) %>%
           summarize(count = n()) %>%
           spread(color, count, fill=0) %>%
           ungroup()) 
}

# Join into grid and crop using hex crop
squirrel_hc <- left_join(hc_grid_sf, rasterize(data, hc_grid_sf), by = "grid_id") %>%
  filter(!is.na(squirrel))

squirrel_quantiles <- quantile(squirrel_hc$squirrel, c(0.2, 0.4, 0.6, 0.8), na.rm = T)

palette = c(
  "#feedde",
  "#fdbe85",
  "#fd8d3c",
  "#e6550d",
  "#a63603"
)

# Map data onto 4 regions based on quartiles
squirrel_hc <- squirrel_hc %>%
  mutate(squirrel_cat = case_when(
    squirrel < squirrel_quantiles[1] ~ palette[1],
    squirrel < squirrel_quantiles[2] ~ palette[2],
    squirrel < squirrel_quantiles[3] ~ palette[3],
    squirrel < squirrel_quantiles[4] ~ palette[4],
    squirrel >= squirrel_quantiles[4] ~ palette[5],
    T ~ NA
  ))

############## 3. Create Map ##################################################

# Create basemap object
# Insert your own mapbox api token to get base map
bm <- basemap_ggplot(ext = st_buffer(st_union(squirrel_hc), 1),
                     map_res = 1,
                     map_service = "mapbox", 
                     map_type = "streets", 
                     map_token = 'pk.eyJ1Ijoic2Vhbm1rb2giLCJhIjoiY20weGIyMmxzMDMzazJzb2RzMXFnbTk0dCJ9.7kSalMKf-WpQ5wpeDUISoA')

title_text = glue("The Central Park Squirrel Census 2018")

squirrel_text = glue("Squirrels in\nCentral Park\nlive on an island,\n isolated from the\nrest of nature.")
squirrel_text_density = glue("Squirrel Density")

p <- bm + 
  geom_sf(data = squirrel_hc, aes(fill = squirrel_cat)) + 
  geom_segment(aes(x = -8235595, xend = -8235595 + 1500, y = 4982449, yend = 4982449), color = "#a63603", size = 1) + 
  geom_segment(aes(x = -8234095, xend = -8234095 + 750, y = 4982449, yend = 4982449 - 750), color = "#a63603", size = 1) + 
  annotate("text", x = -8235395 - 200, y = 4978449 + 4300, label = squirrel_text_density, color = "#a63603", size = 7, fontface = "bold", hjust = 0) + 
  annotate("text", x = -8235395 + 3300, y = 4978449 + 600, label = squirrel_text, color = "#a63603", size = 7, hjust = 1) + 
  # annotate("text", x = -8235395 + 2200, y = 4978449 + 1250, label = squirrel_text_2, color = "#a63603", size = 4, hjust = 0) + 
  # annotate("text", x = -8235395 + 1950, y = 4978449 + 900, label = squirrel_text_3, color = "#a63603", size = 4, hjust = 0) + 
  # annotate("text", x = -8235395 + 1700, y = 4978449 + 550, label = squirrel_text_4, color = "#a63603", size = 4, hjust = 0) + 
  # annotate("text", x = -8235395 + 1700, y = 4978449 + 200, label = squirrel_text_5, color = "#a63603", size = 4, hjust = 0) + 
  # annotate("text", x = -8235395 + 1700, y = 4978449 - 150, label = squirrel_text_6, color = "#a63603", size = 4, hjust = 0) + 
  scale_fill_identity() + 
  
  labs(title = title_text,
       subtitle = "Basemap from Mapbox Streets") + 
  theme_void() + 
  theme(plot.title = element_markdown(hjust = 0, face = "bold", size = 17),
        panel.background = element_rect(color = NA))

p

ggsave("output/day-6.jpg", p, bg = "white", width = 8, height = 8)
