library(tidyverse)
library(tidycensus)
library(sf)
library(RColorBrewer)
library(ggtext)
library(glue)
library(basemaps)

############## 1. Import Data ####################################################

# Manually enter the places I went
tour_spots_matrix <- matrix(data = c(40.707406463924954, -74.00631801535344, # FP at sheraton
                              40.70603641856803, -74.00189306734869, # Pier 17
                              # 40.709546350044775, -74.00306842758559, # Southbridge Towers
                              40.7157314563436, -73.99691472477778, # Chinatown
                              40.7489015743702, -73.98527294981731, # Empire state
                              40.758713361336895, -73.98342255384354, # Times square
                              40.769278219149435, -73.97801951207236, # Central Park
                              40.761466370238786, -73.98509955009779, # Broadway
                              40.707406463924954, -74.00631801535344 # Back to Sheraton
                              ), ncol = 2, byrow = T)

############## 2. Process Data ##################################################


# Convert to sf object
tour <- data.frame(lat = tour_spots_matrix[,1],
                   lon = tour_spots_matrix[,2],
                   place = c("Hotel", "Pier 17", # "Southbridge Towers",
                             "Chinatown", "Empire State Building", "Times Square",
                             "Central Park", "Broadway", "Hotel"),
                   num = 1:8) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Convert to web mercator 3857 for use with base maps
tour_plot <- st_transform(tour,  crs = st_crs(3857))

# Get tourline
tourline_plot <- tour_plot %>%
  summarize(do_union=FALSE) %>%
  st_cast("LINESTRING") 

# Cut last point for plotting purposes
tour_plot <- tour_plot[1:7,]

############## 3. Create Map ##################################################

# Create basemap object
# Insert your own mapbox api token to get base map
bm <- basemap_ggplot(ext = st_buffer(tour_plot, 2000),
                     map_res = 1,
                     map_service = "mapbox", 
                     map_type = "streets", 
                     map_token = )


title_text = glue("My Tour of Manhattan, NYC")

p <- bm + 
  geom_sf(data = tourline_plot, colour = "black") + 
  geom_sf(data = tour_plot, size = 6, colour = "orange") + 
  geom_sf_text(data = tour_plot, aes(label = num)) + 
  labs(title = title_text,
       caption = "Basemap from Mapbox Streets") + 
  theme_void() + 
  theme(plot.title = element_markdown(hjust = 0, face = "bold", size = 17))

p

ggsave("output/day-5.jpg", p, bg = "white", width = 8, height = 8)
