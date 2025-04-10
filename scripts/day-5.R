library(tidyverse)
library(tidycensus)
library(sf)
library(RColorBrewer)
library(ggtext)
library(glue)
library(basemaps)
library(ggimage)

############## 1. Import Data ####################################################

# Manually enter the places I went
tour_spots_matrix <- matrix(data = c(40.707406463924954, -74.00631801535344, # FP at sheraton
                              40.70603641856803, -74.00189306734869, # Pier 17
                              40.7157314563436, -73.99691472477778, # Chinatown
                              40.73594605797066, -74.00588211831324, # JJ plaque
                              40.74823224801664, -74.00476490407061, # highline
                              40.7489015743702, -73.98527294981731, # Empire state
                              40.758713361336895, -73.98342255384354, # Times square
                              40.76539706771235, -73.97483450281594, # Park Lane
                              40.761466370238786, -73.98509955009779, # Broadway
                              40.707406463924954, -74.00631801535344 # Back to Sheraton
                              ), ncol = 2, byrow = T)

############## 2. Process Data ##################################################


# Convert to sf object
tour <- data.frame(lat = tour_spots_matrix[,1],
                   lon = tour_spots_matrix[,2],
                   num = 1:length(tour_spots_matrix[,1])) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Convert to web mercator 3857 for use with base maps
tour_plot <- st_transform(tour,  crs = st_crs(3857))

# Get tourline
tourline_plot <- tour_plot %>%
  summarize(do_union=FALSE) %>%
  st_cast("LINESTRING") 

# Cut last point for plotting purposes
tour_plot <- tour_plot[1:(dim(tour_plot)[1]-1),]

# Create normal df
tour_img = as.data.frame(st_coordinates(tour_plot))
tour_img$img = c("data/hotel.jpg",
                 "data/pier.jpg",
                 "data/chinatown.jpg",
                 "data/janejacobs.jpg",
                 "data/highline.jpg",
                 "null",
                 "data/times_square.jpg",
                 "data/parklane.jpg",
                 "data/broadway.jpg")

############## 3. Create Map ##################################################

# Create basemap object
# Insert your own mapbox api token to get base map
bm <- basemap_ggplot(ext = st_buffer(tour_plot, 2000),
                     map_res = 1,
                     map_service = "mapbox", 
                     map_type = "dark", 
                     map_token = )

title_text = glue("My Tour of Manhattan, NYC")
tour_plot


p <- bm + 
  geom_sf(data = tourline_plot, colour = "orange3") + 
  geom_sf(data = tour_plot, size = 6, colour = "orange") + 
  geom_sf_text(data = tour_plot, aes(label = num)) + 
  
  geom_image(data = tour_img[2,], aes(image = img, x = X + 1000, y = Y - 700), size = 0.1) + 
  geom_image(data = tour_img[3,], aes(image = img, x = X + 1300, y = Y + 700), size = 0.1) + 
  geom_image(data = tour_img[4,], aes(image = img, x = X - 1000, y = Y - 100), size = 0.1) + 
  geom_image(data = tour_img[5,], aes(image = img, x = X - 1000, y = Y + 1000), size = 0.1) + 
  geom_image(data = tour_img[7,], aes(image = img, x = X + 1000, y = Y - 1000), size = 0.1) + 
  geom_image(data = tour_img[8,], aes(image = img, x = X + 1000, y = Y + 1000), size = 0.1) + 
  geom_image(data = tour_img[9,], aes(image = img, x = X - 1000, y = Y + 1000), size = 0.1) + 
  
  labs(title = title_text,
       subtitle = "Basemap from Mapbox Streets") + 
  theme_void() + 
  theme(plot.title = element_markdown(hjust = 0, face = "bold", size = 17),
        panel.background = element_rect(color = NA))

p

ggsave("output/day-5.jpg", p, bg = "white", width = 8, height = 8)
