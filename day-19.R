library(tidyverse)
library(tidycensus)
library(sf)
library(ggtext)
library(glue)
library(basemaps)
library(ggimage)
library(magick)
library(cropcircles)
?crop_circle
?geometry_area
############## 1. Import Data ####################################################

NYC_CRS = "EPSG:32118"

# Get NJ data from 2020 census
NJ_counties <- get_decennial(year = 2020,
                      geography = "county",
                      state = c("New Jersey"),
                      variables = c("H3_001N", "H3_003N"),
                      geometry = T,
                      sumfile = "dhc",
                      output="wide") %>%
  st_transform(NYC_CRS) %>%
  rename(total_housing = "H3_001N", 
         vacant = "H3_003N")

# Read NYC neighborhoods
nyc_neighborhoods <- st_read("data/Neighborhoods Boundries.geojson") %>%
  st_transform(crs = NYC_CRS)

# Manually enter the places
food_spots_matrix <- matrix(data = c(40.64190691623345, -74.00295993041696, "Hainan Chicken House",
                                     40.760697917518925, -73.98220580734326, "Urban hawker",
                                     40.737450025718324, -73.99114771713131, "Laut",
                                     40.71388934487965, -73.99102859832436, "Kopitiam",
                                     40.74038859494913, -73.94648330481917, "Lemak Kitchen"
                              ), ncol = 3, byrow = T)

############## 2. Process Data ##################################################


# Convert to sf object
food_spots <- data.frame(lat = food_spots_matrix[,1],
                   lon = food_spots_matrix[,2],
                   name = food_spots_matrix[,3]) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = NYC_CRS)

# Create normal df
food_img = as.data.frame(st_coordinates(food_spots))
food_img$img = c("data/hainan.jpg",
                 "data/hawker.jpg",
                 "data/kopitiam.jpg",
                 "data/laut.jpg",
                 "data/lemak.png")

food_img$img <- crop_circle(food_img$img)

############## 3. Create Map ##################################################


title_text = glue("Restaurants to grab <b><span style = 'color:#EF3340;'>Singaporean</span></b>* food from in NYC and Long Island")

p <- ggplot() + 
  xlim(291000, 315000) + 
  ylim(50000, 71000) + 
  geom_sf(data = NJ_counties, fill = "lightgrey", colour = "white") + 
  geom_sf(data = nyc_neighborhoods, fill = "lightgrey", colour = "white") + 
  geom_sf(data = food_spots, size = 3, colour = "#EF3340") + 
  labs(title = title_text) + 
  theme_void() + 
  theme(plot.title = element_markdown(size=14)) + 
  

  geom_segment(aes(x = 299749, xend = 299749 + 1000, y = 52773, yend = 52773 + 2000), colour = "#EF3340") + 
  geom_segment(aes(x = 299749 + 1000, xend = 299749 + 6000, y = 52773 + 2000, yend = 52773 + 2000), colour = "#EF3340") + 
  annotate("text", x = 299749 + 1000, y = 52773 + 3000, label = "Hainan\nChicken House", color = "#EF3340", size = 4, fontface = "bold", hjust = 0) + 
  geom_image(data = food_img[1,], aes(image = img, x = 299749 + 7000, y = 52773 + 3000), size = 0.1) + 

  geom_segment(aes(x = 300747, xend = 300747 - 1000, y = 63383, yend = 63383 + 2000), colour = "#EF3340") + 
  geom_segment(aes(x = 300747 - 1000, xend = 300747 - 5000, y = 63383 + 2000, yend = 63383 + 2000), colour = "#EF3340") + 
  annotate("text", x = 300747 - 2500, y = 63383 + 2500, label = "Laut", color = "#EF3340", size = 4, fontface = "bold", hjust = 0) + 
  geom_image(data = food_img[4,], aes(image = img, x = 300747 - 6000, y = 63383 + 3000), size = 0.1) + 
  
  geom_segment(aes(x = 301502, xend = 301502 + 1000, y = 65964, yend = 65964 + 2000), colour = "#EF3340") + 
  geom_segment(aes(x = 301502 + 1000, xend = 301502 + 5000, y = 65964 + 2000, yend = 65964 + 2000), colour = "#EF3340") + 
  annotate("text", x = 301502 + 1000, y = 65964 + 3000, label = "Urban\nHawker", color = "#EF3340", size = 4, fontface = "bold", hjust = 0) + 
  geom_image(data = food_img[2,], aes(image = img, x = 301502 + 6000, y = 65964 + 3000), size = 0.1) +  
  
  geom_segment(aes(x = 304520, xend = 304520 + 1000, y = 63710, yend = 63710 - 2000), colour = "#EF3340") + 
  geom_segment(aes(x = 304520 + 1000, xend = 304520 + 5000, y = 63710 - 2000, yend = 63710 - 2000), colour = "#EF3340") + 
  annotate("text", x = 304520 + 1000, y = 63710 - 1000, label = "Lemak\nKitchen", color = "#EF3340", size = 4, fontface = "bold", hjust = 0) + 
  geom_image(data = food_img[5,], aes(image = img, x = 304520 + 6000, y = 63710 - 1000), size = 0.1) +  
  
  geom_segment(aes(x = 300758, xend = 300758 - 1000, y = 60766, yend = 60766 - 2000), colour = "#EF3340") + 
  geom_segment(aes(x = 300758 - 1000, xend = 300758 - 5000, y = 58766, yend = 58766), colour = "#EF3340") + 
  annotate("text", x = 300758 - 3700, y = 58766 - 500, label = "Kopitiam", color = "#EF3340", size = 4, fontface = "bold", hjust = 0) + 
  geom_image(data = food_img[5,], aes(image = img, x = 300758 - 6000, y = 58766 - 1000), size = 0.1)
  
ggsave("output/day-17.jpg", p, bg = "white", width = 8, height = 8)
