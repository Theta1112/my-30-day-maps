
library(tidyverse)
library(tidycensus)
library(sf)
library(ggtext)
library(glue)

# Load variables for EDA
NYC_CRS = "EPSG:32118"

nyc_neighborhoods <- st_read("data/Neighborhoods Boundries.geojson") %>%
  st_transform(crs = NYC_CRS) %>%
  select(geometry, ntacode)

housing.data <- read.csv("data/Affordable_Housing_Production_by_Building_20241130.csv") %>%
  drop_na(Longitude, Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = NYC_CRS)

ggplot() + 
  geom_sf(data = housing.data)
# Get nyc bounding box
bbox <- st_bbox(nyc_only)

housing.sf <- housing.data %>%
  mutate(Legend = "housing") %>%
  st_join(nyc_neighborhoods, join=st_within) %>%
  st_drop_geometry() %>%
  group_by(ntacode, Legend) %>%
  summarize(total_units = sum(All.Counted.Units)) %>%
  spread(Legend, total_units, fill=0) %>%
  right_join(nyc_neighborhoods) %>%
  st_sf()

housing.filtered <- housing.sf %>%
  filter(!(is.na(housing) | housing < 250))

sum(housing.data$All.Counted.Units)

title_text = glue("NYC Neighborhoods with <b>more than 250</b> affordable housing units.")
subtitle_text = glue("In the last decade, NYC has built over 259,000 affordable housing units.")

p<- ggplot() + 
  xlim(bbox[1], bbox[3]) + 
  ylim(bbox[2], bbox[4]) +
  # geom_sf(data = st_union(nyc_neighborhoods), colour = "lightgrey", fill = NA) + 
  geom_sf(data = housing.filtered, 
          fill = "white",
          # aes(fill = housing), 
          colour = "#003884") + 
  labs(title = title_text,
       subtitle = subtitle_text,
       caption = "Source: NYC Open Data") + 
  theme_void() + 
  theme(plot.title = element_markdown(hjust = 0, size = 18),
        plot.subtitle = element_markdown(hjust = 0, size = 13),
        panel.background = element_rect(fill = "#003884"))

ggsave("output/day-30.png", p, bg = "white", width = 8, height = 8)

