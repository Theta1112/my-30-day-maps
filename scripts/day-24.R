library(tidyverse)
library(tidycensus)
library(sf)
library(ggtext)
library(glue)

############## 1. Import Data ####################################################

NYC_CRS = "EPSG:32118"

# Read UHF42 data
UHF42 <- st_read("data/UHF42.geojson") %>%
  st_transform(NYC_CRS)

# Read air pollution data and join 
raw.air.pollution <- read.csv("data/Air_Quality_20241128.csv") 

ggplot() + 
  geom_sf(data = air.pollution, aes(fill = value))



# Get NJ data from 2020 census
NJ_counties <- get_decennial(year = 2020,
                      geography = "county",
                      state = c("New Jersey", "New York"),
                      variables = c("H3_001N", "H3_003N"),
                      geometry = T,
                      sumfile = "dhc",
                      output="wide") %>%
  st_transform(NYC_CRS) %>%
  rename(total_housing = "H3_001N", 
         vacant = "H3_003N")


############## 2. Process Data ##################################################

# Create circles and scale them
UHF42$area <- as.numeric(st_area(UHF42)) / max(as.numeric(st_area(UHF42)))

scale.factor = 4500

UHF42.circles <- st_buffer(st_centroid(UHF42), sqrt(UHF42$area) * scale.factor)

# Join
air.pollution <- raw.air.pollution %>%
  filter(Geo.Type.Name == "UHF42") %>%
  filter(Name == "Nitrogen dioxide (NO2)" & Time.Period == "Annual Average 2022") %>%
  select(Geo.Join.ID, Data.Value) %>%
  rename(id = "Geo.Join.ID",
         value = "Data.Value") %>%
  mutate(id = as.character(id)) %>%
  right_join(UHF42.circles) %>%
  filter(!is.na(value)) %>%
  st_sf()

############## 3. Create Map ##################################################


title_text = glue("<b><span style = 'color:#b30000;'>Nitrogen Dioxide (NO2)</span></b> <br>
                  <span style = 'color:#fc8d59;'>Concentration in the air<br>in NYC</span>")

ap.range <- quantile(air.pollution$value, c(0.25, 0.5, 0.75))

air.pollution <- air.pollution %>%
  mutate(value.cat = factor(case_when(
    value < 14.5 ~ "<14.5",
    value < 15.7 ~ "14.5 - 15.7",
    value < 17.4 ~ "15.7 - 17.4",
    value >= 17.4 ~ ">17.4"
  ), levels = c("<14.5", "14.5 - 15.7", "15.7 - 17.4", ">17.4")))



palette <- c(
  "#fdcc8a",
  "#fc8d59",
  "#e34a33",
  "#b30000")

p <- ggplot() + 
  xlim(280821.11, 325024.01) + 
  ylim(38063.51, 82315.86) + 
  geom_richtext(aes(x = 282000, y = 77000), label = title_text, 
                color = "white", size = 7, fontface = "bold", hjust = 0) +
  annotate("text", x = 282000, y = 67000, 
           label = "Parts per Billion", color = "black", size = 5, hjust = 0) + 
  geom_sf(data = air.pollution, aes(fill = value.cat)) + 
  scale_fill_manual(values = palette) + 
  theme_void() + 
  labs(fill = "", caption = "Source: NYC Open Data") + 
  theme(legend.position = c(0.19, 0.55))

ggsave("output/day-24.jpg", p, bg = "white", width = 8, height = 8)
