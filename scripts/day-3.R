
library(tidyverse)
library(tidycensus)
library(sf)
library(RColorBrewer)
library(ggtext)
library(glue)

# Load variables for EDA
# a <- load_variables(2020, "dhc")

# Get data from 2020 census
data <- get_decennial(year = 2020,
              geography = "county",
              state = c("New York", "New Jersey"),
              variables = c("H3_001N", "H3_003N"),
              geometry = T,
              sumfile = "dhc",
              output="wide") %>%
  st_transform("EPSG:32118") %>%
  rename(total_housing = "H3_001N", 
         vacant = "H3_003N")

# Get only nyc counties
nyc_only <- data %>%
  filter(GEOID %in% c(36005, 36047, 36061, 36085, 36081))

# Get nyc bounding box
nyc_bb <- st_bbox(nyc_only) %>%
  st_as_sfc()

# Filter everything into the nyc bounding box
data_filtered <- st_intersection(data, nyc_bb)

# Transform data to get vacancy rate and map onto 3 regions
data <- data %>%
  mutate(vr = vacant / total_housing,
         vr_cat = case_when(
           vr < 0.05 ~ "tight",
           vr < 0.1 ~ "mid",
           vr >= 0.1 ~ "high"
         ))

low_vac_text = glue("Low: < 5%")
mid_vac_text = glue("Mid: 5%-10%")
high_vac_text = glue("High: > 10%")
title_text = glue("Manhattan has the <span style = 'color:#F03B20;'>highest vacancy rates</span> in NYC")

p <- ggplot(data = data_filtered) + 
  xlim(278333.21, 325308.55) + 
  ylim(36611.74, 83403.70) + 
  geom_sf(data = nyc_bb, fill = "lightblue3", colour = "white", size = 0.3, show.legend = FALSE) + 
  geom_sf(aes(fill = vr_cat), colour = "black", size = 0.3, show.legend = FALSE) + 
  scale_fill_manual(values = rev(brewer.pal(3, "YlOrRd"))) + 
  guides(fill="none") + 
  annotate("text", x = 298611.74, y = 38503.70, label = low_vac_text, color = "#FEC20C", size = 4, fontface = "bold", hjust = 0) + 
  annotate("text", x = 306611.74, y = 38503.70, label = mid_vac_text, color = "orange", size = 4, fontface = "bold", hjust = 0) + 
  annotate("text", x = 314911.74, y = 38503.70, label = high_vac_text, color = brewer.pal(3, "YlOrRd")[3], size = 4, fontface = "bold", hjust = 0) + 
  labs(title = title_text,
    subtitle = "High home prices and rents have pushed people out of Manhattan and into the surrounding counties",
    caption = "Data from U.S. 2020 Decennial Census") + 
  theme_void() + 
  theme(plot.title = element_markdown(hjust = 0, face = "bold", size = 17))
p

ggsave("output/day-3.jpg", p, bg = "white", width = 8, height = 8)
