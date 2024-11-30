library(tidyverse)
library(tidycensus)
library(sf)
library(ggtext)
library(glue)
library(data.table)
library(biscale)
library(cowplot)
library(patchwork)

############## 1. Import Data ####################################################

NYC_CRS = "EPSG:32118"
SG_CRS = "EPSG:3414"

nyc_neighborhoods <- st_read("data/Neighborhoods Boundries.geojson") %>%
  st_transform(crs = SG_CRS)

sg_neighbourhoods <- st_read("data/district_and_planning_area.geojson") %>%
  st_transform(crs = NYC_CRS)


############## 2. Process Data ##################################################

############## 3. Create Map ##################################################


title_text = glue("<span style = 'color:#003884;'><b>Singapore</b></span> and 
                  <span style = 'color:#EF3340;'><b>New York City</b></span>
                  <b>flip</b> Coordinate Reference Systems and Projections")


nyc_plot <- ggplot() + 
  geom_sf(data = nyc_neighborhoods, fill = "#EF3340", colour = "white") + 
  theme_void()

sg_plot <- ggplot() + 
  geom_sf(data = sg_neighbourhoods, fill = "#003884", colour = "white") + 
  theme_void()

layout <- c(
  area(t = 1, l = 1, b = 8, r = 9),
  area(t = 4, l = 6, b = 11, r = 12)
)

# Combine map and legend using cowplot
p <- nyc_plot + sg_plot + 
  plot_layout(design = layout) + 
  plot_annotation(title = title_text,
                  caption = "Areas not to scale",
                  theme = theme(plot.title = element_markdown(lineheight = 1.1)))

ggsave("output/day-26.png", p, bg = "white", width = 8, height = 8)
