library(tidyverse)
library(tidycensus)
library(sf)
library(ggtext)
library(glue)
library(data.table)
library(biscale)
library(cowplot)

############## 1. Import Data ####################################################

NYC_CRS = "EPSG:32118"

# Read air pollution data and join 
# raw.permits <- fread("C:/Users/seank/Downloads/DOB_Permit_Issuance_20241128.csv") 
# write.csv(permits, "data/permits_issued.csv", row.names = F)
# permits <- raw.permits %>%
#   rename(job.type = "Job Type") %>%
#   filter(job.type %in% c("NB", "DM") & !is.na(LATITUDE) & !is.na(LONGITUDE) & 
#            LATITUDE != "" & LONGITUDE != "") %>%
#   mutate(date = mdy(`Issuance Date`),
#          year = year(date)) %>%
#   filter(!is.na(date)) %>%
#   filter(year > 2019) %>%
#   select(job.type, LATITUDE, LONGITUDE, date)

permits.raw <- read.csv("data/permits_issued.csv")

nyc_neighborhoods <- st_read("data/Neighborhoods Boundries.geojson") %>%
  st_transform(crs = NYC_CRS)

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
         vacant = "H3_003N") %>%
  filter(!(GEOID %in% c(36005, 36047, 36061, 36085, 36081)))

############## 2. Process Data ##################################################

permits <- permits.raw %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  mutate(date = ymd(date),
         year = year(date)) %>%
  filter(year == 2023) %>%
  st_transform(NYC_CRS) %>%
  arrange(date) %>%
  st_jitter(100)

b <- permits %>%
  filter(job.type == "NB") %>%
  st_join(nyc_neighborhoods, join=st_within) %>%
  st_drop_geometry() %>%
  group_by(ntacode) %>%
  summarize(count = n()) %>%
  ungroup()

d <- permits %>%
  filter(job.type == "DM") %>%
  st_join(nyc_neighborhoods, join=st_within) %>%
  st_drop_geometry() %>%
  group_by(ntacode) %>%
  summarize(count = n()) %>%
  ungroup()

data.neigh <- nyc_neighborhoods %>%
  left_join(b %>% rename(nb = "count") ) %>%
  left_join(d %>% rename(dm = "count") ) %>%
  mutate(nb = ifelse(is.na(nb), 0, nb),
         dm = ifelse(is.na(dm), 0, dm))


data.bi <- bi_class(data.neigh, x = nb, y = dm, style = "quantile", dim = 3)

bbox <- st_bbox(data.bi)
############## 3. Create Map ##################################################


title_text = glue("<span style = 'color:#00008B;'><b>Constructions</b></span> vs 
                  <span style = 'color:#AA4A44;'><b>Demolitions</b></span>
                  in NYC 2023")

map <- ggplot() + 
  xlim(bbox[1], bbox[3]) + 
  ylim(bbox[2], bbox[4]) +
  geom_sf(data = data.bi, colour = "white", aes(fill = bi_class), linewidth = 0.1, show.legend = F) + 
  bi_scale_fill(pal = "DkViolet", dim = 3, flip_axes = T, rotate_pal = FALSE) + 
  geom_sf(data = NJ_counties, colour = "white", fill = "lightgrey") + 
  theme_void() + 
  labs(title = title_text, 
       caption = "Source: NYC Open Data") + 
  theme(plot.title = element_markdown(size=18))
  
legend <- bi_legend(pal = "DkViolet",   
                    flip_axes = FALSE,
                    rotate_pal = FALSE,
                    dim = 3,
                    xlab = "Demolition",
                    ylab = "Construction",
                    size = 9)


# Combine map and legend using cowplot
p<- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.8, 0.05, 0.2, 0.2)


ggsave("output/day-21.png", p, bg = "white", width = 8, height = 8)
