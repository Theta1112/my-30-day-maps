library(tidyverse)
library(tidycensus)
library(sf)
library(ggtext)
library(glue)
library(data.table)
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
  st_transform(crs = NYC_CRS) %>%
  select(geometry)



############## 2. Process Data ##################################################

permits <- permits.raw %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  mutate(date = ymd(date),
         year = year(date)) %>%
  filter(year >= 2022) %>%
  st_transform(NYC_CRS) %>%
  filter(job.type == "NB") %>%
  arrange(date) %>%
  st_jitter(100)

bbox <- st_bbox(permits)

############## 3. Create Map ##################################################


title_text = glue("<span style = 'color:#3057E1;'><b>New Building Permits Issued</b>
                  in NYC since 2022</span>")

p <- ggplot() + 
  xlim(bbox[1], bbox[3]) + 
  ylim(bbox[2], bbox[4]) +
  geom_sf(data = nyc_neighborhoods, colour = "#CED8F7", fill = "#3057E1") + 
  geom_sf(data = permits, colour = "#CED8F7", size = 0.001) + 
  theme_void() + 
  labs(title = title_text, 
       caption = "Source: NYC Open Data") + 
  theme(panel.background = element_rect(fill = "#3057E1", colour = "#CED8F7"),
        panel.grid.major = element_line(colour="#CED8F7", size=0.5),
        plot.title = element_markdown(size=18))
  

ggsave("output/day-22.jpg", p, bg = "white", width = 8, height = 8)
