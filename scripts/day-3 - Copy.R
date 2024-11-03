library(httr)
library(jsonlite)
library(stringr)
library(tidyverse)
library(tidycensus)
library(sf)

get_decennial(year = 2020,
              geography = "state")

?get_decennial

a <- load_variables(2020, "dhc")

t.df <- rbind(
  st_read(rawToChar(GET("https://data.cityofnewyork.us/resource/vfnx-vebw.geojson")$content)),
  st_read(rawToChar(GET("https://data.cityofnewyork.us/resource/vfnx-vebw.geojson?$offset=1000")$content)),
  st_read(rawToChar(GET("https://data.cityofnewyork.us/resource/vfnx-vebw.geojson?$offset=2000")$content)),
  st_read(rawToChar(GET("https://data.cityofnewyork.us/resource/vfnx-vebw.geojson?$offset=3000")$content)))


glimpse(t.df)

t <- st_read("data/NY_State_Plane_Coordinate_System_Zones_-5738512443882850290.geojson")

ggplot() + 
  geom_sf(data = t)
