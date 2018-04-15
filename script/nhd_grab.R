rm(list=ls())

library(tidyverse)
library(tigris)
library(tmap)
library(FedData)
library(sf)

atl <-urban_areas('2010') %>%
  st_as_sf() %>%
  filter(NAME10 == 'Atlanta, GA')

## import watershed data
## note that PERCENTAGE in 'huc' object equals that in ARC (from ArcGIS)
huc <- st_read("data/spatial/huc10.shp") %>%
  st_transform(4269)

tm_shape(atl) + 
  tm_polygons() + 
  tm_shape(huc) + 
  tm_borders() + 
  tm_shape(filter(huc, PERCENTAGE >= 50)) + 
  tm_borders(col = 'purple')
