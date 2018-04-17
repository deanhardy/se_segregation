rm(list=ls())

library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
library(tmap)
library(FedData)
library(sf)

atl <-urban_areas('2010') %>%
  filter(NAME10 == 'Atlanta, GA') %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")

## define region for census import
cnty <- c("Cherokee", "Clayton","Cobb", "DeKalb", "Douglas", 
          "Fayette", "Fulton", "Gwinnett", "Henry", "Rockdale")

## import spatial data for "cnty" region
arc <- counties('GA', 2010) %>% 
  filter(NAME %in% cnty)

## import huc data using FedData package
### not working, will download but not unzip and extract
atl_sp <- as(atl, 'Spatial')
label = "atl_urban"
nhd <- get_nhd(atl_sp, label, raw.dir = "./data/nhd",
               extraction.dir = paste0("./data/nhd/extractions/", label, "/nhd"))

## import watershed data based on manual extraction
nhd1 <- st_read("data/spatial/nhd/nhd0307_huc10.shp") 
nhd2 <- st_read("data/spatial/nhd/nhd0313_huc10.shp")
nhd3 <- st_read("data/spatial/nhd/nhd0315_huc10.shp")
nhd4 <- rbind(nhd1, nhd2)
nhd <- rbind(nhd4, nhd3) %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")

## returns all watersheds that intersect urban area via indexing
huc <- nhd[atl,]

tm_shape(huc) + 
  tm_borders() +
  tm_shape(atl) + 
  tm_polygons() +
  tm_shape(huc) + 
  tm_borders()
