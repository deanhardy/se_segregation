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
# atl_sp <- as(atl, 'Spatial')
# label = "atl_urban"
# nhd <- get_nhd(atl_sp, label, raw.dir = "./data/nhd",
#                extraction.dir = paste0("./data/nhd/extractions/", label, "/nhd"))

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

## calculate area & percent of each huc in ATL urban area
int <- as.tibble(st_intersection(atl, huc))

atl_huc <- int %>%
  mutate(SqKmATLinHUC = as.numeric((st_area(geometry) / 1e6))) %>% 
  mutate(PercATLinHUC = (SqKmATLinHUC/AreaSqKm)) %>%
  select(Name, SqKmATLinHUC, PercATLinHUC) %>%
  left_join(huc, ., by = 'Name')

## ancillary data for mapping reference
## grab roads for cartographic purposes
rd <- primary_roads(year = 2016)

## plot yea things
fig <-   
  tm_shape(atl_huc) + 
    tm_borders(col = "black") + 
  tm_shape(atl) + 
    tm_fill('NAME10', palette = 'grey90',
            title = 'Urban area') +
  tm_shape(rd) +
    tm_lines(col = "black") +
  tm_shape(atl_huc) + 
    tm_borders(col = "grey50") + 
  tm_shape(arc) +
    tm_borders(lwd = 2) +
  # tm_text('NAME') +
  tm_shape(filter(atl_huc, PercATLinHUC >= 0.3)) + 
    tm_borders(col = "red") + 
    tm_text('Name', size = 0.4, col = "black") + 
  tm_shape(filter(atl_huc, PercATLinHUC >= 0.5)) + 
    tm_borders(col = "blue") + 
  tm_compass(type = "arrow", size = 2, position = c(0.74, 0.1)) +
  tm_scale_bar(breaks = c(0,20), size = 1, position= c(0.7, 0.0)) +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = 0.8,
            legend.title.size = 1)

fig

tiff('figures/atl_urban_huc10s.tif', res = 300, compression = 'lzw',
     height = 8, width = 6, units = 'in')
fig
dev.off()

