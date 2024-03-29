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

## ancillary data for mapping reference
## grab roads for cartographic purposes
rd <- primary_roads(year = 2016)

tm_shape(atl) +
  tm_polygons(col = 'grey80') +
  tm_shape(rd) +
  tm_lines(col = 'black')

tmap_mode("plot")

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
huc10 <- nhd[atl,]

## calculate area & percent of each huc in ATL urban area
int <- as.tibble(st_intersection(atl, huc10))

atl_huc10 <- int %>%
  mutate(SqKmATLinHUC = as.numeric((st_area(geometry) / 1e6))) %>% 
  mutate(PercATLinHUC = (SqKmATLinHUC/AreaSqKm)) %>%
  select(Name, SqKmATLinHUC, PercATLinHUC) %>%
  left_join(huc10, ., by = 'Name')

#st_write(atl_huc10, "data/data_share/huc10_atlurban", driver = 'GeoJSON')

## plot yea things
fig <-   
  tm_shape(atl_huc10) + 
    tm_borders(col = "black") + 
  tm_shape(atl) + 
    tm_fill('NAME10', palette = 'grey90',
            title = 'Urban area') +
  tm_shape(rd) +
    tm_lines(col = "black") +
  tm_shape(atl_huc10) + 
    tm_borders(col = "grey50") + 
  tm_shape(arc) +
    tm_borders(lwd = 2) +
  # tm_text('NAME') +
  tm_shape(filter(atl_huc10, PercATLinHUC >= 0.3)) + 
    tm_borders(col = "red") + 
    tm_text('Name', size = 0.4, col = "black") + 
  tm_shape(filter(atl_huc10, PercATLinHUC >= 0.5)) + 
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


#####################
## HUC12 processing
#####################

## import watershed data based on manual extraction
nhd1 <- st_read("data/spatial/nhd/nhd0307_huc12.shp") 
nhd2 <- st_read("data/spatial/nhd/nhd0313_huc12.shp")
nhd3 <- st_read("data/spatial/nhd/nhd0315_huc12.shp")
nhd4 <- rbind(nhd1, nhd2)
nhd <- rbind(nhd4, nhd3) %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")

## returns all watersheds contained by huc10s
huc12 <- st_join(huc10, nhd, join = st_contains)
huc12 <- nhd %>%
  filter(HUC12 %in% huc12$HUC12)

## calculate area & percent of each huc in ATL urban area
int <- as.tibble(st_intersection(atl, huc12))

atl_huc12 <- int %>%
  mutate(SqKmATLinHUC = as.numeric((st_area(geometry) / 1e6))) %>% 
  mutate(PercATLinHUC = (SqKmATLinHUC/AreaSqKm)) %>%
  select(Name, SqKmATLinHUC, PercATLinHUC) %>%
  left_join(huc12, ., by = 'Name') %>%
  st_transform(4269)

st_write(atl_huc12, "data/data_share/huc12_atlurban", driver = 'GeoJSON')

## plot yea things
fig12 <-   
  tm_shape(atl_huc10) + 
  tm_borders(col = "black") + 
  tm_shape(atl) + 
  tm_fill('NAME10', palette = 'grey90',
          title = 'Urban area') +
  tm_shape(rd) +
  tm_lines(col = "black") +
  tm_shape(atl_huc12) + 
  tm_borders(col = "grey50") + 
  tm_shape(arc) +
  tm_borders(lwd = 2) +
  # tm_text('NAME') +
  tm_shape(filter(atl_huc12, PercATLinHUC >= 0.3)) + 
  tm_borders(col = "red") + 
  # tm_text('Name', size = 0.4, col = "black") + 
  tm_shape(filter(atl_huc12, PercATLinHUC >= 0.5)) + 
  tm_borders(col = "blue") + 
  tm_shape(atl_huc10) + 
  tm_borders(col = "black") + 
  tm_compass(type = "arrow", size = 2, position = c(0.74, 0.1)) +
  tm_scale_bar(breaks = c(0,20), size = 1, position= c(0.7, 0.0)) +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = 0.8,
            legend.title.size = 1)
fig12

tiff('figures/atl_urban_huc12s.tif', res = 300, compression = 'lzw',
     height = 8, width = 6, units = 'in')
fig12
dev.off()



#################################################################
## "HUC14" processing; source of data is NHDPlus HR "catchments"
#################################################################

## import watershed data based on manual extraction
nhd1 <- st_read("data/spatial/nhd/nhd0307_huc14.shp") 
nhd2 <- st_read("data/spatial/nhd/nhd0313_huc14.shp")
nhd3 <- st_read("data/spatial/nhd/nhd0315_huc14.shp")
nhd4 <- rbind(nhd1, nhd2)
nhd <- rbind(nhd4, nhd3) %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")

## returns all watersheds contained by huc10s
huc14 <- st_join(huc10, nhd, join = st_contains)

## calculate area & percent of each huc in ATL urban area
int <- as.tibble(st_intersection(atl, huc14))

atl_huc14 <- int %>%
  mutate(SqKmATLinHUC = as.numeric((st_area(geometry) / 1e6))) %>% 
  mutate(PercATLinHUC = (SqKmATLinHUC/AreaSqKm)) %>%
  select(Name, SqKmATLinHUC, PercATLinHUC) %>%
  left_join(huc14, ., by = 'Name')

st_write(atl_huc14, "data/data_share/huc14_atlurban", driver = 'GeoJSON', delete_dsn = TRUE)
