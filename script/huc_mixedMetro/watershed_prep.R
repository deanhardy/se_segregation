##############################################################################
# watershed prep for HUC Mixed Metro
# created: July 11, 2023
# Author: Taylor Hafley, Dean Hardy
# git: deanhardy/se_segregation
# local location: Dropbox/school/Projects/inProgress/watershed/se_segregation/
##############################################################################
rm(list=ls())

# load necessary libraries
library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
library(sf)
library(tmap)
library(tmaptools)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/se_segregation/')

#########################################
##  HUC imports & fitting to ATL
#########################################

# Atlanta urban area
atl <-urban_areas(year = '2020') %>%
  filter(NAME10 == 'Atlanta, GA')
  # st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
  #              +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
  #              +units=m +no_defs")

## import watershed data based on manual extraction
nhd1 <- st_read(paste0(datadir, "data/spatial/nhd/nhd0307_huc10.shp"))
nhd2 <- st_read(paste0(datadir, "data/spatial/nhd/nhd0313_huc10.shp"))
nhd3 <- st_read(paste0(datadir, "data/spatial/nhd/nhd0315_huc10.shp"))
nhd4 <- rbind(nhd1, nhd2)
nhd <- rbind(nhd4, nhd3)
  # st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
  #              +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
  #              +units=m +no_defs")

rm(nhd1,nhd2,nhd3,nhd4)

## returns all watersheds that intersect urban area via indexing
huc10 <- nhd[atl,]
huc10 <-  huc10 %>%
  mutate(category = 'huc10') %>%
  select(Name, category, HUC10) %>%
  rename(HUC_NO = HUC10)

## HUC12s import ##

## import watershed data based on manual extraction
nhd1 <- st_read(paste0(datadir, "data/spatial/nhd/nhd0307_huc12.shp"))
nhd2 <- st_read(paste0(datadir, "data/spatial/nhd/nhd0313_huc12.shp"))
nhd3 <- st_read(paste0(datadir, "data/spatial/nhd/nhd0315_huc12.shp"))
nhd4 <- rbind(nhd1, nhd2)
nhd <- rbind(nhd4, nhd3)
  # st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
  #              +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
  #              +units=m +no_defs")

rm(nhd1, nhd2, nhd3, nhd4)
## returns all watersheds contained by huc10s
huc12 <- st_join(huc10, nhd, join = st_contains)
huc12 <- nhd %>%
  filter(HUC12 %in% huc12$HUC12) %>%
  mutate(category = 'huc12') %>%
  select(Name, category, HUC12) %>%
  rename(HUC_NO = HUC12)
rm(nhd)

## wishful thinking
# huc14 <- st_read(paste0(datadir, "data/spatial/nhd/WBDHU14.shp"))

## import create 
srwa <- st_read(paste0(datadir, "data/spatial/srwa_watersheds.shp")) %>%
  mutate(Name = 'SRWA', HUC_NO = 'SRWA', category = 'local') %>%
  select(Name, category, HUC_NO, geometry)
wawa <- st_read(paste0(datadir, "data/spatial/wawa_watersheds.shp")) %>%
  mutate(Name = 'WAWA', HUC_NO = 'WAWA', category = 'local') %>%
  select(Name, category, HUC_NO, geometry)
# uflint <- filter(huc10, HUC_NO == '0313000501') %>%
#   mutate(Name = 'Upper Flint', category = 'local', HUC_NO = 'uFlint') %>%
#   select(Name, category, HUC_NO, geometry)
uflint <- filter(huc12, HUC_NO %in% c('031300050103', '031300050101', '031300050102', '031300050104')) %>%
  st_union() %>%
  st_as_sf() %>%
  mutate(Name = 'Upper Flint', category = 'local', HUC_NO = 'uFlint') %>%
  rename(geometry = x) %>%
  select(Name, category, HUC_NO)

## count number of HUC12s inside "local" community watersheds
nrow(huc12[srwa,])
nrow(huc12[wawa,])
nrow(huc12[uflint,])

df <- rbind(huc10, huc12, srwa, wawa, uflint)

st_write(df, paste0(datadir, 'data/spatial/watersheds.GEOJSON'), driver = 'GEOJSON', append = FALSE)
