rm(list=ls())

##then packages are loaded into the R environment
library(tidyverse)
library(tidycensus)
library(viridis)
library(tigris)
library(tmap)
library(sf)
library(ggthemes)

# census_api_key("") ## Already installed

## downloads ACS data for 2011-2015
acs <- load_variables(yr, "acs5", cache = TRUE)

## ARC's 10 counties
# cnty <- c("Cherokee", "Clayton", "Cobb", "Dekalb", "Douglas", 
#            "Fayette", "Fulton", "Gwinnett", "Henry", "Rockdale")

## import counties that are contain HUC10s >50% in the ARC region, ie the "ARC watershed"
cnty <- read.csv("data/cnty.txt") %>%
  select(NAME10) %>%
  mutate(NAME10 = as.character(NAME10)) %>%
  unlist(use.names = FALSE)

## import race variables of interest
race_vars <- c(white = "B03002_003E", black = "B03002_004E", 
               native_american = "B03002_005E", asian = "B03002_006E", 
               hawaiian = "B03002_007E", other = "B03002_008E", 
               multiracial = "B03002_009E", latinx = "B03002_012E")

## import ACS data for ARC (ie region defined by "cnty")
arc <- get_acs(geography = "block group", 
              variables = race_vars,
              state = "GA", county = cnty, 
              year = yr)

## transform ACS census data for joining to spatial data
arc2 <- arc %>%
  select(-moe, -NAME) %>%
  spread(key = "variable", value = "estimate")

## import spatial data for "cnty" region
shp <- get_acs(geography = "block group", 
               variables = "B03002_001E",
               state = "GA", county = cnty, 
               year = yr, geometry = TRUE)
shp <- st_zm(shp) ## drop "Z" data

## plot to confirm extent
qtm(shp, fill = "estimate")

## append census race data to spatial data
arc.shp <- left_join(shp, arc2, by = "GEOID", copy = TRUE) %>%
  select(-moe, -variable, -NAME) %>%
  rename(B03002_001 = estimate) %>%
  mutate(nwnl_prc = 1-(B03002_003/B03002_001))
# arc.shp <- arc.shp %>% st_as_sf() 

## export spatial census race data
# st_write(arc.shp, "spatial/acs15_arc.shp", delete_dsn = TRUE)

## map ARC census data for NWNL people
map <- tm_shape(arc.shp) +
  tm_polygons("nwnl_prc",
              palette = "Purples",
              title = "Non-White,\nNon-Latinx\nPopulation (%)") +
  tm_compass(type = "arrow", size = 3, position = c(0.05, 0.1)) +
  tm_scale_bar(breaks = c(0,10), size = 0.8, position= c(0.06, 0.04)) +
  tm_legend(position = c(0.9,0.05)) + 
  tm_layout(main.title = "Greater Atlanta Region (2015)", main.title.position = "center", frame = FALSE)
map

## export as PNG file
# png("figures/nwnl_arc2015.png", width = 7.5, height = 7.5, units = "in", res = 300, bg = "white")
# map
# dev.off()

##import HUC10 data
# nhd <- get_nhd(shp, "arc_nhd") ## didn't work out using "FedData" package
huc <- st_read("data/spatial/huc10.shp")

## import DMR data downloaded from internet for HUC03 and filter to GA and make spatial
## https://echo.epa.gov/trends/loading-tool/get-data/custom-search
dmr <- read.csv("data/dmr_2015_huc03.csv") %>%
  filter(State == "GA") %>%
  st_as_sf(coords = c("Facility.Longitude", "Facility.Latitude"), crs = 4269)

poll <- dmr %>%
  group_by(NPDES.Permit.Number) %>%
  summarise(sum = sum(Pollutant.Load..kg.yr., na.rm = TRUE))

## transform projection of ARC & HUC spatial data to match DMR data
arc.shp <- st_transform(arc.shp, 4269)  
huc <- st_transform(huc, 4269)

huc.arc <- st_intersection(arc.shp, huc)
qtm(huc.arc, fill = "nwnl_prc")

## union ARC race data with DMR summed data
huc.poll <- st_intersection(huc, poll)

## map ARC race data with watersheds and DMR overlaid
dmr.map <- tm_shape(huc.arc) +
  tm_polygons("nwnl_prc",
              palette = "Greys",
              title = "Non-White,\nNon-Latinx\nPopulation (%)") +
  tm_shape(huc) + 
  tm_borders(col = "yellow") +
  tm_shape(huc.poll) + 
  tm_bubbles(size = "sum", col = "red", scale = 2, title.size = "Pollution (kg/yr)",
             size.lim = c(0,14e6), sizes.legend = c(2e6, 4e6, 6e6, 10e6, 12e6)) + 
  tm_compass(type = "arrow", size = 2, position = c(0.86, 0.06)) +
  tm_scale_bar(breaks = c(0,10), size = 0.8, position= c(0.85, 0.0)) +
  tm_legend(position = c(-0.15, 0)) + 
  tm_layout(main.title = "Atlanta's 'Watershed' (2015)", main.title.position = "center", 
            frame = FALSE)
dmr.map

tiff("figures/dmr_map.tif", res = 300, units = "in", height = 7.5, width = 7.5, compression = "lzw")
dmr.map
dev.off()


hp.sum <- huc.poll %>%
  group_by(Name) %>%
  summarise(sum = sum(sum/AreaSqKm)) %>%
  st_set_geometry(NULL)

hr.sum <- huc.arc %>%
  ungroup() %>%
  group_by(Name) %>%
  summarise(sum003 = sum(B03002_003), sum001 = sum(B03002_001)) %>%
  mutate(nwnl_prc = 1-(sum003/sum001)) %>%
  st_set_geometry(NULL)

hpr.sum <- merge(hp.sum, hr.sum, by = "Name")

colourCount =length(unique(hpr.sum$Name))

fig <- ggplot(hpr.sum, aes(nwnl_prc, sum)) +
  geom_point(aes(nwnl_prc, sum, col = Name)) + 
  geom_smooth(method = "loess", linetype = "dashed") +
  xlab("Non-white, Non-Latinx Population (%)") + 
  ylab("Pollution (kg/yr/km^2)") + 
  scale_color_manual(name = "HUC10 Watershed",
                     values = colorRampPalette(solarized_pal()(19))(colourCount))
fig

tiff("figures/")



#################################################################################
## to create animated gif after pngs are created, but from terminal, not console
#################################################################################
# library(magick)
# library(animation)
# 
# setwd("C:/Users/dhardy/Dropbox/sesync/manuscripts/in_prep/se_segregation/R/figures/")
# 
# im.convert('*.png', output = "animation.gif", convert = c("convert", "gm convert"),
#            cmd.fun = if (.Platform$OS.type == "windows") shell else system, extra.opts = "",
#            clean = FALSE)
