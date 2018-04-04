rm(list=ls())

##then packages are loaded into the R environment
library(tidyverse)
library(tidycensus)
library(viridis)
library(tigris)
library(tmap)
library(sf)
library(ggthemes)
# library(dmm)

# census_api_key("") ## Already installed

yr <- "2015"

## import segregation data from 2011-2015 ACSs
seg <- st_read("data/geojson/arc15.geojson") %>%
  mutate(rd15c = as.factor(rd15c))

## import DMR data downloaded from internet for HUC03 and filter to GA and make spatial
## https://echo.epa.gov/trends/loading-tool/get-data/custom-search
dmr11 <- read.csv("data/dmr/dmr_2011_huc03.csv", skip = 4)
dmr12 <- read.csv("data/dmr/dmr_2012_huc03.csv", skip = 4)
dmr13 <- read.csv("data/dmr/dmr_2013_huc03.csv", skip = 4)
dmr14 <- read.csv("data/dmr/dmr_2014_huc03.csv", skip = 4)
dmr15 <- read.csv("data/dmr/dmr_2015_huc03.csv", skip = 4)

dmr <- rbind(dmr11, dmr12, dmr13, dmr14, dmr15) %>%
  filter(State == "GA") %>%
  st_as_sf(coords = c("Facility.Longitude", "Facility.Latitude"), crs = 4269)

## convert sso shapefile to csv for manual cleaning of ESTIMATED column in Excel
# st_read("data/sso/sso.shp") %>%
#   st_transform(., 4269) %>%
#   st_as_sf(., coords = c("lon", "lat"), crs = 4269) %>% 
#   cbind(., st_coordinates(.)) %>% 
#   st_set_geometry(NULL) %>% 
#   write_csv(., 'data/sso/sso.csv')

## import revised sso data
sso <- read.csv("data/sso/sso.csv") %>%
  filter(ESTIMATED != "NA") %>%
  st_as_sf(., coords = c("X", "Y"), crs = 4269)

##import HUC10 data
# nhd <- get_nhd(shp, "arc_nhd") ## didn't work out using "FedData" package
huc <- st_read("data/spatial/huc10.shp")

## downloads ACS data for 2011-2015
# acs <- load_variables(yr, "acs5", cache = TRUE)

## ARC's 10 counties
# cnty <- c("Cherokee", "Clayton", "Cobb", "Dekalb", "Douglas", 
#            "Fayette", "Fulton", "Gwinnett", "Henry", "Rockdale")

## import counties that are contain HUC10s >50% in the ARC region, ie the "ARC watershed"
cnty <- read.csv("data/cnty.txt") %>%
  mutate(NAME10 = as.character(NAME10)) %>%
  dplyr::select(NAME10) %>%
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
  dplyr::select(-moe, -NAME) %>%
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
  dplyr::select(-moe, -variable, -NAME) %>%
  rename(B03002_001 = estimate) %>%
  mutate(nwnl_prc = 1-(B03002_003/B03002_001))
# arc.shp <- arc.shp %>% st_as_sf() 

## export spatial census race data
# st_write(arc.shp, "spatial/acs15_arc.shp", delete_dsn = TRUE)

## map ARC census data for NWNL people
# map <- tm_shape(arc.shp) +
#   tm_polygons("nwnl_prc",
#               palette = "Purples",
#               title = "Non-White,\nNon-Latinx\nPopulation (%)") +
#   tm_compass(type = "arrow", size = 3, position = c(0.05, 0.1)) +
#   tm_scale_bar(breaks = c(0,10), size = 0.8, position= c(0.06, 0.04)) +
#   tm_legend(position = c(0.9,0.05)) + 
#   tm_layout(main.title = "Greater Atlanta Region (2010)", main.title.position = "center", frame = FALSE)
# map

## export as PNG file
# png("figures/nwnl_arc2015.png", width = 7.5, height = 7.5, units = "in", res = 300, bg = "white")
# map
# dev.off()

## extract pollution load data from dmr and summarize by facility
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

ha.test <- st_intersection(st_union(arc.shp), st_union(huc))
  
## map ARC race data with watersheds and DMR overlaid
dmr.map <- tm_shape(huc.arc) +
  tm_polygons("nwnl_prc",
              palette = "Greys",
              title = "Non-White /\nNon-Latinx\nPopulation (%)") +
  tm_shape(huc) + 
  tm_borders(col = "yellow") +
  tm_shape(huc.poll) + 
  tm_bubbles(size = "sum", col = "green", scale = 2, title.size = "Pollution (kg/yr)",
             size.lim = c(0,42e6), sizes.legend = c(2e6, 4e6, 6e6, 10e6, 20e6, 40e6)) + 
  tm_compass(type = "arrow", size = 2, position = c(0.86, 0.06)) +
  tm_scale_bar(breaks = c(0,10), size = 0.8, position= c(0.85, 0.0)) +
  tm_legend(position = c(-0.15, 0)) + 
  tm_layout(main.title = "Atlanta's 'Watershed' (2011-2015)", main.title.position = "center", 
            frame = FALSE)
dmr.map

tiff("figures/dmr_map2011-15.tif", res = 300, units = "in", height = 7.5, width = 7.5, compression = "lzw")
dmr.map
dev.off()

## sum pollution loads by HUC10 watershed, hence "hp"
hp.sum <- huc.poll %>%
  group_by(Name) %>%
  summarise(sum = sum(sum/AreaSqKm)) %>%
  st_set_geometry(NULL)

## percent nwnl by HUC10 watershed, hence "hr"; note that this approach doesn't account for BGs
## that were cut off, so no adjustment made for some BGs that crossover watershed boundary
hr.sum <- huc.arc %>%
  ungroup() %>%
  group_by(Name) %>%
  summarise(sum003 = sum(B03002_003), sum001 = sum(B03002_001)) %>%
  mutate(nwnl_prc = 1-(sum003/sum001)) %>%
  st_set_geometry(NULL)

## join pollution sums and percent race by watershed
hpr.sum <- merge(hp.sum, hr.sum, by = "Name")

## create spectrum of colors for plot
colourCount =length(unique(hpr.sum$Name))

library(RColorBrewer)
n <- 49
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
col = sample(col_vector, n)

##plot % nwnl by pollution as kg/yr/km&2
fig <- ggplot(hpr.sum, aes(nwnl_prc, sum/1000)) +
  geom_point(aes(nwnl_prc, sum/1000, col = Name), size = 3) + 
  geom_smooth(method = "auto", se = TRUE, linetype = "solid", level = 0.95) +
  scale_x_continuous(limits = c(0,1)) +
  xlab("Non-white / Non-Latinx Population (%)") + 
  ylab("Pollution (1,000 kg/yr/km^2)") + 
  scale_color_manual(name = "HUC10 Watershed",
                     values = col) +
  ggtitle("Pollution by Race (2011-2015)")
fig

tiff("figures/pollution_race_2011-15.tiff", res = 300, compression = "lzw", units = "in", 
     height = 5.5, width = 8)
fig
dev.off()

# col <- c("0" = "white", "2" = "orange", "3" = "green", "6" = "salmon", "7" = "purple", 
#          "8" = "light orange", "9" = "light green", "13" = "light purple", "14" = "brown")

col <- c("white", "orange", "green","pink", "purple", 
         "light orange", "yellow", "light purple", "brown", "black")
# col <= get_brewer_pal("Accent", n = 9)

## map ARC race data with watersheds and DMR overlaid
seg.map <- tm_shape(seg) +
  tm_polygons("rd15c",
              fill = col,
              title = "Segregation/\nDiversity") +
  tm_shape(huc) + 
  tm_borders(col = "black") +
  tm_shape(huc.poll) + 
  tm_bubbles(size = "sum", col = "black", scale = 2, title.size = "Pollution (kg/yr)",
             size.lim = c(0,42e6), sizes.legend = c(2e6, 4e6, 6e6, 10e6, 20e6, 40e6)) + 
  tm_compass(type = "arrow", size = 2, position = c(0.86, 0.06)) +
  tm_scale_bar(breaks = c(0,10), size = 0.8, position= c(0.85, 0.0)) +
  tm_legend(position = c(-0.15, 0)) + 
  tm_layout(main.title = "Atlanta's 'Watershed' (2011-2015)", main.title.position = "center", 
            frame = FALSE)
seg.map

tiff("figures/dmr_seg_map2011-15.tif", res = 300, units = "in", height = 7.5, width = 7.5, compression = "lzw")
dmr.map
dev.off()
