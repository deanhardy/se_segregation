rm(list=ls())

##then packages are loaded into the R environment
library(tidyverse)
library(tidycensus)
library(viridis)
library(tigris)
library(tmap)
library(sf)
library(ggthemes)
library(wellknown)

# census_api_key("") ## Already installed

yr <- "2016"

## import segregation data from 2011-2015 ACSs
seg <- st_read("data/geojson/arc15.geojson")

## convert sso shapefile to csv for manual cleaning of ESTIMATED column in Excel
# library(dmm)
# st_read("data/sso/sso.shp") %>%
#   st_transform(., 4269) %>%
#   st_as_sf(., coords = c("lon", "lat"), crs = 4269) %>% 
#   cbind(., st_coordinates(.)) %>% 
#   st_set_geometry(NULL) %>% 
#   write_csv(., 'data/sso/sso.csv')

## import revised sso data
sso <- read.csv("data/sso/sso.csv") %>%
  filter(ESTIMATED != "NA", YEAR %in% c(2012, 2013, 2014, 2015, 2016)) %>%
  st_as_sf(., coords = c("X", "Y"), crs = 4269)

## sum sso volumes
# sso.vol <- sso %>%
#   summarise(sum = sum(ESTIMATED, na.rm = TRUE))

##import HUC10 data
# nhd <- get_nhd(shp, "arc_nhd") ## didn't work out using "FedData" package
huc <- st_read("data/spatial/huc10.shp") %>%
  st_transform(4269)

## import race variables of interest
race_vars <- c(white = "B03002_003E", black = "B03002_004E", 
               native_american = "B03002_005E", asian = "B03002_006E", 
               hawaiian = "B03002_007E", other = "B03002_008E", 
               multiracial = "B03002_009E", latinx = "B03002_012E")

## import ACS data for DeKalb
## transform ACS census data for joining to spatial data
dkb <- get_acs(geography = "block group", 
               variables = race_vars,
               state = "GA", county = "DeKalb", 
               year = yr) %>%
  select(-NAME, -moe) %>%
  spread(key = "variable", value = "estimate")

## import spatial data for "cnty" region
shp <- get_acs(geography = "block group", 
               variables = "B03002_001E",
               state = "GA", county = "DeKalb", 
               year = yr, geometry = TRUE)
shp <- st_zm(shp) ## drop "Z" data

## plot to confirm extent
qtm(shp, fill = "estimate")

## append census race data to spatial data
dkb.shp <- left_join(shp, dkb, by = "GEOID", copy = TRUE) %>%
  select(-moe, -variable, -NAME) %>%
  rename(B03002_001 = estimate) %>%
  mutate(nwnl_prc = 1-(B03002_003/B03002_001)) %>%
  st_transform(4269)

## map DKB census data for NWNL people
map <- tm_shape(dkb.shp) +
  tm_polygons("nwnl_prc",
              palette = "Purples",
              title = "Non-White,\nNon-Latinx\nPopulation (%)") +
  tm_compass(type = "arrow", size = 3, position = c(0.05, 0.1)) +
  tm_scale_bar(breaks = c(0,10), size = 0.8, position= c(0.06, 0.04)) +
  tm_legend(position = c(0.9,0.05)) +
  tm_layout(main.title = "DeKalb County (2011-15)", main.title.position = "center", frame = FALSE)
map

dkb.huc <- st_intersection(dkb.shp, huc)
qtm(dkb.huc, fill = "nwnl_prc")

## union ARC race data with DMR summed data
huc.sso <- st_intersection(huc, sso)

## map ARC race data with watersheds and DMR overlaid
map <- tm_shape(dkb.shp) +
  tm_polygons("nwnl_prc",
              palette = "Greys",
              title = "Non-White /\nNon-Latinx\nPopulation (%)") +
  tm_shape(huc) + 
  tm_borders(col = "yellow") +
  tm_shape(huc.sso) + 
  tm_bubbles(size = "ESTIMATED", col = "green", scale = 2, title.size = "Volume (Gallons)") + 
  tm_compass(type = "arrow", size = 2, position = c(0.8, 0.07)) +
  tm_scale_bar(breaks = c(0,3), size = 0.6, position= c(0.8, 0.0)) +
  tm_legend(position = c(-0.3, 0)) + 
  tm_layout(main.title = "DeKalb (2012-2016)", main.title.position = "center", 
            frame = FALSE)
map

tiff("figures/dekalb_sso_map2012-16.tif", res = 300, units = "in", height = 7.5, width = 8.5, compression = "lzw")
map
dev.off()


## sum pollution by HUC10 watershed, hence "hp"
hp.sum <- huc.sso %>%
  group_by(Name) %>%
  summarise(sum = sum(ESTIMATED/AreaSqKm)) %>%
  st_set_geometry(NULL)

## percent nwnl by HUC10 watershed, hence "hr"; note that this approach doesn't account for BGs
## that were cut off, so no adjustment made for some BGs that crossover watershed boundary
hr.sum <- dkb.huc %>%
  ungroup() %>%
  group_by(Name) %>%
  summarise(sum003 = sum(B03002_003), sum001 = sum(B03002_001)) %>%
  mutate(nwnl_prc = 1-(sum003/sum001)) %>%
  st_set_geometry(NULL)

## join pollution sums and percent race by watershed
hpr.sum <- merge(hp.sum, hr.sum, by = "Name")

library(RColorBrewer)
n <- 20
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
col = sample(col_vector, n)

##plot % nwnl by pollution as kg/yr/km&2
fig <- ggplot(hpr.sum, aes(nwnl_prc, sum)) +
  geom_point(aes(nwnl_prc, sum, col = Name), size = 3) + 
  geom_smooth(method = "lm", se = TRUE, linetype = "solid", level = 0.95) +
  scale_x_continuous(limits = c(0,1)) +
  xlab("Non-white / Non-Latinx Population (%)") + 
  ylab("SSO Volume (Gallons/KM^2)") + 
  scale_color_manual(name = "HUC10 Watershed",
                     values = col) +
  ggtitle("Pollution by Race (2012-2016)")
fig

tiff("figures/dekalb_sso_by_race_2012-16.tiff", res = 300, compression = "lzw", units = "in", 
     height = 5.5, width = 8)
fig
dev.off()

