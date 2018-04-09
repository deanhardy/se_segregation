rm(list=ls())

##then packages are loaded into the R environment
library(tidyverse)
library(tidycensus)
library(tigris)
library(tmap)
library(sf)

# census_api_key("", install = TRUE) ## Census API Key
yr <- '2015'

################################################################
## data import and prepping

### cut and past from sso_seg script, need to clean up prep work
################################################################

## import revised sso data
sso <- read.csv("data/sso/sso.csv") %>%
  filter(ESTIMATED != "NA", YEAR %in% c(2011, 2012, 2013, 2014, 2015, 2016, 2017)) %>%
  st_as_sf(., coords = c("X", "Y"), crs = 4269)

## import HUC10 data
huc <- st_read("data/spatial/huc10.shp") %>%
  st_transform(4269)

## import segregation/diversity data from 2011-2015 ACS
seg <- st_read("data/geojson/arc15.geojson") %>%
  mutate(rd15c = as.factor(rd15c), i15c = as.factor(i15c)) %>%
  st_transform(4269) %>%
  filter(county == "DeKalb County")

## calculate the area of seg block groups within each watershed
## via https://rpubs.com/rural_gis/255550
int <- as.tibble(st_intersection(seg, huc))

#add in an area count column to the tibble & calc area and percent area for each BG by watershed
library(lwgeom)
int2 <- int %>%
  mutate(AreaSqKM_BGinHUC = as.numeric((st_area(int$geometry) / 1e6))) %>%
  mutate(percBGinHUC = AreaSqKM_BGinHUC/(aland+awater)*1e6)

## Want the percent of each watershed that has low diversity with majority people of color
## will plot that against volume sso per area
se_seg <- int2 %>%
  filter(PERCENTAGE >= 50) %>%
  mutate(lowdiv_AreaSQKM = ifelse(rd15c %in% c(3,4,5,6,7), AreaSqKM_BGinHUC, 0)) %>%
  group_by(Name) %>%
  summarise(Area_LD = sum(lowdiv_AreaSQKM), Area_HUC = sum(AreaSqKM_BGinHUC)) %>%
  mutate(PercentLD = Area_LD/Area_HUC)

## calculate total area as SqKM of DeKalb by HUC
dkb_huc <- int %>%
  mutate(AreaSqKM_BGinHUC = as.numeric((st_area(int$geometry) / 1e6))) %>%
  group_by(Name) %>%
  summarise(AreaSqKMinHUC = sum(AreaSqKM_BGinHUC)) %>%
  select(Name, AreaSqKMinHUC)

dkb_huc2 <- left_join(huc, dkb_huc, by = 'Name') %>%
  filter(AreaSqKMinHUC != 'NA') %>%
  select(Name, AreaSqKMinHUC)

###################################################################
## graphing REPORTING rates by watershed

###################################################################

df <- st_intersection(dkb_huc2, sso) %>%
  mutate(Name = as.character(Name)) %>% 
  count(Name, sort = TRUE)

df2 <- left_join(df, se_seg, by = 'Name')

## reporting counts
fig2 <- ggplot(df2) +
  geom_col(aes(x = reorder(Name, -n), n)) + 
  xlab("HUC10 Watershed") + 
  ylab("Reported SSOs (2011-2017)") +
  theme(axis.text.x=element_text(angle=45,hjust=1))
fig2

tiff("figures/dkb_sso2011-2017_reporting_counts.tiff", res = 300, compression = "lzw", units = "in", 
     height = 5.5, width = 8)
fig2
dev.off()

## reporting rates
fig3 <- ggplot(df2) +
  geom_col(aes(x = reorder(Name, -n/Area_HUC), n/Area_HUC)) + 
  xlab("HUC10 Watershed") + 
  ylab("Reported SSOs / KM^2 (2011-2017)") +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        plot.margin = margin(1,1,1,1.1, "cm"))
fig3

tiff("figures/dkb_sso2011-2017_reporting_rates.tiff", res = 300, compression = "lzw", units = "in", 
     height = 5.5, width = 7)
fig3
dev.off()
