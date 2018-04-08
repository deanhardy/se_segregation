rm(list=ls())

##then packages are loaded into the R environment
library(tidyverse)
library(tidycensus)
library(tigris)
library(tmap)
library(sf)

# census_api_key("") ## install personal census API key

##############################################################
## data import and prepping
##############################################################

## define year for census data import
yr <- '2015'

## import watershed data
## note that PERCENTAGE in 'huc' object equals that in ARC (from ArcGIS)
huc <- st_read("data/spatial/huc10.shp") %>%
  st_transform(4269)

## downloads ACS data for 2011-2015
acs <- load_variables(yr, "acs5", cache = TRUE)

## define region for census import
cnty <- c("Cherokee", "Clayton", "Cobb", "Dekalb", "Douglas",
           "Fayette", "Fulton", "Gwinnett", "Henry", "Rockdale")

# ## import custom counties
# cnty <- read.csv("data/cnty.txt") %>%
#   mutate(NAME10 = as.character(NAME10)) %>%
#   dplyr::select(NAME10) %>%
#   unlist(use.names = FALSE)

## import race variables of interest
race_vars <- c(white = "B03002_003E", black = "B03002_004E", 
               native_american = "B03002_005E", asian = "B03002_006E", 
               hawaiian = "B03002_007E", other = "B03002_008E", 
               multiracial = "B03002_009E", latinx = "B03002_012E")

## import ACS data for ARC (ie region defined by "cnty")
arc <- get_acs(geography = "block group", 
              variables = race_vars,
              state = "GA", county = cnty, 
              year = yr) %>%
  dplyr::select(-moe, -NAME) %>%
  spread(key = "variable", value = "estimate")

## import spatial data for "cnty" region
shp <- get_acs(geography = "block group", 
               variables = "B03002_001E",
               state = "GA", county = cnty, 
               year = yr, geometry = TRUE)
shp <- st_zm(shp) ## drop "Z" data

## append census race data to spatial data
arc_shp <- left_join(shp, arc, by = "GEOID", copy = TRUE) %>%
  dplyr::select(-moe, -variable, -NAME) %>%
  rename(B03002_001 = estimate) %>%
  mutate(perc_POC = 1-(B03002_003/B03002_001)) %>%
  st_as_sf() %>%
  st_transform(4269)

## import DMR data downloaded from internet for HUC03 and filter to GA and make spatial
## https://echo.epa.gov/trends/loading-tool/get-data/custom-search
dmr11 <- read.csv("data/dmr/dmr_2011_huc03.csv", skip = 4)
dmr12 <- read.csv("data/dmr/dmr_2012_huc03.csv", skip = 4)
dmr13 <- read.csv("data/dmr/dmr_2013_huc03.csv", skip = 4)
dmr14 <- read.csv("data/dmr/dmr_2014_huc03.csv", skip = 4)
dmr15 <- read.csv("data/dmr/dmr_2015_huc03.csv", skip = 4)

## combine dmr data, clip to Georgia, & sum by loading over watershed area in ARC region
dmr <- rbind(dmr11, dmr12, dmr13, dmr14, dmr15) %>%
  filter(State == "GA") %>%
  st_as_sf(coords = c("Facility.Longitude", "Facility.Latitude"), crs = 4269) %>%
  group_by(NPDES.Permit.Number) %>%
  summarise(sum = sum(Pollutant.Load..kg.yr., na.rm = TRUE)) %>% ## extract load data from dmr, summarize by facility
  st_intersection(huc) %>%
  mutate(dmr_area = sum/(AreaSqKm * PERCENTAGE)) %>%
  st_intersection(arc.shp) %>%
  select(NPDES.Permit.Number, HUC10, Name, dmr_area)

## import spatial data for counties as "background" to map
bkgd <- get_acs(geography = "county", 
                variables = "B03002_001E",
                state = c("AL", "GA", "SC"), 
                year = yr, geometry = TRUE)
bkgd <- st_zm(bkgd) ## drop "Z" data

## grab roads for cartographic purposes
rd <- primary_roads(year = yr)


##################################################################
## map ARC RACE PERCENTAGE data
##################################################################

## mapping racial percentages
race_map <- 
  tm_shape(huc) +
  tm_borders(col = "white") +
  tm_shape(bkgd) +
  tm_fill(col = "azure1") +
  tm_shape(arc_shp) +
  tm_fill("perc_POC", palette = "Greens",
          title = "People of Color (%)") +
  tm_shape(rd) + 
  tm_lines(col = "black") +
  tm_shape(bkgd) +
  tm_borders() +
  tm_compass(type = "arrow", size = 4, position = c(0.82, 0.08)) +
  tm_scale_bar(breaks = c(0,20), size = 1.1, position= c(0.8, 0.0)) +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = 1.1,
            legend.title.size = 1.4) + 
  tm_layout(frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=0)
race_map

tiff("figures/racepercent_map2011-15.tif", res = 300, units = "in", 
     height = 7.5, width = 10, compression = "lzw")
race_map
dev.off()


#####################################################################
## graphing pollution rates by racial percentages at watershed scale
#####################################################################

## calculate the area of seg block groups within each watershed
## via https://rpubs.com/rural_gis/255550
int <- as.tibble(st_intersection(arc.shp, huc))

###
# need to grab aland and awater via tidycensus for arc.shp!!!!
###
# add in an area count column to the tibble & calc area and percent area for each BG by watershed
library(lwgeom)
int <- int %>%
  mutate(AreaSqKmHUC = as.numeric((st_area(int$geometry) / 1e6))) %>%
  mutate(PercentHUC = AreaSqKmHUC/(aland+awater)*1e6)

## Want the percent of each watershed that has low diversity with majority people of color
## will plot that against volume dmr per year per area
se_seg <- int %>%
  filter(PERCENTAGE >= 50) %>%
  mutate(lowdiv_AreaSQKM = ifelse(rd15c %in% c(3,4,5,6,7), AreaSqKmHUC, 0)) %>%
  group_by(Name) %>%
  summarise(Area_LD = sum(lowdiv_AreaSQKM), Area_HUC = sum(AreaSqKmHUC)) %>%
  mutate(PercentLD = Area_LD/Area_HUC)

dmr_sum <- dmr %>%
  group_by(Name) %>%
  summarise(dmr_load = sum(dmr_area))

se_seg_dmr <- merge(se_seg, dmr_sum, by = "Name")

library(RColorBrewer)
n <- 19
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))
col3 = sample(col_vector, n)

##plot low diversity by pollution as kg/yr/km&2
fig <- ggplot(se_seg_dmr) +
  geom_point(aes(PercentLD*100, dmr_load, col = Name), size = 3) + 
  geom_smooth(aes(PercentLD*100, dmr_load), method = "loess", 
              span = 2, se = TRUE, linetype = "solid", level = 0.95) +
  # geom_smooth(aes(PercentLD*100, dmr_load), method = "lm", se = TRUE, linetype = "solid", level = 0.95) +
  scale_x_continuous(limits = c(0,60)) +
  xlab("Watershed Area (%) with Low Diversity (People of Color)") + 
  ylab("Total Pollution Loading (kg/yr/km^2)") + 
  scale_color_manual(name = "HUC10 Watershed",
                     values = col3) +
  ggtitle("Pollution by Segregation (2011-2015)")
fig

tiff("figures/dmr_race_seg2011-15.tiff", res = 300, compression = "lzw", units = "in", 
     height = 5.5, width = 8)
fig
dev.off()
