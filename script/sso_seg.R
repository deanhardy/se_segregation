rm(list=ls())

##then packages are loaded into the R environment
library(tidyverse)
library(tidycensus)
library(tigris)
library(tmap)
library(sf)

# census_api_key("", install = TRUE) ## Census API Key
yr <- '2015'

##############################################################
## data import and prepping
##############################################################

## convert sso shapefile to csv for manual cleaning of ESTIMATED column in Excel
## only needed to do once
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

## import HUC10 data
huc <- st_read("data/spatial/huc10.shp") %>%
  st_transform(4269)

## import segregation/diversity data from 2011-2015 ACS
seg <- st_read("data/geojson/arc15.geojson") %>%
  mutate(rd15c = as.factor(rd15c), i15c = as.factor(i15c)) %>%
  st_transform(4269) %>%
  filter(county == "DeKalb County")

## import spatial data for counties as "background" to map
bkgd <- get_acs(geography = "county", 
                variables = "B03002_001E",
                state = c("GA"), 
                year = yr, geometry = TRUE)
bkgd <- st_zm(bkgd) ## drop "Z" data

## grab roads for cartographic purposes
rd <- primary_roads(year = yr)

## import spatial data for defining map area
shp <- get_acs(geography = "block group", 
               variables = "B03002_001E",
               state = "GA", county = "DeKalb", 
               year = yr, geometry = TRUE)
shp <- st_zm(shp) ## drop "Z" data

##################################################################
## map DeKalb RACE PERCENTAGE data
##################################################################

## create custom palette with custom legend labels for seg indices
col <- c("white", "#ff9900", "#66cc00", "#ff6666", "#9966ff", 
         "#ffcc99", "#99ff99", "#ff9999", "#99752e")
leg_col <- c("#ff9900", "#66cc00", "#ff6666", "#9966ff", 
             "#ffcc99", "#99ff99", "#ff9999", "#99752e")
lbl <- c("Low (White)", "Low (African American)", "Low (Asian)", "Low (Latinx)",
         "Mod (White)", "Mod (African American)", "Mod (Latinx)", "High Diversity")

## mapping racial segregation with watersheds and pollution
sso_map <- 
  tm_shape(shp) +
  tm_fill() +
  tm_shape(bkgd) +
  tm_fill(col = "azure1") +
  tm_shape(filter(seg, rd15c != 0)) +
  tm_fill("rd15c", legend.show = FALSE, palette = col) +
  tm_shape(bkgd) +
  tm_borders() +
  tm_shape(filter(huc, PERCENTAGE >= 50)) +
  tm_borders(col = "black") +
  tm_add_legend(type = c("fill"), labels = lbl, col = leg_col, 
                title = "Racial Diversity\n(by majority group)") +
  tm_shape(sso) +
  tm_bubbles(size = "ESTIMATED", col = "black", scale = 2, title.size = "SSO Volume (Gallons)",
             size.lim = c(0,6.5e5), sizes.legend = c(1e5, 3e5, 6e5)) + 
  tm_compass(type = "arrow", size = 4, position = c(0.82, 0.08)) +
  tm_scale_bar(breaks = c(0,5), size = 1.1, position= c(0.8, 0.0)) +
  tm_legend(position = c(0.01, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = 1.1,
            legend.title.size = 1.4) + 
  tm_layout(frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=3.2/2)
sso_map

tiff("figures/dkb_raceseg_hucsso_map2011-15.tif", res = 300, units = "in", 
     height = 7.5, width = 12, compression = "lzw")
sso_map
dev.off()


###################################################################
## graphing pollution rates by racial diversity at watershed scale
###################################################################

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

## sum sso loading by huc area in dekalb
sso_sum <- 
  st_intersection(dkb_huc2, sso) %>%
  mutate(loadSqKM = ESTIMATED/AreaSqKMinHUC) %>%
  group_by(Name) %>%
  summarise(SSOloadSqKM = sum(ESTIMATED))

se_seg_sso <- merge(se_seg,sso_sum, by = "Name")

col <- c("blue", "red", "yellow", "green", "black", "purple")

##plot low diversity by pollution as kg/yr/km&2
fig <- ggplot(se_seg_sso) +
  # geom_smooth(aes(PercentLD*100, SSOloadSqKM), method = "loess", 
  #             span = 2, se = TRUE, linetype = "solid", level = 0.95) +
  geom_smooth(aes(PercentLD*100, SSOloadSqKM/1000), method = "lm", 
              se = TRUE, linetype = "dashed", level = 0.95) +
  geom_point(aes(PercentLD*100, SSOloadSqKM/1000, col = Name), size = 3) + 
  # scale_x_continuous(limits = c(0,100)) +
  # scale_y_continuous(limits = c(-250,1500)) +
  xlab("Watershed Area (%) with Low Diversity (People of Color)") + 
  ylab("SSO Volume (1000 Gallons/km^2)") + 
  scale_color_manual(name = "HUC10 Watershed",
                     values = col) +
  ggtitle("Pollution by Segregation (2011-2015)")
fig

tiff("figures/dkb_sso2012-2016_raceseg2011-15.tiff", res = 300, compression = "lzw", units = "in", 
     height = 5.5, width = 8)
fig
dev.off()


