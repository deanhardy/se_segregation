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
yr <- '2016'

## convert sso shapefile to csv for manual cleaning of ESTIMATED column in Excel
# library(dmm)
# st_read("data/sso/sso.shp") %>%
#   st_transform(., 4269) %>%
#   st_as_sf(., coords = c("lon", "lat"), crs = 4269) %>% 
#   cbind(., st_coordinates(.)) %>% 
#   st_set_geometry(NULL) %>% 
#   write_csv(., 'data/sso/sso.csv')

## import revised sso data and sum volume pollution by address
sso <- read.csv("data/sso/sso.csv") %>%
  filter(ESTIMATED != "NA", YEAR %in% c(2012, 2013, 2014, 2015, 2016)) %>%
  st_as_sf(., coords = c("X", "Y"), crs = 4269) 

## import HUC10 data
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

## grab aland and awater from tigris data
bg <- block_groups("Georgia", "DeKalb") %>%
  st_as_sf() %>%
  select(GEOID, ALAND, AWATER) %>%
  rename(aland = ALAND, awater = AWATER) %>%
  mutate(aland = as.numeric(aland), awater = as.numeric(awater)) %>%
  st_set_geometry(NULL)

## 
shp <- left_join(shp, bg, by = "GEOID")

## append census race data to spatial data
dkb_shp <- left_join(shp, dkb, by = "GEOID", copy = TRUE) %>%
  dplyr::select(-moe, -variable, -NAME) %>%
  rename(B03002_001 = estimate) %>%
  mutate(perc_POC = 1-(B03002_003/B03002_001), count_POC = B03002_001 - B03002_003) %>%
  st_as_sf() %>%
  st_transform(4269)

## import spatial data for counties as "background" to map
bkgd <- get_acs(geography = "county", 
                variables = "B03002_001E",
                state = c("AL", "GA", "SC"), 
                year = yr, geometry = TRUE)
bkgd <- st_zm(bkgd) ## drop "Z" data

## grab roads for cartographic purposes
rd <- primary_roads(year = yr)


##################################################################
## map DeKalb RACE PERCENTAGE data
##################################################################

## mapping racial percentages
race_map <-
  tm_shape(dkb_shp) + 
  tm_fill() +
  tm_shape(bkgd) +
  tm_fill(col = "azure1") +
  tm_shape(dkb_shp) +
  tm_polygons("perc_POC", palette = "Greens",
          title = "People of Color (%)") +
  tm_shape(huc) + 
  tm_borders(col = "black") +
  tm_shape(bkgd) +
  tm_borders() +
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
race_map

tiff("figures/dkb_raceperc_hucsso_map2012-16.tif", res = 300, units = "in", 
     height = 7.5, width = 12, compression = "lzw")
race_map
dev.off()


#####################################################################
## graphing sso by racial segregation at watershed scale
#####################################################################

## calculate the area of block groups within each watershed
## begin by intersecting features and creating "fragments" by spliting BGs by HUCs
## via https://rpubs.com/rural_gis/255550
int <- as.tibble(st_intersection(dkb_shp, huc))

## Want the percent of POC in each watershed
## will plot that against volume sso per year per area
## add in an area count column to the tibble & calc area and percent area for each BG fragment by watershed
## then calculate count of POC and total POP in each BG fragment by HUC
library(lwgeom)
se_race <- int %>%
  mutate(AreaSqKm_BGinHUC = as.numeric((st_area(int$geometry) / 1e6))) %>%
  mutate(perc_BGinHUC = AreaSqKm_BGinHUC/(aland+awater)*1e6) %>%
  mutate(count_POCinHUC = perc_BGinHUC * count_POC, 
         count_POPinHUC = perc_BGinHUC * B03002_001) %>%
  group_by(Name) %>%
  summarise(sumPOCinHUC = sum(count_POCinHUC), sumPOPinHUC = sum(count_POPinHUC)) %>%
  mutate(percPOCinHUC = sumPOCinHUC/sumPOPinHUC)

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

se_race_sso <- merge(se_race,sso_sum, by = "Name")

# library(RColorBrewer)
# n <-6
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=sample(col_vector, n))
# col3 = sample(col_vector, n)

col <- c("blue", "red", "yellow", "green", "black", "purple")

##plot low diversity by pollution as kg/yr/km&2
fig <- ggplot(se_race_sso) +
  # geom_smooth(aes(percPOCinHUC*100, SSOloadSqKM/1000), method = "loess",
  #             span = 2, se = TRUE, linetype = "solid", level = 0.95) +
  geom_smooth(aes(percPOCinHUC*100, SSOloadSqKM/1000), method = "lm",
              se = TRUE, linetype = "dashed", level = 0.95) +
  geom_point(aes(percPOCinHUC*100, SSOloadSqKM/1000, col = Name), size = 3) + 
  scale_x_continuous(limits = c(0,100)) +
  scale_y_continuous(limits = c(-2000,3300)) +
  xlab("People of Color (%)") + 
  ylab("SSO Volume (1000 Gallons/KM^2)") + 
  scale_color_manual(name = "HUC10 Watershed",
                     values = col) +
  ggtitle("Pollution by Race (2012-2016)")
fig

tiff("figures/dkb__raceperc_hucsso_2012-16.tiff", res = 300, compression = "lzw", units = "in", 
     height = 5.5, width = 8)
fig
dev.off()

