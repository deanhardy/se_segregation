rm(list=ls())

##then packages are loaded into the R environment
library(tidyverse)
library(tidycensus)
library(tigris)
library(tmap)
library(sf)

# census_api_key("", install = TRUE) ## Census API Key
yr <- 2015

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/se_segregation/')

##############################################################
## data import and prepping
##############################################################

## import HUC12 data
huc <- st_read(paste0(datadir, "data/data_share/huc12_atlurban")) %>%
  st_transform(4269)

## import MixedHydro results
mh <- st_read(paste0(datadir, 'data/spatial/hucMixedMetro.GEOJSON')) %>%
  mutate(AreaSqKm = as.numeric((st_area(geometry) / 1e6)))

## import DMR data downloaded from internet for HUC03 and filter to GA and make spatial
## https://echo.epa.gov/trends/loading-tool/get-data/custom-search
dmr10 <- read.csv(paste0(datadir, "data/dmr/dmr_2010_huc03.csv"), skip = 3)
# dmr11 <- read.csv(paste0(datadir, "data/dmr/dmr_2011_huc03.csv"), skip = 4)
# dmr12 <- read.csv(paste0(datadir, "data/dmr/dmr_2012_huc03.csv"), skip = 4)
# dmr13 <- read.csv(paste0(datadir, "data/dmr/dmr_2013_huc03.csv"), skip = 4)
# dmr14 <- read.csv(paste0(datadir, "data/dmr/dmr_2014_huc03.csv"), skip = 4)
# dmr15 <- read.csv(paste0(datadir, "data/dmr/dmr_2015_huc03.csv"), skip = 4)
dmr20 <- read.csv(paste0(datadir, "data/dmr/dmr_2020_huc03.csv"), skip = 3)

## import dmr data
## header descriptions: https://echo.epa.gov/tools/data-downloads/icis-npdes-dmr-summary
# dmr20 <- read.csv(paste0(datadir, "data/dmr/GA_FY2020_NPDES_DMRS.csv"))

## import facility registry service spatial data
# frs <- read.csv(paste0(datadir, "data/dmr/STATE_SINGLE_GA.csv"))

## combine dmr data, filter to Georgia, & sum by loading over watershed area in ATL urban area
dmr <- rbind(dmr10, 
             # dmr11, dmr12, dmr13, dmr14, dmr15, 
             dmr20) %>%
  filter(State == "GA") %>%
  st_as_sf(coords = c("Facility.Longitude", "Facility.Latitude"), crs = 4269) %>%
  group_by(NPDES.Permit.Number) %>%
  summarise(poll_sum = sum(Pollutant.Load..kg.yr., na.rm = TRUE)) %>% ## extract load data from dmr, summarize by facility
  st_intersection(mh) %>%
  mutate(dmr_SqKm = poll_sum/(AreaSqKm))
  # # st_intersection(seg) %>%
  # select(NPDES.Permit.Number, HUC_NO, dmr_SqKm)

# seg2 <- seg %>% select(HUC12, class10) %>% st_set_geometry(NULL)
# 
# dmr_seg <- merge(dmr_seg, seg2, by = 'HUC12')

boxplot(dmr_SqKm ~ class10, data = dmr, notch = F, ylim = c(0,3e5))

## import spatial data for counties as "background" to map
bkgd <- get_acs(geography = "county", 
               variables = "B03002_001E",
               state = c("AL", "GA", "SC"), 
               year = yr, geometry = TRUE)
bkgd <- st_zm(bkgd) ## drop "Z" data

## grab roads for cartographic purposes
rd <- primary_roads(year = yr)

##################################################################
## map ARC RACE DIVERSITY data with watersheds and DMR overlaid
##################################################################

## create custom palette with custom legend labels for seg indices
col <- c("#ff9999", "#99752e",
          "#ff9900", "#66cc00", 
         "#ffcc99", "#99ff99")
leg_col <- c("#ff9999", "#99752e",
             "#ff9900", "#66cc00",
             "#ffcc99", "#99ff99")
lbl <- c("Latinx (Mod)", "High Diversity",
         "White (Low)", "Black (Low)",
         "White (Mod)", "Black (Mod)")

## mapping race and diversity
seg_map <- 
  tm_shape(huc) +
  tm_borders(col = "white") +
  tm_shape(bkgd) +
  tm_fill(col = "azure1") +
  tm_shape(seg) +
  tm_fill("class10", legend.show = FALSE, palette = col) +
  tm_shape(rd) + 
  tm_lines(col = "black") +
  tm_shape(bkgd) +
  tm_borders() +
  tm_add_legend(type = c("fill"), labels = lbl, col = leg_col, 
                title = "Race (Diversity)") +
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
seg_map

# tiff("figures/raceseg_map2011-15.tif", res = 300, units = "in", 
#      height = 7.5, width = 10, compression = "lzw")
# seg_map
# dev.off()

# ## mapping racial segregation with watersheds
# huc_map <- 
#   tm_shape(huc) +
#   tm_borders(col = "white") +
#   tm_shape(bkgd) +
#   tm_fill(col = "azure1") +
#   tm_shape(filter(seg, rd15c != 0)) +
#   tm_fill("rd15c", legend.show = FALSE, palette = col) +
#   tm_shape(bkgd) +
#   tm_borders() +
#   tm_shape(filter(huc, PERCENTAGE >= 50)) +
#   tm_borders(col = "black") +
#   tm_add_legend(type = c("fill"), labels = lbl, col = leg_col, 
#                 title = "Racial Diversity\n(by majority group)") +
#   tm_compass(type = "arrow", size = 4, position = c(0.82, 0.08)) +
#   tm_scale_bar(breaks = c(0,20), size = 1.1, position= c(0.8, 0.0)) +
#   tm_legend(position = c(0.025, 0.05),
#             bg.color = "white",
#             frame = TRUE,
#             legend.text.size = 1.1,
#             legend.title.size = 1.4) + 
#   tm_layout(frame = FALSE, 
#             outer.margins=c(0,0,0,0), 
#             inner.margins=c(0,0,0,0), asp=0)
# huc_map
# 
# tiff("figures/raceseg_huc_map2011-15.tif", res = 300, units = "in", 
#      height = 7.5, width = 10, compression = "lzw")
# huc_map
# dev.off()

rd <- primary_roads(year = 2016)

## mapping racial segregation with watersheds and pollution
dmr_map <- 
  tm_shape(huc) +
  tm_borders(col = "white") +
  # tm_shape(bkgd) +
  # tm_fill(col = "azure1") +
  tm_shape(seg) +
  tm_polygons("class10", legend.show = FALSE, palette = col) +
  # tm_shape(bkgd) +
  # tm_borders() +
  # tm_shape(filter(huc, PERCENTAGE >= 50)) +
  # tm_borders(col = "black") +
  tm_add_legend(type = c("fill"), labels = lbl, col = leg_col, 
                title = "Race (Diversity)") +
  tm_shape(rd) + 
  tm_lines() + 
  tm_shape(huc_cntrd) + 
  tm_bubbles(size = "dmr_SqKm", col = "black", scale = 1, title.size = "Pollution (kg/yr/km^2)",
             size.lim = c(0,6e5), sizes.legend = c(1e5, 3e5, 4e5, 5e5)) + 
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
dmr_map

tiff("figures/race_diversity_hucdmr_map2011-15.tif", res = 300, units = "in", 
     height = 7.5, width = 10, compression = "lzw")
dmr_map
dev.off()


###################################################################
## map ARC INCOME DIVERSITY data with watersheds and DMR overlaid
## still working out clipping of huc to ARC region
###################################################################

# ## create custom palette with custom legend labels for seg indices
# col2 <- c("white", "#ff9900", "#66cc00",  
#          "#ffcc99", "#99ff99", "#ff9999", "#cc99ff",
#          "#99752e")
# leg_col2 <- c("#ff9900", "#66cc00",  
#              "#ffcc99", "#99ff99", "#ff9999", "#cc99ff",
#              "#99752e")
# lbl2 <- c("Low (Low-Income)", "Low (Low-Middle)",
#          "Mod (Low-Income)", "Mod (Low-Middle)", "Mod (Upper-Middle)", "Mod (Upper)", 
#          "High Diversity")
# 
# ## mapping
# inc_map <- 
#   tm_shape(huc) +
#   tm_borders(col = "white") +
#   tm_shape(seg) +
#   tm_fill("i15c", legend.show = FALSE, palette = col2) +
#   tm_add_legend(type = c("fill"), labels = lbl2, col = leg_col2, 
#                 title = "Income Diversity\n(by dominant group)") +
#   tm_shape(huc) +
#   tm_borders(col = "black") +
#   tm_shape(dmr) + 
#   tm_bubbles(size = "dmr_area", col = "black", scale = 2, title.size = "Pollution (kg/yr/km^2)",
#              size.lim = c(0,1.5e3), sizes.legend = c(10, 100, 250, 500, 750, 1000)) + 
#   tm_compass(type = "arrow", size = 2, position = c(0.83, 0.06)) +
#   tm_scale_bar(breaks = c(0,20), size = 0.8, position= c(0.8, 0.0)) +
#   tm_legend(position = c(-0.2, 0)) + 
#   tm_layout(main.title = "Greater Atlanta Metro Area (2011-2015)", main.title.position = "center", 
#             frame = FALSE)
# # tm_credits("*Pollution data from EPA Discharge Monitoring Report for 2011-2015; Segregation data from ACS-5YR 2011-2015",
# #            position = c("RIGHT","BOTTOM"), size = 14)
# inc_map
# 
# tiff("figures/dmr_income_seg_map2011-15.tif", res = 300, units = "in", 
#      height = 7.5, width = 10, compression = "lzw")
# inc_map
# dev.off()


###################################################################
## graphing pollution rates by racial diversity at watershed scale
###################################################################

## calculate the area of seg block groups within each watershed
## via https://rpubs.com/rural_gis/255550
int <- as.tibble(st_intersection(seg, huc))

#add in an area count column to the tibble & calc area and percent area for each BG by watershed
library(lwgeom)
int <- int %>%
  mutate(AreaSqKmHUC = as.numeric((st_area(int$geometry) / 1e6))) %>%
  mutate(percBGinHUC = AreaSqKmHUC/(aland+awater)*1e6)

int$AreaSqKmHUC
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

# library(RColorBrewer)
# n <- 19
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# pie(rep(1,n), col=sample(col_vector, n))
# col3 = sample(col_vector, n)


col <- c("#4DAF4A", "#A6761D", "#8DA0CB", "#7570B3", "#377EB8", "#FED9A6", "#66A61E", "#A6CEE3", "#66C2A5", "#FB8072",
         "#A65628", "#B3CDE3", "#E78AC3", "#D95F02", "#FFFFCC", "#A6D854", "#666666", "#FFFFB3", "#B2DF8A")

##plot low diversity by pollution as kg/yr/km&2
fig <- ggplot(se_seg_dmr) +
  geom_smooth(aes(PercentLD*100, dmr_load), method = "loess", 
              span = 2, se = TRUE, linetype = "solid", level = 0.95) +
  geom_smooth(aes(PercentLD*100, dmr_load), method = "lm", 
              se = FALSE, linetype = "dashed", level = 0.95) +
  geom_point(aes(PercentLD*100, dmr_load, col = Name), size = 3) + 
  scale_x_continuous(limits = c(0,60)) +
  scale_y_continuous(limits = c(-250,1500)) +
  xlab("Watershed Area (%) with Low Diversity (People of Color)") + 
  ylab("Total Pollution Loading (kg/yr/km^2)") + 
  scale_color_manual(name = "HUC10 Watershed",
                     values = col) +
  ggtitle("Pollution by Segregation (2011-2015)")
fig

tiff("figures/dmr_race_seg2011-15.tiff", res = 300, compression = "lzw", units = "in", 
     height = 5.5, width = 8)
fig
dev.off()




