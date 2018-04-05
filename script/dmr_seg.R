rm(list=ls())

##then packages are loaded into the R environment
library(tidyverse)
library(tmap)
library(sf)

##############################################################
## data import and prepping
##############################################################

##import HUC10 data
huc <- st_read("data/spatial/huc10.shp") %>%
  st_transform(4269)
  
## import segregation/diversity data from 2011-2015 ACS
seg <- st_read("data/geojson/arc15.geojson") %>%
  mutate(rd15c = as.factor(rd15c), i15c = as.factor(i15c)) %>%
  st_transform(4269)

## import DMR data downloaded from internet for HUC03 and filter to GA and make spatial
## https://echo.epa.gov/trends/loading-tool/get-data/custom-search
dmr11 <- read.csv("data/dmr/dmr_2011_huc03.csv", skip = 4)
dmr12 <- read.csv("data/dmr/dmr_2012_huc03.csv", skip = 4)
dmr13 <- read.csv("data/dmr/dmr_2013_huc03.csv", skip = 4)
dmr14 <- read.csv("data/dmr/dmr_2014_huc03.csv", skip = 4)
dmr15 <- read.csv("data/dmr/dmr_2015_huc03.csv", skip = 4)

## combine dmr data, clip to Georgia, & sum by loading over watershed area in ARC region
## note that PERCENTAGE in 'huc' object equals that in ARC (from ArcGIS)
dmr <- rbind(dmr11, dmr12, dmr13, dmr14, dmr15) %>%
  filter(State == "GA") %>%
  st_as_sf(coords = c("Facility.Longitude", "Facility.Latitude"), crs = 4269) %>%
  group_by(NPDES.Permit.Number) %>%
  summarise(sum = sum(Pollutant.Load..kg.yr., na.rm = TRUE)) %>% ## extract load data from dmr, summarize by facility
  st_intersection(huc) %>%
  mutate(dmr_area = sum/(AreaSqKm * PERCENTAGE)) %>%
  st_intersection(seg) %>%
  select(NPDES.Permit.Number, HUC10, Name, dmr_area)


##################################################################
## map ARC RACE DIVERSITY data with watersheds and DMR overlaid
## still working out clipping of huc to ARC region
##################################################################

## create custom palette with custom legend labels for seg indices
col <- c("white", "#ff9900", "#66cc00", "#ff6666", "#9966ff", 
         "#ffcc99", "#99ff99", "#ff9999", "#99752e")
leg_col <- c("#ff9900", "#66cc00", "#ff6666", "#9966ff", 
             "#ffcc99", "#99ff99", "#ff9999", "#99752e")
lbl <- c("Low (White)", "Low (African American)", "Low (Asian)", "Low (Latinx)",
         "Mod (White)", "Mod (African American)", "Mod (Latinx)", "High Diversity")

## mapping
raceseg_map <- 
  tm_shape(huc) +
  tm_borders(col = "white") +
  tm_shape(filter(seg, rd15c != 0)) +
  tm_fill("rd15c", legend.show = FALSE, palette = col) +
  tm_add_legend(type = c("fill"), labels = lbl, col = leg_col, 
                title = "Racial Diversity\n(by majority group)") +
  tm_shape(huc) +
  tm_borders(col = "black") +
  tm_shape(dmr) + 
  tm_bubbles(size = "dmr_area", col = "black", scale = 2, title.size = "Pollution (kg/yr/km^2)",
             size.lim = c(0,1.5e3), sizes.legend = c(10, 100, 250, 500, 750, 1000)) + 
  tm_compass(type = "arrow", size = 2, position = c(0.83, 0.06)) +
  tm_scale_bar(breaks = c(0,20), size = 0.8, position= c(0.8, 0.0)) +
  tm_legend(position = c(-0.2, 0)) + 
  tm_layout(main.title = "Greater Atlanta Metro Area (2011-2015)", main.title.position = "center", 
            frame = FALSE)
  # tm_credits("*Pollution data from EPA Discharge Monitoring Report for 2011-2015; Segregation data from ACS-5YR 2011-2015",
  #            position = c("RIGHT","BOTTOM"), size = 14)
raceseg_map

tiff("figures/dmr_race_seg_map2011-15.tif", res = 300, units = "in", 
     height = 7.5, width = 10, compression = "lzw")
raceseg_map
dev.off()


###################################################################
## map ARC INCOME DIVERSITY data with watersheds and DMR overlaid
## still working out clipping of huc to ARC region
###################################################################

## create custom palette with custom legend labels for seg indices
col2 <- c("white", "#ff9900", "#66cc00",  
         "#ffcc99", "#99ff99", "#ff9999", "#cc99ff",
         "#99752e")
leg_col2 <- c("#ff9900", "#66cc00",  
             "#ffcc99", "#99ff99", "#ff9999", "#cc99ff",
             "#99752e")
lbl2 <- c("Low (Low-Income)", "Low (Low-Middle)",
         "Mod (Low-Income)", "Mod (Low-Middle)", "Mod (Upper-Middle)", "Mod (Upper)", 
         "High Diversity")

## mapping
inc_map <- 
  tm_shape(huc) +
  tm_borders(col = "white") +
  tm_shape(seg) +
  tm_fill("i15c", legend.show = FALSE, palette = col2) +
  tm_add_legend(type = c("fill"), labels = lbl2, col = leg_col2, 
                title = "Income Diversity\n(by dominant group)") +
  tm_shape(huc) +
  tm_borders(col = "black") +
  tm_shape(dmr) + 
  tm_bubbles(size = "dmr_area", col = "black", scale = 2, title.size = "Pollution (kg/yr/km^2)",
             size.lim = c(0,1.5e3), sizes.legend = c(10, 100, 250, 500, 750, 1000)) + 
  tm_compass(type = "arrow", size = 2, position = c(0.83, 0.06)) +
  tm_scale_bar(breaks = c(0,20), size = 0.8, position= c(0.8, 0.0)) +
  tm_legend(position = c(-0.2, 0)) + 
  tm_layout(main.title = "Greater Atlanta Metro Area (2011-2015)", main.title.position = "center", 
            frame = FALSE)
# tm_credits("*Pollution data from EPA Discharge Monitoring Report for 2011-2015; Segregation data from ACS-5YR 2011-2015",
#            position = c("RIGHT","BOTTOM"), size = 14)
inc_map

tiff("figures/dmr_income_seg_map2011-15.tif", res = 300, units = "in", 
     height = 7.5, width = 10, compression = "lzw")
inc_map
dev.off()


###################################################################
## still working on things below here
## graphing pollution rates by racial diversity at watershed scale
###################################################################

## 
seg_huc <- st_intersection(huc, seg)
qtm(seg_huc)

# ha.test <- st_intersection(st_union(huc), st_union(arc.shp))
# qtm(ha.test)

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
fig <- ggplot(hpr.sum, aes(nwnl_prc, dmr_areasum/1000)) +
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


