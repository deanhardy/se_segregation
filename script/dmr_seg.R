rm(list=ls())

##then packages are loaded into the R environment
library(tidyverse)
library(tmap)
library(sf)
library(ggthemes)
# library(dmm)

##import HUC10 data
huc <- st_read("data/spatial/huc10.shp") %>%
  st_transform(4269)

## import segregation/diversity data from 2011-2015 ACS
seg <- st_read("data/geojson/arc15.geojson") %>%
  mutate(rd15c = as.factor(rd15c)) %>%
  st_transform(4269)

## import DMR data downloaded from internet for HUC03 and filter to GA and make spatial
## https://echo.epa.gov/trends/loading-tool/get-data/custom-search
dmr11 <- read.csv("data/dmr/dmr_2011_huc03.csv", skip = 4)
dmr12 <- read.csv("data/dmr/dmr_2012_huc03.csv", skip = 4)
dmr13 <- read.csv("data/dmr/dmr_2013_huc03.csv", skip = 4)
dmr14 <- read.csv("data/dmr/dmr_2014_huc03.csv", skip = 4)
dmr15 <- read.csv("data/dmr/dmr_2015_huc03.csv", skip = 4)

## combine dmr data & clip to Georgia
dmr <- rbind(dmr11, dmr12, dmr13, dmr14, dmr15) %>%
  filter(State == "GA") %>%
  st_as_sf(coords = c("Facility.Longitude", "Facility.Latitude"), crs = 4269)

## extract pollution load data from dmr and summarize by facility
dmr.sum <- dmr %>%
  group_by(NPDES.Permit.Number) %>%
  summarise(sum = sum(Pollutant.Load..kg.yr., na.rm = TRUE))

# seg_huc <- st_intersection(huc, seg)
# qtm(seg_huc)

## union ARC race data with DMR summed data
huc.poll <- st_intersection(huc, poll)
qtm(huc.poll)

ha.test <- st_intersection(st_union(huc), st_union(arc.shp))
qtm(ha.test)

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


