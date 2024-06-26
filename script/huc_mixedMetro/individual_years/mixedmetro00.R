##############################################################################
# mixed metro data for HUC 10s and HUC 12s - 2000
# created: May 5, 2018
# Author: Taylor Hafley
# git: deanhardy/se_segregation
# local location: Dropbox/school/Projects/inProgress/watershed/se_segregation/
##############################################################################

# note, this is copied from mixedmetro.com. In this script, I condense and organize
# the steps and operations in a more straight-forward, and clear example.

# load necessary libraries
library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
library(sf)
library(tmap)
library(tmaptools)
library(lwgeom)

# race data from decennial census needed for Mixed Metro classifications
# [note: latinx, 'P0040003', = total - nonlatinx] #tested and confirmed
# six racial groups: White, Black, American Indian, Asian or Pacific Islanders, 
#                           Some other race & two or more races, Latinx
dec_vars00sf3 <- c(total = "P007001",
                nonlatinx = "P007002",
                white = "P007003", black = "P007004",
                native_american = "P007005", asian = "P007006",
                hawaiian = "P007007", other = "P007008",
                twomore = "P007009", latinx = "P007010")

# vector of Atlanta HUC counties (need county in call for block group data
# using tidycensus (get_decennial/acs))
cnty <- c("Baldwin","Banks","Barrow","Bartow","Butts","Carroll","Cherokee","Clarke",
          "Clayton","Cobb", "Coweta", "Dawson", "DeKalb", "Douglas","Fann",
          "Fayette","Floyd","Forsyth", "Fulton","Gordon","Gwinnett","Habersham",
          "Hall","Haralson","Heard", "Henry","Jackson","Jasper","Jones","Lamar",
          "Lincoln","Lumpkin","Meriwether","Monroe","Morgan","Newton",
          "Paulding","Pickens","Pike","Putnam","Polk","Rabun",
          "Rockdale","Spalding","Towns","Union","Upson","Walton","White")

# import 2000 race data using tidycensus
gaBG <- get_decennial(geography = "block group", variables = dec_vars00sf3, 
                      state = "GA", county = cnty, year = 2000,
                      output = 'wide',
                      geometry = TRUE) %>%
  mutate(SqKM_BG = as.numeric(st_area(geometry)) / 1e6) %>%
  mutate(ahpi = asian + hawaiian,
         oth2 = other + twomore)

# test accuracy of count data
gaBG %>%
  mutate(tot1 = (latinx + nonlatinx) - total,
         tot2 = (white + black + native_american + ahpi + oth2 + latinx ) - total) %>%
  select(total, latinx,nonlatinx, tot2, tot1, GEOID) %>%
  arrange(desc(tot2))

gaBG %>%
  group_by(GEOID) %>%
  summarize(checktotal = 1,
          checktotal = ((white + black + native_american + ahpi + oth2 + latinx) - total)) %>%
  arrange(desc(checktotal))

########################################
# Atlanta urban area
atl <-urban_areas('2010') %>%
  filter(NAME10 == 'Atlanta, GA') %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")

## import watershed data based on manual extraction
nhd1 <- st_read("data/spatial/nhd/nhd0307_huc10.shp") 
nhd2 <- st_read("data/spatial/nhd/nhd0313_huc10.shp")
nhd3 <- st_read("data/spatial/nhd/nhd0315_huc10.shp")
nhd4 <- rbind(nhd1, nhd2)
nhd <- rbind(nhd4, nhd3) %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")

rm(nhd1,nhd2,nhd3,nhd4)

## returns all watersheds that intersect urban area via indexing
huc10 <- nhd[atl,]

######################################################################
## calculate area & percent of each BG in each HUC to set up        #
## proportional allocation method                                   #

ga2 <- gaBG %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")

# returns all block groups  that intersect urban huc10s (via indexing)
huc10_bg00 <- ga2[huc10,]

# identify sub-geographies that intersect watershed boundary
int2 <- st_intersection(huc10_bg00, huc10)

bg00_huc10 <- int2 %>%
  mutate(BG_SqKmHuc = round(as.numeric((st_area(geometry) / 1e6)),4)) %>%
  mutate(pct_BGinHUC = round(BG_SqKmHuc/SqKM_BG,4))

bg00_huc10d <- bg00_huc10 %>%
  group_by(HUC10) %>%
  summarise(total = sum(total*pct_BGinHUC),
            white = sum(white*pct_BGinHUC),
            black = sum(black*pct_BGinHUC),
            native_american = sum(native_american*pct_BGinHUC),
            ahpi = sum(ahpi*pct_BGinHUC),
            oth2 = sum(oth2*pct_BGinHUC),
            latinx = sum(latinx*pct_BGinHUC),
            whtpct00 = round((white/total), 4),
            blkpct00 = round((black/total), 4),
            napct00 = round((native_american/total),4),
            ahpipct00 = round((ahpi/total), 4),
            othpct00 = round((oth2/total), 4),
            ltxpct00 = round((latinx/total), 4))

bg00_huc10d %>%
  group_by(HUC10) %>%
    summarize(checktotal = 1,
            checktotal = sum(white + black + native_american + ahpi + oth2 + latinx) - total) %>%
  arrange(desc(checktotal))

# verify that percentages == 1.
bg00_huc10d %>%
  mutate(one = whtpct00 + blkpct00 + napct00 + ahpipct00 + othpct00 + ltxpct00) %>%
  select(HUC10, total, one) %>%
  arrange(one)

bg00_huc10d %>%
  mutate(one = whtpct00+blkpct00+napct00+ahpipct00+othpct00+ltxpct00) %>%
  select(HUC10, total, one) %>%
  arrange(desc(one))

###########
# 2000 BG entropy calculation

bg00_huc10d <- bg00_huc10d %>%
  mutate(E = -((whtpct00*log(whtpct00)+blkpct00*log(blkpct00) + napct00*log(napct00) +
                  ahpipct00*log(ahpipct00) + othpct00*log(othpct00) +
                  ltxpct00*log(ltxpct00)))/log(6))

##############################################################
##### assign 'classes' for segregation X diversity - RACE ##
##############################################################

bg00_class00_huc10 <- bg00_huc10d %>%
  mutate(class00 = 1,
         class00 = replace(class00, E <= .3707 & whtpct00 > .65 | whtpct00 >=.8, 2),
         class00 = replace(class00, E <= .3707 & blkpct00 > .65 | blkpct00 >= .8, 3),
         class00 = replace(class00, E > .7414 & whtpct00 < .46 & blkpct00 < .46, 14),
         class00 = replace(class00, E > .3707 & E < .7414 & whtpct00 < .8 & whtpct00 > blkpct00 & whtpct00 > ltxpct00,8),
         class00 = replace(class00, whtpct00 >.46 & whtpct00 <.8, 8),
         class00 = replace(class00, E > .3707 & E < .7414 & blkpct00 < .8 & blkpct00 > whtpct00 & blkpct00 > ltxpct00,9),
         class00 = replace(class00, blkpct00 >.46 & blkpct00 <.8,9),
         class00 = replace(class00, E > .3707 & E < .7414 & ltxpct00 < .8 & ltxpct00 > whtpct00 & ltxpct00 > blkpct00,13),
         class00 = replace(class00, E <= .3707 & ltxpct00 > .65 | ltxpct00 >= .8, 7),
         class00 = replace(class00, ltxpct00 >.46 & ltxpct00 <.8, 13),
         class00 = replace(class00, E <= .3707 & othpct00 > .65 | othpct00 >= .8, 4),
         class00 = replace(class00, E <= .3707 & napct00 > .65 | napct00 >= .8, 5))

bg00_class00_huc10$class00 <- bg00_class00_huc10$class00 %>% as.factor()

tm_shape(bg00_class00_huc10) +
  tm_polygons('class00')

#########################################
#  BGs to HUC 12s
#########################################
#####################
## HUC12s processing
#####################

## import watershed data based on manual extraction
nhd1 <- st_read("data/spatial/nhd/nhd0307_huc12.shp") 
nhd2 <- st_read("data/spatial/nhd/nhd0313_huc12.shp")
nhd3 <- st_read("data/spatial/nhd/nhd0315_huc12.shp")
nhd4 <- rbind(nhd1, nhd2)
nhd <- rbind(nhd4, nhd3) %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")

rm(nhd1, nhd2, nhd3, nhd4)
## returns all watersheds contained by huc10s
huc12 <- st_join(huc10, nhd, join = st_contains)
huc12 <- nhd %>%
  filter(HUC12 %in% huc12$HUC12)

######################################################################
## calculate area & percent of each BG in each HUC to set up        ##
## proportional allocation method                                   ##
######################################################################

# applying Dean work to census tracts/block group (sub-geography)
ga3 <- gaBG %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")

# returns all block groups  that intersect urban huc10s (via indexing)
huc12_bg <- ga3[huc10,]

# identify sub-geographies that intersect watershed boundary
int3 <- st_intersection(huc12_bg, huc12)

bg00_huc12 <- int3 %>%
  mutate(BG_SqKmHuc = round(as.numeric((st_area(geometry) / 1e6)),4)) %>%
  mutate(pct_BGinHUC = round(BG_SqKmHuc/SqKM_BG,4))

bg00_huc12d <- bg00_huc12 %>%
  group_by(HUC12) %>%
  summarise(total = sum(total*pct_BGinHUC),
            white = sum(white*pct_BGinHUC),
            black = sum(black*pct_BGinHUC),
            native_american = sum(native_american*pct_BGinHUC),
            ahpi = sum(ahpi*pct_BGinHUC),
            oth2 = sum(oth2*pct_BGinHUC),
            latinx = sum(latinx*pct_BGinHUC),
            whtpct00 = round((white/total), 4),
            blkpct00 = round((black/total), 4),
            napct00 = round((native_american/total),4),
            ahpipct00 = round((ahpi/total), 4),
            othpct00 = round((oth2/total), 4),
            ltxpct00 = round((latinx/total), 4))

bg00_huc12d %>%
  group_by(HUC12) %>%
  summarize(checktotal = 1,
            checktotal = sum(white + black + native_american + ahpi + oth2 + latinx) - total) %>%
  arrange((checktotal))

# verify that percentages == 1.
bg00_huc12d %>%
  mutate(one = whtpct00 + blkpct00 + napct00 + ahpipct00 + othpct00 + ltxpct00) %>%
  select(HUC12, total, one) %>%
  arrange(one)

bg00_huc12d %>%
  mutate(one = whtpct00+blkpct00+napct00+ahpipct00+othpct00+ltxpct00) %>%
  select(HUC12, total, one) %>%
  arrange(desc(one))

########################################################
#####    2000 BG -- HUC 12 entropy calculation       ###
########################################################

emm <- bg00_huc12d %>%
  mutate(E = -((whtpct00*log(whtpct00)+blkpct00*log(blkpct00) + napct00*log(napct00) +
                  ahpipct00*log(ahpipct00) + othpct00*log(othpct00) +
                  ltxpct00*log(ltxpct00)))/log(6))

###########################################################
##### assign 'classes' for segregation X diversity -     ##
###########################################################

bg00_class00_huc12 <- emm %>%
  mutate(class00 = 1,
         class00 = replace(class00, E <= .3707 & whtpct00 > .65 | whtpct00 >=.8, 2),
         class00 = replace(class00, E <= .3707 & blkpct00 > .65 | blkpct00 >= .8, 3),
         class00 = replace(class00, E > .7414 & whtpct00 < .46 & blkpct00 < .46, 14),
         class00 = replace(class00, E > .3707 & E < .7414 & whtpct00 < .8 & whtpct00 > blkpct00 & whtpct00 > ltxpct00,8),
         class00 = replace(class00, whtpct00 >.46 & whtpct00 <.8, 8),
         class00 = replace(class00, E > .3707 & E < .7414 & blkpct00 < .8 & blkpct00 > whtpct00 & blkpct00 > ltxpct00,9),
         class00 = replace(class00, blkpct00 >.46 & blkpct00 <.8,9),
         class00 = replace(class00, E > .3707 & E < .7414 & ltxpct00 < .8 & ltxpct00 > whtpct00 & ltxpct00 > blkpct00,13),
         class00 = replace(class00, E <= .3707 & ltxpct00 > .65 | ltxpct00 >= .8, 7),
         class00 = replace(class00, ltxpct00 >.46 & ltxpct00 <.8, 13),
         class00 = replace(class00, E <= .3707 & othpct00 > .65 | othpct00 >= .8, 4),
         class00 = replace(class00, E <= .3707 & napct00 > .65 | napct00 >= .8, 5))

bg00_class00_huc12$class00 <- bg00_class00_huc12$class00 %>% as.factor()

tm_shape(bg00_class00_huc12) +
  tm_polygons('class00')

##################################################################
## map RACE diversity/segregation by HUC 12                   ###
##################################################################

## create custom palette with custom legend labels for seg indices
race_mm_col <- c("#ff9900","#66cc00","#ffcc99", "#99ff99","#99752e")
leg_col2 <- c("#ff9900","#66cc00","#ffcc99", "#99ff99","#99752e")
lbl2 <- c("White (Low)","Black (Low)","White (Mod)",
          "Black (Mod)","High Diversity")
rd <- primary_roads(year = 2016)

huc12 <- 
  tm_shape(bg00_class00_huc12) +
  tm_fill('class00', legend.show = FALSE, palette = race_mm_col)+
  tm_add_legend(type = c("fill"), labels = lbl2, col = leg_col2, 
                title = "2000 seg & diversity\nby HUC12 Watershed") +
  tm_shape(rd) + 
  tm_lines(col = "black") +
  tm_compass(type = "arrow", size = 4, position = c(0.82, 0.08)) +
  tm_scale_bar(breaks = c(0,20), size = 1.1, position= c(0.8, 0.0)) +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = .9,
            legend.title.size = 1.1) + 
  tm_layout(frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=0)
huc12

tmap_mode('plot')
## create custom palette with custom legend labels for seg indices
race_mm_col3 <- c("#ff9900", "#ffcc99", "#99ff99")
leg_col3 <- c("#ff9900","#ffcc99", "#99ff99")
lbl3 <- c("White (Low)","White (Mod)","Black (Mod)")

# here is where I need to make new map
huc10 <- 
  tm_shape(bg00_class00_huc10) +
  tm_fill('class00', legend.show = FALSE, palette = race_mm_col3)+
  tm_add_legend(type = c("fill"), labels = lbl3, col = leg_col3, 
                title = "2000 seg & diversity\nby HUC10 Watershed") +
  tm_shape(rd) + 
  tm_lines(col = "black") +
  tm_compass(type = "arrow", size = 4, position = c(0.82, 0.08)) +
  tm_scale_bar(breaks = c(0,20), size = 1.1, position= c(0.8, 0.0)) +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = .9,
            legend.title.size = 1.1) + 
  tm_layout(frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=0)
huc10


tapply(bg00_class00_huc10$class00, bg00_class00_huc10$class00, length)
tapply(bg00_class00_huc12$class00, bg00_class00_huc12$class00, length)

huc1200 <- ggplot(data = bg00_class00_huc12) +
  geom_bar(mapping = aes(x = class00, fill = class00
  )) +
  ggtitle("2000 segregtion & diversity\nby huc12 watershed") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    x = "segregation and diversity",
    y = 'Total Number of huc12s') 

rdkb2 <- huc1200 + scale_fill_manual(values = race_mm_col,
                                     limits = c("2","3","8","9","14"),
                                     name = "race, diversity",
                                     labels = lbl2)
rdkb2


#write_sf(bg00_class00_huc10, "data/spatial/"

bg00_class00_huc10
bg00_class00_huc12