##############################################################################
# mixed metro data for HUC 10s and HUC 12s
# created: May 5, 2018
# Author: Taylor Hafley
# git: deanhardy/se_segregation
# local location: Dropbox/school/Projects/inProgress/watershed/se_segregation/
##############################################################################

# load necessary libraries
library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
library(sf)
library(tmap)
library(tmaptools)

# race data from decennial census needed for Mixed Metro classifications
# six racial groups: White, Black, American Indian, Asian or Pacific Islanders, 
#                           Some other race & two or more races, Latinx
# [note: latinx, 'P0040003', = total - nonlatinx]
dec_vars <- c(total = "P0050001",
              nonlatinx = "P0050002",
              white = "P0050003", black = "P0050004",
              native_american = "P0050005", asian = "P0050006",
              hawaiian = "P0050007", other = "P0050008",
              twomore = "P0050009", latinx = "P0040003"
              )

# repeat above. Block Groups to huc10

# vector of Atlanta HUC counties
cnty <- c("Baldwin","Banks","Barrow","Bartow","Butts","Carroll","Cherokee","Clarke",
          "Clayton","Cobb", "Coweta", "Dawson", "DeKalb", "Douglas","Fann",
          "Fayette","Floyd","Forsyth", "Fulton","Gordon","Gwinnett","Habersham",
          "Hall","Haralson","Heard", "Henry","Jackson","Jasper","Jones","Lamar",
          "Lincoln","Lumpkin","Meriwether","Monroe","Morgan","Newton",
          "Paulding","Pickens","Pike","Putnam","Polk","Rabun",
          "Rockdale","Spalding","Towns","Union","Upson","Walton","White")

# import race data using tidycensus
gaBG <- get_decennial(geography = "block group", variables = dec_vars, 
                      state = "GA", county = cnty, year = 2010,
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

###############################################
## calculate area & percent of each BG in each HUC to set up
## proportional allocation method

gabg10 <- gaBG %>%
         st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")

# returns all block groups  that intersect urban huc10s (via indexing)
huc10_bg10 <- gabg10[huc10,]

# identify sub-geographies that intersect watershed boundary
int2 <- st_intersection(huc10_bg10, huc10)

huc10_bg10 <- int2 %>%
  mutate(BG_SqKmHuc = round(as.numeric((st_area(geometry) / 1e6)),4)) %>%
  mutate(pct_BGinHUC = round(BG_SqKmHuc/SqKM_BG,4))

huc10_bg10 <- huc10_bg10 %>%
  group_by(HUC10) %>%
  summarise(total = sum(total*pct_BGinHUC),
            white = sum(white*pct_BGinHUC),
            black = sum(black*pct_BGinHUC),
            native_american = sum(native_american*pct_BGinHUC),
            ahpi = sum(ahpi*pct_BGinHUC),
            oth2 = sum(oth2*pct_BGinHUC),
            latinx = sum(latinx*pct_BGinHUC),
            whtpct10 = round((white/total), 4),
            blkpct10 = round((black/total), 4),
            napct10 = round((native_american/total),4),
            ahpipct10 = round((ahpi/total), 4),
            othpct10 = round((oth2/total), 4),
            ltxpct10 = round((latinx/total), 4))

huc10_bg10 %>%
  group_by(HUC10) %>%
  summarize(checktotal = 1,
            checktotal = sum(white + black + native_american + ahpi + oth2 + latinx) - total) %>%
  arrange(desc(checktotal))

huc10_bg10 %>%
  mutate(one = whtpct10 + blkpct10 + napct10 + ahpipct10 + othpct10 + ltxpct10) %>%
  select(HUC10, total, one) %>%
  arrange(one)

huc10_bg10 %>%
  mutate(one = whtpct10 + blkpct10 + napct10 + ahpipct10 + othpct10 + ltxpct10) %>%
  select(HUC10, total, one) %>%
  arrange(desc(one))


##    2010 BG entropy calculation

huc10_bg10 <- huc10_bg10 %>%
  mutate(E = -((whtpct10*log(whtpct10)+blkpct10*log(blkpct10) + napct10*log(napct10) +
                  ahpipct10*log(ahpipct10) + othpct10*log(othpct10) +
                  ltxpct10*log(ltxpct10)))/log(6))


##### assign 'classes' for segregation X diversity 

huc10_bg10 <- huc10_bg10 %>%
  mutate(class10 = 1,
         class10 = replace(class10, E <= .3707 & whtpct10 > .65 | whtpct10 >=.8, 2),
         class10 = replace(class10, E <= .3707 & blkpct10 > .65 | blkpct10 >= .8, 3),
         class10 = replace(class10, E > .7414 & whtpct10 < .46 & blkpct10 < .46, 14),
         class10 = replace(class10, E > .3707 & E < .7414 & whtpct10 < .8 & whtpct10 > blkpct10 & whtpct10 > ltxpct10,8),
         class10 = replace(class10, whtpct10 >.46 & whtpct10 <.8, 8),
         class10 = replace(class10, E > .3707 & E < .7414 & blkpct10 < .8 & blkpct10 > whtpct10 & blkpct10 > ltxpct10,9),
         class10 = replace(class10, blkpct10 >.46 & blkpct10 <.8,9),
         class10 = replace(class10, E > .3707 & E < .7414 & ltxpct10 < .8 & ltxpct10 > whtpct10 & ltxpct10 > blkpct10,13),
         class10 = replace(class10, E <= .3707 & ltxpct10 > .65 | ltxpct10 >= .8, 7),
         class10 = replace(class10, ltxpct10 >.46 & ltxpct10 <.8, 13),
         class10 = replace(class10, E <= .3707 & othpct10 > .65 | othpct10 >= .8, 4),
         class10 = replace(class10, E <= .3707 & napct10 > .65 | napct10 >= .8, 5))

huc10_bg10$class10 <- huc10_bg10$class10 %>% as.factor()

tm_shape(huc10_bg10) +
  tm_polygons('class10')

## create custom palette with custom legend labels for seg indices
race_mm_col3 <- c("#ff9900","#ffcc99", "#99ff99","#99752e")
leg_col3 <- c("#ff9900","#ffcc99", "#99ff99","#99752e")
lbl3 <- c("White (Low)","White (Mod)","Black (Mod)","High Diversity")

rd <- primary_roads(year = 2016)

# here is where I need to make new map
huc10 <- 
  tm_shape(huc10_bg10) +
  tm_fill('class10', legend.show = FALSE, palette = race_mm_col3)+
  tm_add_legend(type = c("fill"), labels = lbl3, col = leg_col3, 
                title = "2010 seg & diversity\nby HUC10 Watershed") +
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

#bg_class10_huc10$class10



