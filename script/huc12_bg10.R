##############################################################################
# mixed metro data for HUC 12s - 2010
# created: May 5, 2018
# Last updated: July 3, 2018
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


#########################################
#  BGs to HUC 12s

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


## calculate area & percent of each BG in each HUC to set up
## proportional allocation method 

gabg10 <- gaBG %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")

# returns all block groups  that intersect urban huc10s (via indexing)
huc12_bg10 <- gabg10[huc10,]

# identify sub-geographies that intersect watershed boundary
int3 <- st_intersection(huc12_bg10, huc12)

huc12_bg10 <- int3 %>%
  mutate(BG_SqKmHuc = round(as.numeric((st_area(geometry) / 1e6)),4)) %>%
  mutate(pct_BGinHUC = round(BG_SqKmHuc/SqKM_BG,4))

huc12_bg10 <- huc12_bg10 %>%
  group_by(HUC12) %>%
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

huc12_bg10 %>%
  group_by(HUC12) %>%
  summarize(checktotal = 1,
            checktotal = sum(white + black + native_american + ahpi + oth2 + latinx) - total) %>%
  arrange((checktotal))

huc12_bg10 %>%
  mutate(one = whtpct10 + blkpct10 + napct10 + ahpipct10 + othpct10 + ltxpct10) %>%
  select(HUC12, total, one) %>%
  arrange(one)

huc12_bg10 %>%
  mutate(one = whtpct10 + blkpct10 + napct10 + ahpipct10 + othpct10 + ltxpct10) %>%
  select(HUC12, total, one) %>%
  arrange(desc(one))

##   2010 BG -- HUC 12 entropy calculation

huc12_bg10 <- huc12_bg10 %>%
  mutate(E = -((whtpct10*log(whtpct10)+blkpct10*log(blkpct10) + napct10*log(napct10) +
                  ahpipct10*log(ahpipct10) + othpct10*log(othpct10) +
                  ltxpct10*log(ltxpct10)))/log(6)) %>%
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

huc12_bg10$class10 <- huc12_bg10$class10 %>% as.factor()

## map RACE diversity/segregation by HUC 12 

## create custom palette with custom legend labels for seg indices
race_mm_col <- c("#ff9900","#66cc00","#ffcc99", "#99ff99", "#cc99ff","#99752e")
leg_col2 <- c("#ff9900","#66cc00","#ffcc99", "#99ff99", "#cc99ff","#99752e")
lbl2 <- c("White (Low)","Black (Low)","White (Mod)",
          "Black (Mod)","Latinx (Mod)","High Diversity")

rd <- primary_roads(year = 2016)

huc12 <- 
  tm_shape(huc12_bg10) +
  tm_fill('class10', legend.show = FALSE, palette = race_mm_col)+
  tm_add_legend(type = c("fill"), labels = lbl2, col = leg_col2, 
                title = "2010 seg & diversity\nby HUC12 Watershed") +
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



tapply(huc12_bg10$class10, huc12_bg10$class10, length)

huc1210 <- ggplot(data = huc12_bg10) +
  geom_bar(mapping = aes(x = class10, fill = class10
  )) +
  ggtitle("2010 segregtion & diversity\nby huc10 watershed") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    x = "segregation and diversity",
    y = 'Total Number of huc10s') 

rdkb2 <- huc1210 + scale_fill_manual(values = race_mm_col,
                                     limits = c("2","3","8","9","13","14"),
                                     name = "race, diversity",
                                     labels = lbl2)
rdkb2
