##############################################################################
# mixed metro data for HUC 10s and HUC 12s - 1990
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

# read in Data for 1990 from NHGIS
# STF3 
gabg90 <- read_csv("data/data_share/nhgis0059_ds123_1990_blck_grp.csv")

# clean up tbl. cut  uneccessary, blank, and repetitive columns
gabg90 <- gabg90[,-(c(3:11,14:20,22:26))]

# rename and create new variables
gabg90 <- gabg90 %>%
  rename(white = "E1A001",
         black = "E1A002",
         native_american = "E1A003",
         asian = "E1A004",
         other = "E1A005",
         h_white = "E1A006",
         h_black = "E1A007",
         h_native_american = "E1A008",
         h_asian = "E1A009",
         h_other = "E1A010") %>%
  mutate(latinx = (h_white + h_black + h_native_american + h_asian + h_other),
         nlatinx = (white + black + native_american + asian + other),
         total = (white + black + native_american + asian + other + latinx)  
         ) 

# test accuracy of count data
gabg90 %>%
  mutate(tot1 = (latinx + nlatinx) - total,
         tot2a = (white + black + native_american + asian + other + latinx) - total) %>%
  select(total, latinx, nlatinx, tot2a, tot1) %>%
  arrange(desc(tot2a))

gabg90 %>%
  group_by(GISJOIN) %>%
  summarize(checktotal = 1,
            checktotal = ((white + black + native_american + asian + other + latinx) - total)) %>%
  arrange((checktotal))

# read in shapefile of GA block groups
ga_sp <- st_read("data/data_share/GA_blck_grp_1990.shp")
gabg90 <- left_join(ga_sp, gabg90)


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

## returns all watersheds that intersect atlanta urban area via indexing
huc10 <- nhd[atl,]


## calculate area & percent of each BG in each HUC to set up
## proportional allocation method
gabg90 <- gabg90 %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")
gabg90 <- gabg90 %>%
  mutate(SqKM_BG = as.numeric(st_area(geometry)) / 1e6)

# returns all block groups  that intersect urban huc10s (via indexing)
huc10_bg90 <- gabg90[huc10,]

# map for fun :)
tm_shape(huc10_bg90) +
  tm_polygons('black') +
  tm_shape(huc10) +
  tm_borders(col='blue')
#tmap_mode("view")

# identify sub-geographies that intersect watershed boundary
int2 <- st_intersection(huc10_bg90, huc10)

huc10_bg90 <- int2 %>%
  mutate(BG_SqKmHuc = round(as.numeric((st_area(geometry) / 1e6)),4)) %>%
  mutate(pct_BGinHUC = round(BG_SqKmHuc/SqKM_BG,4))

huc10_bg90 <- huc10_bg90 %>%
  group_by(HUC10) %>%
  summarise(total = sum(total*pct_BGinHUC),
            white = sum(white*pct_BGinHUC),
            black = sum(black*pct_BGinHUC),
            native_american = sum(native_american*pct_BGinHUC),
            ahpi = sum(asian*pct_BGinHUC),
            oth2 = sum(other*pct_BGinHUC),
            latinx = sum(latinx*pct_BGinHUC),
            whtpct90 = round((white/total), 4),
            blkpct90 = round((black/total), 4),
            napct90 = round((native_american/total),4),
            ahpipct90 = round((ahpi/total), 4),
            othpct90 = round((oth2/total), 4),
            ltxpct90 = round((latinx/total), 4))

# verify total
huc10_bg90 %>%
  group_by(HUC10) %>%
  summarize(checktotal = 1,
            checktotal = sum(white + black + native_american + ahpi + oth2 + latinx) - total) %>%
  arrange((checktotal))

# verify that percentages == 1.
huc10_bg90 %>%
  mutate(one = whtpct90 + blkpct90 + napct90 + ahpipct90 + othpct90 + ltxpct90) %>%
  select(HUC10, total, one) %>%
  arrange(one)

huc10_bg90 %>%
  mutate(one = whtpct90 + blkpct90 + napct90 + ahpipct90 + othpct90 + ltxpct90) %>%
  select(HUC10, total, one) %>%
  arrange((one))


# 1990 BG entropy calculation
huc10_bg90 <- huc10_bg90 %>%
  mutate(E = -((whtpct90*log(whtpct90)+blkpct90*log(blkpct90) + napct90*log(napct90) +
                  ahpipct90*log(ahpipct90) + othpct90*log(othpct90) +
                  ltxpct90*log(ltxpct90)))/log(6))


# assign 'classes' for segregation X diversity
huc10_bg90 <- huc10_bg90 %>%
  mutate(class90 = 1,
         class90 = replace(class90, E <= .3707 & whtpct90 > .65 | whtpct90 >=.8, 2),
         class90 = replace(class90, E <= .3707 & blkpct90 > .65 | blkpct90 >= .8, 3),
         class90 = replace(class90, E > .7414 & whtpct90 < .46 & blkpct90 < .46, 14),
         class90 = replace(class90, E > .3707 & E < .7414 & whtpct90 < .8 & whtpct90 > blkpct90 & whtpct90 > ltxpct90,8),
         class90 = replace(class90, whtpct90 >.46 & whtpct90 <.8, 8),
         class90 = replace(class90, E > .3707 & E < .7414 & blkpct90 < .8 & blkpct90 > whtpct90 & blkpct90 > ltxpct90,9),
         class90 = replace(class90, blkpct90 >.46 & blkpct90 <.8,9),
         class90 = replace(class90, E > .3707 & E < .7414 & ltxpct90 < .8 & ltxpct90 > whtpct90 & ltxpct90 > blkpct90,13),
         class90 = replace(class90, E <= .3707 & ltxpct90 > .65 | ltxpct90 >= .8, 7),
         class90 = replace(class90, ltxpct90 >.46 & ltxpct90 <.8, 13),
         class90 = replace(class90, E <= .3707 & othpct90 > .65 | othpct90 >= .8, 4),
         class90 = replace(class90, E <= .3707 & napct90 > .65 | napct90 >= .8, 5))

huc10_bg90$class90 <- huc10_bg90$class90 %>% as.factor()

tm_shape(huc10_bg90) +
  tm_polygons('class90')

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
ga3 <- ga2 %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")

# returns all block groups that intersect urban huc10s (via indexing)
huc12_bg <- ga3[huc10,]

# identify sub-geographies that intersect watershed boundary
int3 <- st_intersection(huc12_bg, huc12)

bg90_huc12 <- int3 %>%
  mutate(BG_SqKmHuc = round(as.numeric((st_area(geometry) / 1e6)),4)) %>%
  mutate(pct_BGinHUC = round(BG_SqKmHuc/SqKM_BG,4))

bg90_huc12d <- bg90_huc12 %>%
  group_by(HUC12) %>%
  summarise(total = sum(total*pct_BGinHUC),
            white = sum(white*pct_BGinHUC),
            black = sum(black*pct_BGinHUC),
            native_american = sum(native_american*pct_BGinHUC),
            ahpi = sum(asian*pct_BGinHUC),
            oth2 = sum(other*pct_BGinHUC),
            latinx = sum(latinx*pct_BGinHUC),
            whtpct90 = round((white/total), 4),
            blkpct90 = round((black/total), 4),
            napct90 = round((native_american/total),4),
            ahpipct90 = round((ahpi/total), 4),
            othpct90 = round((oth2/total), 4),
            ltxpct90 = round((latinx/total), 4))

# testing accuracy of data
bg90_huc12d %>%
  group_by(HUC12) %>%
  summarize(checktotal = 1,
            checktotal = sum(white + black + native_american + ahpi + oth2 + latinx) - total) %>%
  arrange((checktotal))

bg90_huc12d %>%
  mutate(one = whtpct90 + blkpct90 + napct90 + ahpipct90 + othpct90 + ltxpct90) %>%
  select(HUC12, total, one) %>%
  arrange(one)

bg90_huc12d %>%
  mutate(one = whtpct90+blkpct90+napct90+ahpipct90+othpct90+ltxpct90) %>%
  select(HUC12, total, one) %>%
  arrange(desc(one))

########################################################
#####    2010 BG -- HUC 12 entropy calculation       ###
########################################################

emm <- bg90_huc12d %>%
  mutate(E = -((whtpct90*log(whtpct90)+blkpct90*log(blkpct90) + napct90*log(napct90) +
                  ahpipct90*log(ahpipct90) + othpct90*log(othpct90) +
                  ltxpct90*log(ltxpct90)))/log(6))

###########################################################
##### assign 'classes' for segregation X diversity -     ##
###########################################################

bg90_class90_huc12 <- emm %>%
  mutate(class90 = 1,
         class90 = replace(class90, E <= .3707 & whtpct90 > .65 | whtpct90 >=.8, 2),
         class90 = replace(class90, E <= .3707 & blkpct90 > .65 | blkpct90 >= .8, 3),
         class90 = replace(class90, E > .7414 & whtpct90 < .46 & blkpct90 < .46, 14),
         class90 = replace(class90, E > .3707 & E < .7414 & whtpct90 < .8 & whtpct90 > blkpct90 & whtpct90 > ltxpct90,8),
         class90 = replace(class90, whtpct90 >.46 & whtpct90 <.8, 8),
         class90 = replace(class90, E > .3707 & E < .7414 & blkpct90 < .8 & blkpct90 > whtpct90 & blkpct90 > ltxpct90,9),
         class90 = replace(class90, blkpct90 >.46 & blkpct90 <.8,9),
         class90 = replace(class90, E > .3707 & E < .7414 & ltxpct90 < .8 & ltxpct90 > whtpct90 & ltxpct90 > blkpct90,13),
         class90 = replace(class90, E <= .3707 & ltxpct90 > .65 | ltxpct90 >= .8, 7),
         class90 = replace(class90, ltxpct90 >.46 & ltxpct90 <.8, 13),
         class90 = replace(class90, E <= .3707 & othpct90 > .65 | othpct90 >= .8, 4),
         class90 = replace(class90, E <= .3707 & napct90 > .65 | napct90 >= .8, 5))

bg90_class90_huc12$class90 <- bg90_class90_huc12$class90 %>% as.factor()

tm_shape(bg90_class90_huc12) +
  tm_polygons('class90')

##################################################################
## map RACE diversity/segregation by HUC 12                   ###
##################################################################

## create custom palette with custom legend labels for seg indices
race_mm_col <- c("#ff9900","#66cc00","#ffcc99", "#99ff99")
leg_col2 <- c("#ff9900","#66cc00","#ffcc99", "#99ff99")
lbl2 <- c("White (Low)","Black (Low)","White (Mod)",
          "Black (Mod)")

rd <- primary_roads(year = 2016)

huc12 <- 
  tm_shape(bg90_class90_huc12) +
  tm_fill('class90', legend.show = FALSE, palette = race_mm_col)+
  tm_add_legend(type = c("fill"), labels = lbl2, col = leg_col2, 
                title = "1990 seg & diversity\nby HUC12 Watershed") +
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

#tmap_mode('view')
## create custom palette with custom legend labels for seg indices
race_mm_col2 <- c("#ff9900", "#ffcc99", "#99ff99")
leg_col3 <- c("#ff9900","#ffcc99", "#99ff99")
lbl3 <- c("White (Low)","White (Mod)","Black (Mod)")

# here is where I need to make new map
huc10 <- 
  tm_shape(bg90_class90_huc10) +
  tm_fill('class90', legend.show = FALSE, palette = race_mm_col2)+
  tm_add_legend(type = c("fill"), labels = lbl3, col = leg_col3, 
                title = "1990 seg & diversity\nby HUC10 Watershed") +
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


huc <- tm_shape(huc10) +
  tm_borders()

tmap_mode("view")

save_tmap(huc, "huc10.html")

tm_shape(huc12) +
  tm_borders()

huc1090 <- ggplot(data = bg90_class90_huc10) +
  geom_bar(mapping = aes(x = class90, fill = class90
           )) +
  ggtitle("1990 segregtion & diversity\nby huc10 watershed") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    x = "segregation and diversity",
    y = 'Total Number of huc10s') 

rdkb2 <- huc1090 + scale_fill_manual(values = race_mm_col2,
                                  limits = c("2","8","9"),
                                  name = "race, diversity",
                                  labels = lbl3)
rdkb2

?geom_bar
race_mm_col2 <- c("#ff9900", "#ffcc99", "#99ff99")
leg_col3 <- c("#ff9900","#ffcc99", "#99ff99")
lbl3 <- c("White (Low)","White (Mod)","Black (Mod)")

race_mm_col <- c("#ff9900","#66cc00","#ffcc99", "#99ff99")
leg_col2 <- c("#ff9900","#66cc00","#ffcc99", "#99ff99")
lbl2 <- c("White (Low)","Black (Low)","White (Mod)",
          "Black (Mod)")
