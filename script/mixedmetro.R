# mixed metro data for HUC 10s
# created: May 5, 2018
# Author: Taylor Hafley
# git: deanhardy/se_segregation
# local location: desktop/school/sideProjects/segXdiversity/se_segregation/


# load necessary libraries
library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
library(sf)
library(tmap)
library(tmaptools)

th_api_acs <- '45544f0d114cfaa037a5566745d18bb8d4778cfa'

v10 <- load_variables(2010, "sf1", cache = TRUE)
View(v10)

# race data from decennial census needed for Mixed Metro classifications
# six racial groups: White, Black, American Indian, Asian or Pacific Islanders, 
#                           Some other race & two or more races, Latinx
# [note: latinx, 'P0040003', = total - nonlatinx] #tested and confirmed
dec_vars <- c(total = "P0050001",
              nonlatinx = "P0050002",
              white = "P0050003", black = "P0050004",
              native_american = "P0050005", asian = "P0050006",
              hawaiian = "P0050007", other = "P0050008",
              twomore = "P0050009", latinx = "P0040003"
              )

# import race data using tidycensus
ga <- get_decennial(geography = 'tract',
              variables = dec_vars,
              state = c('GA'),
              year = 2010,
              output = 'wide',
              geometry = TRUE) 

# collapse race variables for Mixed Metro
ga <- ga %>%
  mutate(ahpi = asian + hawaiian,
         oth2 = other + twomore)

tmap_mode("view")

########################################
# copied from nhd_grab

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

tm_shape(huc10) +
  tm_borders(col = 'blue') +
  tm_shape(atl) +
  tm_borders()

######################################################################
## calculate area & percent of each CT in each HUC to set up        ##
## proportional allocation method                                   ##
######################################################################

int <- as.tibble(st_intersection(atl, huc10))
int1 <- as.tibble(st_intersection(huc10, atl))
class(int)

atl_huc10 <- int %>%
  mutate(SqKmATLinHUC = as.numeric((st_area(geometry) / 1e6))) %>%
  mutate(PercATLinHUC = (SqKmATLinHUC/AreaSqKm)) %>%
  select(Name, SqKmATLinHUC, PercATLinHUC) %>%
  left_join(huc10, ., by = 'Name')

# applying Dean work to census tracts/block group (sub-geography)
ga2 <- ga %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")

## calculate area (sqKm) for each census tract 
ga2 <- ga2 %>%
  mutate(areaSqKmct = as.numeric((st_area(geometry) / 1e6)))

### returns all census tracts that intersect urban huc10s (via indexing)
#& percent of each huc in ATL urban area
huc10_ct <- ga2[huc10,]

# map for fun :)
tm_shape(huc10_ct) +
  tm_polygons('black') +
  tm_shape(huc10) +
  tm_borders(col='blue') +
  ?tm_borders

# identify sub-geographies that intersect watershed boundary
int2 <- st_intersection(huc10_ct, huc10)

ct_huc10 <- int2 %>%
  mutate(ctSqKmHuc = round(as.numeric((st_area(geometry) / 1e6)),4)) %>%
  mutate(pCTinHUC = round(ctSqKmHuc/areaSqKmct,4))

ct_huc10b <- ct_huc10 %>%
  group_by(HUC10) %>%
  mutate(tot2 = round(total*pCTinHUC,0),
         wht2 = round(white*pCTinHUC,0),
         blk2 = round(black*pCTinHUC,0))

ct_huc10c <- ct_huc10 %>%
  group_by(HUC10) %>%
  mutate(tot2 = round(total*pCTinHUC,0),
         wht2 = round(white*pCTinHUC,0),
         blk2 = round(black*pCTinHUC,0)) %>%
  summarise(total = sum(tot2),
            white = sum(wht2),
            black = sum(blk2),
            pbl = black/total)

ct_huc10d <- ct_huc10 %>%
  group_by(HUC10) %>%
  summarise(total = sum(round(total*pCTinHUC,0)),
            white = sum(round(white*pCTinHUC,0)),
            black = sum(round(black*pCTinHUC,0)),
            native_american = sum(round(native_american*pCTinHUC,0)),
            ahpi = sum(round(ahpi*pCTinHUC,0)),
            oth2 = sum(round(oth2*pCTinHUC,0)),
            latinx = sum(round(latinx*pCTinHUC,0)),
            whtpct10 = round((white/total), 4),
            blkpct10 = round((black/total), 4),
            napct10 = round((native_american/total),4),
            ahpipct10 = round((ahpi/total), 4),
            othpct10 = round((oth2/total), 4),
            ltxpct10 = round((latinx/total), 4))

# verify that percentages == 1.

verify <- ct_huc10d %>%
  mutate(one = whtpct10+blkpct10+napct10+ahpipct10+othpct10+ltxpct10)
# largest value = 1.0005
# smallest value = 0.9998

################################################
#######    2010 CT entropy calculation       ###
################################################

emm <- ct_huc10d %>%
  mutate(E = -((whtpct10*log(whtpct10)+blkpct10*log(blkpct10) + napct10*log(napct10) +
                  ahpipct10*log(ahpipct10) + othpct10*log(othpct10) +
                  ltxpct10*log(ltxpct10)))/log(6))

##############################################################
##### assign 'classes' for segregation X diversity - RACE ##
##############################################################

library(data.table)
setDT(emm)[emm$E <= .3707 & emm$whtpct10 > .65 | emm$whtpct10 >= .8,  rc := 2 ]
emm[(E <= .3707 & blkpct10 > .65) | blkpct10 >= .8,  rc := 3 ]
emm[(E > .7414 & whtpct10 < .46 & blkpct10 < .46), rc := 14]
emm[E > .3707 & E < .7414 & whtpct10 < .8 & whtpct10 > blkpct10 & whtpct10 > ltxpct10, rc := 8]
emm[is.na(rc) & whtpct10 >.46 & whtpct10 <.8, rc :=8]
emm[E > .3707 & E < .7414 & blkpct10 < .8 & blkpct10 > whtpct10 & blkpct10 > ltxpct10, rc := 9]
emm[is.na(rc) & blkpct10 >.46 & blkpct10 <.8, rc :=9]
emm[E > .3707 & E < .7414 & ltxpct10 < .8 & ltxpct10 > whtpct10 & ltxpct10 > blkpct10, rc := 13]
emm[(E <= .3707 & ltxpct10 > .65) | ltxpct10 >= .8,  rc := 7 ]
emm[is.na(rc) & ltxpct10 >.46 & ltxpct10 <.8, rc :=13]
emm[(E <= .3707 & othpct10 > .65) | othpct10 >= .8,  rc := 4 ]
emm[(E <= .3707 & napct10 > .65) | napct10 >= .8,  rc := 5 ]



class(spem)
spem <- st_as_sf(emm)

spem$rc <- spem$rc %>% as.factor()
str(spem)
tm_shape(spem) +
  tm_polygons('rc')



############################################
# repeat above. Block Groups to huc10     ##
############################################

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
  mutate(latinx = total - nonlatinx, SqKM_BG = as.numeric(st_area(geometry)) / 1e6) %>%
  dplyr::select(-nonlatinx) %>%
  mutate(ahpi = asian + hawaiian,
         oth2 = other + twomore)

tm_shape(gaBG) +
  tm_polygons('white')

########################################
# copied from nhd_grab

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

tm_shape(huc10) +
  tm_borders(col = 'blue') +
  tm_shape(atl) +
  tm_borders()

######################################################################
## calculate area & percent of each BG in each HUC to set up        ##
## proportional allocation method                                   ##
######################################################################

# applying Dean work to census tracts/block group (sub-geography)
ga2 <- gaBG %>%
         st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")

# returns all block groups  that intersect urban huc10s (via indexing)
huc10_bg <- ga2[huc10,]

# map for fun :)
tm_shape(huc10_bg) +
  tm_polygons('black') +
  tm_shape(huc10) +
  tm_borders(col='blue')
#tmap_mode("view")

# identify sub-geographies that intersect watershed boundary
int2 <- st_intersection(huc10_bg, huc10)

#bg_huc10 <- int2 %>%
#  mutate(BG_SqKmHuc = round(as.numeric((st_area(geometry) / 1e6)),4)) %>%
#  mutate(pct_BGinHUC = round(BG_SqKmHuc/SqKM_BG,4))

#bg_huc10b <- bg_huc10 %>%
#  group_by(HUC10) %>%
#  mutate(tot2 = round(total*pct_BGinHUC,0),
#         wht2 = round(white*pct_BGinHUC,0),
#         blk2 = round(black*pct_BGinHUC,0))

#bg_huc10c <- bg_huc10 %>%
#  group_by(HUC10) %>%
#  mutate(tot2 = round(total*pct_BGinHUC,0),
#         wht2 = round(white*pct_BGinHUC,0),
#         blk2 = round(black*pct_BGinHUC,0)) %>%
#  summarise(total = sum(tot2),
#            white = sum(wht2),
#            black = sum(blk2),
#            pbl = black/total)

bg_huc10d <- bg_huc10 %>%
  group_by(HUC10) %>%
  summarise(total = sum(round(total*pct_BGinHUC,0)),
            white = sum(round(white*pct_BGinHUC,0)),
            black = sum(round(black*pct_BGinHUC,0)),
            native_american = sum(round(native_american*pct_BGinHUC,0)),
            ahpi = sum(round(ahpi*pct_BGinHUC,0)),
            oth2 = sum(round(oth2*pct_BGinHUC,0)),
            latinx = sum(round(latinx*pct_BGinHUC,0)),
            whtpct10 = round((white/total), 4),
            blkpct10 = round((black/total), 4),
            napct10 = round((native_american/total),4),
            ahpipct10 = round((ahpi/total), 4),
            othpct10 = round((oth2/total), 4),
            ltxpct10 = round((latinx/total), 4))

# verify that percentages == 1.
verify <- bg_huc10d %>%
  mutate(one = whtpct10+blkpct10+napct10+ahpipct10+othpct10+ltxpct10)
# max = 1.0002
# min = 0.9997

################################################
#######    2010 CT entropy calculation       ###
################################################

emm <- bg_huc10d %>%
  mutate(E = -((whtpct10*log(whtpct10)+blkpct10*log(blkpct10) + napct10*log(napct10) +
                  ahpipct10*log(ahpipct10) + othpct10*log(othpct10) +
                  ltxpct10*log(ltxpct10)))/log(6))

##############################################################
##### assign 'classes' for segregation X diversity - RACE ##
##############################################################

cars %>% mutate(speed = replace(speed, speed < 30, 0))

em2$dist1

em2 <- emm %>%
  mutate(dist1 = 1,
         dist1 = replace(dist1, E <= .3707 & whtpct10 > .65 | whtpct10 >=.8, 2),
         dist1 = replace(dist1, E <= .3707 & blkpct10 > .65 | blkpct10 >= .8, 3),
         dist1 = replace(dist1, E > .7414 & whtpct10 < .46 & blkpct10 < .46, 14),
         dist1 = replace(dist1, E > .3707 & E < .7414 & whtpct10 < .8 & whtpct10 > blkpct10 & whtpct10 > ltxpct10,8),
         dist1 = replace(dist1, whtpct10 >.46 & whtpct10 <.8, 8),
         dist1 = replace(dist1, E > .3707 & E < .7414 & blkpct10 < .8 & blkpct10 > whtpct10 & blkpct10 > ltxpct10,9),
         dist1 = replace(dist1, blkpct10 >.46 & blkpct10 <.8,9),
         dist1 = replace(dist1, E > .3707 & E < .7414 & ltxpct10 < .8 & ltxpct10 > whtpct10 & ltxpct10 > blkpct10,13),
         dist1 = replace(dist1, E <= .3707 & ltxpct10 > .65 | ltxpct10 >= .8, 7),
         dist1 = replace(dist1, ltxpct10 >.46 & ltxpct10 <.8, 13),
         dist1 = replace(dist1, E <= .3707 & othpct10 > .65 | othpct10 >= .8, 4),
         dist1 = replace(dist1, E <= .3707 & napct10 > .65 | napct10 >= .8, 5))

sp_em2 <- st_as_sf(em2)
class(sp_em2)
str(sp_em2)

sp_em2$dist1 <- sp_em2$dist1 %>% as.factor()
str(spem)

tm_shape(sp_em2) +
  tm_polygons('dist1')

#######################################################################
# various short scripts used to verify data (and coding) accuracy 
# (in no particular order)
#######################################################################

## import area of interest data
#ga <- get_decennial(geography = "block",
#                    variables = dec_vars,
#                    state = "Georgia",
#                    year = 2010,
#                    output = 'wide',
#                    geometry = TRUE) %>%
#  st_transform(crs = alb) %>%
#  mutate(latinx = total - nonlatinx, SqKM_BG = as.numeric(st_area(geometry)) / 1e6) %>%
#  dplyr::select(-nonlatinx)


## define acs variables of interest
#acs_vars <- c(white = "B03002_003E", black = "B03002_004E",
#              native_american = "B03002_005E", asian = "B03002_006E",
#              hawaiian = "B03002_007E", other = "B03002_008E",
#              multiracial = "B03002_009E", latinx = "B03002_012E")


# verify totals for variables that have separate variable codes (i.e. multiple 'latinx')
m <- get_decennial(geography = "tract",
                   variables = c('P0040001','P0040002','P0040003','P0050001', 
                                 'P0050002'),
                   state = c("Georgia"), 
                   year = 2010, 
                   output = 'wide')

m <- m %>%
  mutate(t1 = P0040001-P0050001,
         t2 = P0040002-P0050002,
         yhl = P0050001-P0050002,
         t3 = P0040003 - yhl)

tmap_mode("view")  
tm_shape(ct_huc10c) +
  tm_polygons('pbl')

hucs <- ct_huc10b %>%
  group_by(HUC10) %>%
  summarise(total = sum(tot2),
            white = sum(wht2),
            black = sum(blk2),
            pbl = black/total)

# https://gis.stackexchange.com/questions/140504/extracting-intersection-areas-in-r?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa