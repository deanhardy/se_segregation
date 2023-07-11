##############################################################################
# census data prep for HUC MixedMetro
# created: July 11, 2023
# Author: Taylor Hafley, Dean Hardy
# git: deanhardy/se_segregation
# local location: Dropbox/school/Projects/inProgress/watershed/se_segregation/
##############################################################################
rm(list=ls())

# load necessary libraries
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

# race data from decennial census needed for Mixed Metro classifications
# [note: latinx, 'P0040003', = total - nonlatinx] #tested and confirmed
# six racial groups: White, Black, American Indian, Asian or Pacific Islanders, 
#                           Some other race & two or more races, Latinx

# vector of Atlanta HUC counties (need county in call for block group data
# using tidycensus (get_decennial/acs))
cnty <- c("Baldwin","Banks","Barrow","Bartow","Butts","Carroll","Cherokee","Clarke",
          "Clayton","Cobb", "Coweta", "Dawson", "DeKalb", "Douglas","Fann",
          "Fayette","Floyd","Forsyth", "Fulton","Gordon","Gwinnett","Habersham",
          "Hall","Haralson","Heard", "Henry","Jackson","Jasper","Jones","Lamar",
          "Lumpkin","Meriwether","Monroe","Morgan","Newton",
          "Paulding","Pickens","Pike","Putnam","Polk","Rabun",
          "Rockdale","Spalding","Towns","Union","Upson","Walton","White")

###################################
#### 1990 decennial data prep ####
###################################
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
         nonlatinx = (white + black + native_american + asian + other),
         total = (white + black + native_american + asian + other + latinx)  
  ) %>%
  filter(COUNTY %in% cnty) 

# read in shapefile of GA block groups
ga_sp <- st_read("data/data_share/GA_blck_grp_1990.shp")
gabg90 <- left_join(ga_sp, gabg90) %>%
  filter(!is.na(COUNTY)) %>%
  mutate(SqKM_BG = as.numeric(st_area(geometry)) / 1e6, year = 1990) %>%
  st_transform(4269) %>%
  rename(ahpi = asian, oth2 = other, NAME = COUNTY, GEOID = STFID) %>%
  select(year, GEOID, NAME, total, latinx, nonlatinx, white, black, native_american, ahpi, oth2, SqKM_BG, geometry)

rm(ga_sp)

###################################
#### 2000 decennial data prep ####
###################################
## define SF3 variables for decennial census years
dec_vars00 <- c(total = "P007001",
                latinx = "P007010",
                nonlatinx = "P007002",
                white = "P007003", black = "P007004",
                native_american = "P007005", asian = "P007006",
                hawaiian = "P007007", other = "P007008",
                twomore = "P007009"
)

# import 2000 race data using tidycensus
gabg00 <- get_decennial(geography = "block group", variables = dec_vars00, 
                        state = "GA", county = cnty, year = 2000,
                        output = 'wide',
                        geometry = TRUE) %>%
  mutate(SqKM_BG = as.numeric(st_area(geometry)) / 1e6, year = 2000) %>%
  mutate(ahpi = asian + hawaiian,
         oth2 = other + twomore) %>%
  st_transform(4269) %>%
  select(year, GEOID, NAME, total, latinx, nonlatinx, white, black, native_american, ahpi, oth2, SqKM_BG, geometry)


###################################
#### 2010 decennial data prep ####
###################################
dec_vars10 <- c(total = "P005001",
                latinx = "P004003",
              nonlatinx = "P005002",
              white = "P005003", black = "P005004",
              native_american = "P005005", asian = "P005006",
              hawaiian = "P005007", other = "P005008",
              twomore = "P005009"
)

# import race data using tidycensus
gabg10 <- get_decennial(geography = "block group", variables = dec_vars10, 
                      state = "GA", county = cnty, year = 2010,
                      output = 'wide',
                      geometry = TRUE) %>%
  mutate(SqKM_BG = as.numeric(st_area(geometry)) / 1e6, year = 2010) %>%
  mutate(ahpi = asian + hawaiian,
         oth2 = other + twomore) %>%
  select(year, GEOID, NAME, total, latinx, nonlatinx, white, black, native_american, ahpi, oth2, SqKM_BG, geometry)


###################################
#### 2020 decennial data prep ####
###################################
dec_vars20 <- c(total = "P2_001N",
                latinx = "P2_002N",
                nonlatinx = "P2_003N",
                white = "P2_005N", black = "P2_006N",
                native_american = "P2_007N", asian = "P2_008N",
                hawaiian = "P2_009N", other = "P2_010N",
                twomore = "P2_011N"
)

# import race data using tidycensus
gabg20 <- get_decennial(geography = "block group", variables = dec_vars20, 
                      state = "GA", county = cnty, year = 2020,
                      output = 'wide',
                      geometry = TRUE) %>%
  mutate(SqKM_BG = as.numeric(st_area(geometry)) / 1e6, year = 2020) %>%
  mutate(ahpi = asian + hawaiian,
         oth2 = other + twomore) %>%
  select(year, GEOID, NAME, total, latinx, nonlatinx, white, black, native_american, ahpi, oth2, SqKM_BG, geometry)


#################################
## test accuracy of count data ##
#################################
gabg20 %>%
  mutate(tot1 = (latinx + nonlatinx) - total,
         tot2 = (white + black + native_american + ahpi + oth2 + latinx ) - total) %>%
  select(total, latinx,nonlatinx, tot2, tot1, GEOID) %>%
  arrange(desc(tot2))

gabg20 %>%
  group_by(GEOID) %>%
  summarize(checktotal = 1,
            checktotal = ((white + black + native_american + ahpi + oth2 + latinx) - total)) %>%
  arrange(desc(checktotal))


# Atlanta urban area
atl <- urban_areas(year = '2020') %>%
  filter(NAME10 == 'Atlanta, GA') %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")

## plot map
tm_shape(atl) +
  tm_fill('red') + 
  tm_shape(gabg90) + 
  tm_borders()

## export data ##
gabg <- rbind(gabg90, gabg00, gabg10, gabg20)

st_write(gabg, 'data/spatial/census_allyears.GEOJSON', driver = 'GEOJSON', append = FALSE)
st_write(gabg90, 'data/spatial/census1990.GEOJSON', driver = 'GEOJSON', append = FALSE)
st_write(gabg00, 'data/spatial/census2000.GEOJSON', driver = 'GEOJSON', append = FALSE)
st_write(gabg10, 'data/spatial/census2010.GEOJSON', driver = 'GEOJSON', append = FALSE)
st_write(gabg20, 'data/spatial/census2020.GEOJSON', driver = 'GEOJSON', append = FALSE)
