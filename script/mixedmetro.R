# mixed metro data for HUC 10s
# created: May 5, 2018
# Author: Taylor Hafley
# git: deanhardy/se_segregation
# local location: desktop/school/sideProjects/segXdiversity/se_segregation/

library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
library(sf)
library(tmap)

th_api_acs <- '45544f0d114cfaa037a5566745d18bb8d4778cfa'

## re-run if census variables need to be changed
## define acs variables of interest
acs_vars <- c(white = "B03002_003E", black = "B03002_004E",
              native_american = "B03002_005E", asian = "B03002_006E",
              hawaiian = "B03002_007E", other = "B03002_008E",
              multiracial = "B03002_009E", latinx = "B03002_012E")

v15 <- load_variables(2016, "acs5", cache = TRUE)
v10 <- load_variables(2010, "sf1", cache = TRUE)

View(v10)

## define decennial variables of interest
dec_vars <- c(total = "P0050001",
              nonlatinx = "P0050002",
              white = "P0050003", black = "P0050004",
              native_american = "P0050005", asian = "P0050006",
              hawaiian = "P0050007", other = "P0050008",
              twomore = "P0050009", latinx = "P0040003"
              )

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


ga <- get_decennial(geography = 'tract',
              variables = dec_vars,
              state = c('GA'),
              year = 2010,
              output = 'wide',
              geometry = TRUE) 

ga <- ga %>%
  mutate(ahpi = asian + hawaiian,
         oth2 = other + twomore,
         whtpct10 = round((white/total), 5),
         blkpct10 = round((black/total), 5),
         napct10 = round((native_american/total),5),
         ahpict10 = round((ahpi/total), 5),
         othpct10 = round((oth2/total), 5),
         ltxpct10 = round((latinx/total), 5))

tm_shape(ga) +
  tm_polygons('ltxpct10')









#2006-10 BG entropoy calculation
gabg10$asian <- gabg10$JMJE006 + gabg10$JMJE007
gabg10$other <- gabg10$JMJE008 + gabg10$JMJE018

gabg10$pctwht <- (gabg10$JMJE003/gabg10$JMJE001)
gabg10$pctblk <- (gabg10$JMJE004/gabg10$JMJE001)
gabg10$pcthisp <- (gabg10$JMJE012/gabg10$JMJE001)
gabg10$pctntv <- (gabg10$JMJE005/gabg10$JMJE001)
gabg10$pctoth <- (gabg10$other/gabg10$JMJE001)
gabg10$pctasi <- (gabg10$asian/gabg10$JMJE001)

gabg10[, 61:66][gabg10[, 61:66] == 0] <- 0.0000000001

gabg10$E <- -((gabg10$pctwht*log(gabg10$pctwht) + gabg10$pctblk*log(gabg10$pctblk) + gabg10$pcthisp*log(gabg10$pcthisp) + gabg10$pctasi *log(gabg10$pctasi) + gabg10$pctntv*log(gabg10$pctntv)))
gabg10$E <- gabg10$E/log(5)




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