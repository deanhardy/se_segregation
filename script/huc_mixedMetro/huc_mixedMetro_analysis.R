##############################################################################
# Analysis for HUC Mixed Metro
# created: July 11, 2023
# Authors: Taylor Hafley, Dean Hardy
# git: deanhardy/se_segregation
##############################################################################
rm(list=ls())

# load necessary libraries
library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
library(sf)
library(tmap)
library(tmaptools)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/se_segregation/')

########################################################################
## calculate area & percent of each BG in each HUC to set up
## proportional allocation method 
########################################################################
gabg <- st_read(paste0(datadir, '/data/spatial/census_allyears.GEOJSON'))
wtr <- st_read(paste0(datadir, 'data/spatial/watersheds.GEOJSON'))

shed <- list('huc12', 'huc10', 'local')
dec_year <- list(1990, 2000, 2010, 2020)
shd_bg <- NULL

for (i in 1:length(dec_year)) {
  for (z in 1:length(shed)) {
  
## filter to census year and watershed unit  
bg <- gabg %>% filter(year == dec_year[[i]])
shd <- wtr %>% filter(category == shed[[z]])

# returns all block groups that intersect watershed (via indexing)
OUT <- bg[shd,]

# identify sub-geographies that intersect watershed boundary
int <- st_intersection(OUT, shd)

# extract new BGs within HUCs
OUT <- int %>%
  mutate(BG_SqKmHuc = round(as.numeric((st_area(geometry) / 1e6)),4)) %>%
  mutate(pct_BGinHUC = round(BG_SqKmHuc/SqKM_BG,4))

# calc % groups in new HUCs
OUT <- OUT %>%
  group_by(HUC_NO) %>%
  summarise(total = sum(total*pct_BGinHUC),
            white = sum(white*pct_BGinHUC),
            black = sum(black*pct_BGinHUC),
            native_american = sum(native_american*pct_BGinHUC),
            ahpi = sum(ahpi*pct_BGinHUC),
            oth2 = sum(oth2*pct_BGinHUC),
            latinx = sum(latinx*pct_BGinHUC),
            whtpct = round((white/total), 4),
            blkpct = round((black/total), 4),
            napct = round((native_american/total),4),
            ahpipct = round((ahpi/total), 4),
            othpct = round((oth2/total), 4),
            ltxpct = round((latinx/total), 4))

## BG -- HUC entropy calculation
OUT <- OUT %>%
  mutate(E = -((whtpct*log(whtpct)+blkpct*log(blkpct) + napct*log(napct) +
                  ahpipct*log(ahpipct) + othpct*log(othpct) +
                  ltxpct*log(ltxpct)))/log(6)) %>%
  mutate(class10 = 1,
         class10 = replace(class10, E <= .3707 & whtpct > .65 | whtpct >=.8, 2), # lowWhite
         class10 = replace(class10, E <= .3707 & blkpct > .65 | blkpct >= .8, 3), # lowBlack
         class10 = replace(class10, E > .7414 & whtpct < .46 & blkpct < .46, 14), #highDiversity
         class10 = replace(class10, E > .3707 & E < .7414 & whtpct < .8 & whtpct > blkpct & whtpct > ltxpct,8), # modWhite
         class10 = replace(class10, whtpct >.46 & whtpct <.8, 8), # modWhite
         class10 = replace(class10, E > .3707 & E < .7414 & blkpct < .8 & blkpct > whtpct & blkpct > ltxpct,9), # modBlack
         class10 = replace(class10, blkpct >.46 & blkpct <.8,9), # modBlack
         class10 = replace(class10, E > .3707 & E < .7414 & ltxpct < .8 & ltxpct > whtpct & ltxpct > blkpct,13), # modLatinx
         class10 = replace(class10, E <= .3707 & ltxpct > .65 | ltxpct >= .8, 7), #lowLatinx
         class10 = replace(class10, ltxpct >.46 & ltxpct <.8, 13), # modLatinx
         class10 = replace(class10, E <= .3707 & othpct > .65 | othpct >= .8, 4), # lowOther
         class10 = replace(class10, E <= .3707 & napct > .65 | napct >= .8, 5)) %>% # lowNA
  mutate(year = dec_year[[i]], shed = shed[[z]])

OUT$class10 <- OUT$class10 %>% as.factor()
  # fct_reorder(shd_bg$class10, c(2,3,8,9,13,14))

shd_bg <- rbind(OUT, shd_bg)

  }
}

## add descriptors to classes
shd_bg2 <- 
  shd_bg %>%
  mutate(category = if_else(class10 == 2, 'LDW', 
                  if_else(class10 == 3, 'LDB', 
                          if_else(class10 == 14, 'HD',
                                  if_else(class10 == 8, 'MDW',
                                          if_else(class10 == 9, 'MDB', 
                                                  if_else(class10 == 13, 'MDL',
                                                          if_else(class10 == 7, 'LDL',
                                                                  if_else(class10 == 13, 'MDL',
                                                                          if_else(class10 == 4, 'LDO',
                                                                                  if_else(class10 == 5, 'LDN', class10))))))))))
  )

## export results
st_write(shd_bg, paste0(datadir, 'data/spatial/hucMixedMetro.GEOJSON'), driver = 'GEOJSON', append = FALSE)
