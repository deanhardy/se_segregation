##############################################################################
# watershed prep for HUC Mixed Metro
# created: July 11, 2023
# Author: Taylor Hafley, Dean Hardy
# git: deanhardy/se_segregation
# local location: Dropbox/school/Projects/inProgress/watershed/se_segregation/
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


########################################################################
## calculate area & percent of each BG in each HUC to set up
## proportional allocation method 
########################################################################
gabg <- st_read('data/spatial/census_allyears.GEOJSON')
wtr <- st_read('data/spatial/watersheds.GEOJSON')

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
         class10 = replace(class10, E <= .3707 & whtpct > .65 | whtpct >=.8, 2),
         class10 = replace(class10, E <= .3707 & blkpct > .65 | blkpct >= .8, 3),
         class10 = replace(class10, E > .7414 & whtpct < .46 & blkpct < .46, 14),
         class10 = replace(class10, E > .3707 & E < .7414 & whtpct < .8 & whtpct > blkpct & whtpct > ltxpct,8),
         class10 = replace(class10, whtpct >.46 & whtpct <.8, 8),
         class10 = replace(class10, E > .3707 & E < .7414 & blkpct < .8 & blkpct > whtpct & blkpct > ltxpct,9),
         class10 = replace(class10, blkpct >.46 & blkpct <.8,9),
         class10 = replace(class10, E > .3707 & E < .7414 & ltxpct < .8 & ltxpct > whtpct & ltxpct > blkpct,13),
         class10 = replace(class10, E <= .3707 & ltxpct > .65 | ltxpct >= .8, 7),
         class10 = replace(class10, ltxpct >.46 & ltxpct <.8, 13),
         class10 = replace(class10, E <= .3707 & othpct > .65 | othpct >= .8, 4),
         class10 = replace(class10, E <= .3707 & napct > .65 | napct >= .8, 5)) %>%
  mutate(year = dec_year[[i]], shed = shed[[z]])

OUT$class10 <- OUT$class10 %>% as.factor()
  # fct_reorder(shd_bg$class10, c(2,3,8,9,13,14))

shd_bg <- rbind(OUT, shd_bg)

  }
}

#write_sf(huc12_bg10, "data/geojson/huc12_bg10.geojson")

#st_write(huc12_bg10, dsn = "data/shp/huc12b", layer = "huc12_bg10", driver = "ESRI Shapefile")

##################################################
## map RACE diversity/segregation by watershed ##
##################################################

## create custom palette with custom legend labels for seg indices
# race_mm_col <- c("#ff9900","#66cc00","#ffcc99", "#99ff99", "#cc99ff","#99752e")
leg_col2 <- c("#ff9900","#66cc00","#ffcc99", "#99ff99", "#cc99ff","#99752e")
lbl2 <- c("White (Low)","Black (Low)","White (Mod)",
          "Black (Mod)","Latinx (Mod)","High Diversity")
# names(shd_bg) <- levels(factor(c(levels(shd_bg$class10))))

rd <- primary_roads(year = 2016)

shd <- 
  tm_shape(shd_bg) +
  tm_fill('class10', legend.show = FALSE, palette = leg_col2)+
  tm_add_legend(type = c("fill"), labels = lbl2, col = leg_col2, 
                title = paste(blk_grps[[i]], "seg & diversity\nby", sheds[[z]], "Watershed")) +
  tm_shape(rd) + 
  tm_lines(col = "black") +
  tm_compass(type = "arrow", size = 4, position = c(0.82, 0.08)) +
  tm_scale_bar(breaks = c(0,20), text.size = 1.1, position= c(0.8, 0.0)) +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = .9,
            legend.title.size = 1.1) + 
  tm_layout(frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=0)

# png(paste0('figures/map-', blk_grps[[i]], '.png'), units = 'in', width = 5, height = 5, res = 150)
# shd
# dev.off()

## save map of diversity/seg
tmap_save(shd, paste0("figures/map-", sheds[[z]], '-', blk_grps[[i]], '.png'), width=1920, height=1080, asp=0)

tapply(shd_bg$class10, shd_bg$class10, length)

## bargraph of diversity/seg
shdbg <- ggplot(data = shd_bg) +
  geom_bar(mapping = aes(x = class10, fill = class10
  )) +
  ggtitle(paste(blk_grps[[i]], "segregation & diversity\nby", sheds[[z]], "watershed")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    x = "segregation and diversity",
    y = 'Total Number of huc12s') 

rdkb2 <- shdbg + scale_fill_manual(values = leg_col2,
                                   limits = c("2","3","8","9","13","14"),
                                   name = "race, diversity",
                                   labels = lbl2)

## save bargraph
ggsave(rdkb2, file=paste('figures/barchart-', sheds[[z]], '-', blk_grps[[i]], ".png", sep=''), width = 6, height = 5, units = 'in', scale=2)


