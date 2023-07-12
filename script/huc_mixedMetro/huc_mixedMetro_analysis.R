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


##################################################
## graph RACE diversity/segregation by watershed ##
##################################################
## plot local watershed stats over time
fig <- ggplot(filter(shd_bg, HUC_NO %in% c('WAWA', 'SWRA', 'uFlint'))) + 
  geom_line(aes(year, (1-whtpct)*100, color = HUC_NO)) + 
  geom_point(aes(year, (1-whtpct)*100, color = HUC_NO)) + 
  scale_y_continuous(name = 'Nonwhite Population (%)', limits = c(0,100), expand = c(0,0), breaks = seq(0,100,10)) +
  scale_x_continuous(name = 'Year') +
  labs(color = 'Watershed') +
  theme(text = element_text(size = 15),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major.y = element_line(colour = 'grey'),
        panel.grid.major.x = element_line(colour = 'grey', linetype = 'dotted'))
fig

png('figures/localsheds-pNonwhite.png', units = 'in', height = '4', width = '6', res = 150)
fig
dev.off()

## generate summary tables of population stats
class_smry <- shd_bg %>%
  group_by(year, shed, class10) %>%
  summarise(total = sum(total), black = sum(black), white = sum(white), n = n())
write.csv(st_drop_geometry(class_smry), 'results-tables/class_smry.csv')

blk_smry <- smry %>%
  filter(class10 %in% c(3,9), shed == 'huc12') %>%
  group_by(year, class10) %>%
  summarise(total = sum(total), black = sum(black), n = sum(n))
write.csv(st_drop_geometry(blk_smry), 'results-tables/blk_smry.csv')

## create custom palette with custom legend labels for seg indices
# race_mm_col <- c("#ff9900","#66cc00","#ffcc99", "#99ff99", "#cc99ff","#99752e")
leg_col2 <- c("#ff9900","#66cc00","#ffcc99", "#99ff99", "#cc99ff","#99752e")
lbl2 <- c("White (Low)","Black (Low)","White (Mod)",
          "Black (Mod)","Latinx (Mod)","High Diversity")

## bargraph of diversity/seg
bargraph <- ggplot(data = blk_smry) +
  geom_col(aes()) +
  ggtitle(paste("Segregation & Diversity\nby HUC12 Watershed")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    x = "Segregation and Diversity",
    y = 'Total Number of HUC 12s') 
bargraph

## linegraph
linegraph <- ggplot(filter(class_smry, shed == 'huc12')) +
  geom_line(aes(year, n, color = class10)) +
  geom_point(aes(year, n, color = class10)) +
  ggtitle(paste("Segregation & Diversity\nby HUC12 Watershed")) +
  labs(
    x = "Year",
    y = 'Total Number of HUC 12s') + 
  theme(text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major.y = element_line(colour = 'grey'),
        panel.grid.major.x = element_line(colour = 'grey', linetype = 'dotted'))
linegraph

rdkb2 <- linegraph + scale_color_manual(values = leg_col2,
                                   limits = c("2","3","8","9","13","14"),
                                   name = "Race (Diversity)",
                                   labels = lbl2)
rdkb2
png('figures/huc12-seg-div.png', units = 'in', height = '4', width = '6', res = 150)
rdkb2
dev.off()

## save bargraph
# ggsave(rdkb2, file=paste('figures/barchart-', sheds[[z]], '-', blk_grps[[i]], ".png", sep=''), width = 6, height = 5, units = 'in', scale=2)


##################################################
## map RACE diversity/segregation by watershed ##
##################################################
rd <- primary_roads(year = 2016)

# Atlanta urban area
atl <-urban_areas(year = '2020') %>%
  filter(NAME10 == 'Atlanta, GA')

shd_bg2 <- shd_bg %>%
  mutate(class10 = factor(class10, levels = c(2,3,8,9,13,14)))

shd <- 
  tm_shape(filter(shd_bg2, shed == 'huc12' & year == 2020)) +
  tm_fill('class10', legend.show = FALSE, palette = leg_col2) + 
  tm_add_legend(type = c("fill"), labels = lbl2, col = leg_col2, 
                title = paste('Race (Diversity)')) +
  tm_shape(atl) + 
  tm_borders(col = 'grey30', lty = 'solid') + 
  tm_shape(filter(shd_bg2, shed == 'local' & year == 2020)) + 
  tm_borders(col = 'black') + 
  tm_shape(rd) + 
  tm_lines(col = "gray60") +
  tm_compass(type = "arrow", size = 3, position = c(0.78, 0.1)) +
  tm_scale_bar(breaks = c(0,20), text.size = 1.1, position= c(0.8, 0.0)) +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = .9,
            legend.title.size = 1.1) + 
  tm_layout(frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0.1,0.1,0.1,0.1), asp=0,
            title = paste(shd_bg2$year, "Segregation & Diversity\nby HUC12 Watershed"))

png(paste0('figures/map-huc12-2020.png'), units = 'in', width = 5, height = 5, res = 150)
shd
dev.off()

## save map of diversity/seg
tmap_save(shd, paste0("figures/map-", sheds[[z]], '-', blk_grps[[i]], '.png'), width=1920, height=1080, asp=0)

