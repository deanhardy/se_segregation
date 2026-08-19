#### HUC Mixed Metro (aka Mixed Hydro) ####
# Analysis of results for HUC Mixed Metro
# created: July 11, 2023
# Authors: Taylor Hafley, Dean Hardy
# git: deanhardy/se_segregation

rm(list=ls())

#### load necessary libraries ####
library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
library(sf)
library(tmap)
library(tmaptools)
library(plotrix)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/se_segregation/')

#### import and organize data ####

## import results
shd_bg <- st_read(paste0(datadir, 'data/spatial/hucMixedMetro.GEOJSON')) %>%
  mutate(SqKM = as.numeric(st_area(geometry)) / 1e6)

## quick huc10 results
h10 <- shd_bg %>% filter(shed == 'huc10') %>% group_by(year, class10) %>% summarise (n = n())
## white dominant
h10 %>% filter(class10 %in% c('2','8')) %>% ungroup() %>% summarise(sum(n))

## quick huc12 results
h12 <- shd_bg %>% filter(shed == 'huc12') %>% group_by(year, class10) %>% summarise (n = n())
## white dominant
h12 %>% filter(class10 %in% c('2','8')) %>% ungroup() %>% summarise(sum(n))

## pop avg for hucs to compare to census unit averages
huc.sum <- shd_bg %>% group_by(year, shed) %>% 
  summarise(pop.sum = sum(total),
            pop.avg = mean(total), 
            pop.se = std.error(total),
            SqKM.sum = sum(SqKM),
            SqKM.avg = mean(SqKM),
            Sqkm.se = std.error(SqKM)
            )
  
## download ancillary data for map
wtr <- st_read(paste0(datadir, 'data/spatial/watersheds.GEOJSON'))
rd <- primary_roads(year = 2022) %>%
  filter(RTTYP == 'I')
atl <-urban_areas(year = '2020') %>%
  filter(NAME10 == 'Atlanta, GA') %>%
  mutate(name = 'Atlanta')
cnty <- counties(state = 'GA')
cnty_list <- list_counties('GA')
arc_list <- c("Cherokee","Clayton","Cobb", "DeKalb", "Douglas",
         "Fayette","Forsyth", "Fulton","Gwinnett", "Henry"
         # "Rockdale"
         )
arc <- filter(cnty_list, county %in% arc_list)
sts <- states(year = 2020) 
ga <- filter(sts, STUSPS == "GA")
rvr <- linear_water('GA', cnty_list$county_code)
rvr2 <- rvr %>%
  filter(FULLNAME %in% c('Chattahoochee Riv', 'Chattahoochie Riv', 'South Riv', 'Yellow Riv', 'Alcovy Riv', 'Ocmulgee Riv'))
# lakes <- area_water('GA', cnty_list$county_code) %>%
#   filter(FULLNAME %in% c('Lk Jackson', 'Lk Sidney Lanier'))

## mm results for tracts from Taylor
mm <- st_read(paste0(datadir, 'data/mm_1990_2000_2010_2020')) %>%
  st_make_valid()
mm2 <- mm %>%
  st_transform(4269)
mm2 <- 
  mm2[atl,]
  # mm2 %>%
  # filter(grepl(arc$county, name2000))
mm2 <- mm2 %>% mutate(class_2020 = factor(class_2020, levels = c(2,3,7,8,9,10,13,14)))

## organize classes with factors
shd_bg <- shd_bg %>%
  mutate(class10 = factor(class10, levels = c(2,3,8,9,13,14)))

## filter to custom/local watersheds
local <- shd_bg %>% filter(shed == 'local' & year == 2020)

## create custom palette with custom legend labels for seg indices
leg_col2 <- c("#ff9900","#66cc00","#ffcc99", "#99ff99", "#cc99ff","#99752e")
lbl2 <- c("White (Low)","Black (Low)","White (Mod)",
          "Black (Mod)","Latinx (Mod)","High Diversity")
lbl3 <- c("LDW","LDB","MDW","MDB","MDL","HD")

leg_mm <- c("#ff9900","#66cc00", "#9966ff",
            "#ffcc99", "#99ff99", "#ff9999","#cc99ff",
            "#99752e")
lbl_mm <- c("White (Low)","Black (Low)","Latinx (Low)",
            "White (Mod)", "Black (Mod)","Asian (Mod)", "Latinx (Mod)", 
            "High Diversity")

shd_bg$HUC_NO <- factor(shd_bg$HUC_NO, levels=c('HUC10', 'HUC12','WAWA', 'SRWA', 'uFlint'))

#### figures from results ####
## plot local watershed stats over time
fig <- ggplot(filter(shd_bg, HUC_NO %in% c('WAWA', 'SRWA', 'uFlint'))) + 
  geom_line(aes(year, (1-whtpct)*100, color = HUC_NO)) + 
  geom_point(aes(year, (1-whtpct)*100, color = HUC_NO)) + 
  geom_line(aes(year, blkpct*100, color = HUC_NO), lty = 'dashed') + 
  geom_point(aes(year, blkpct*100, color = HUC_NO)) + 
  scale_y_continuous(name = 'Nonwhite (solid) & Black (dashed) Population (%)', limits = c(0,100), expand = c(0,0), breaks = seq(0,100,10)) +
  scale_x_continuous(name = 'Year') +
  labs(color = 'Community\nOrganizing\nWatershed')+
  theme(text = element_text(size = 10),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major.y = element_line(colour = 'grey'),
        panel.grid.major.x = element_line(colour = 'grey', linetype = 'dotted'))
fig

png(paste0(datadir, 'figures/localsheds-pNonwhite+pBlack.png'), units = 'in', height = '4', width = '6', res = 300)
fig
dev.off()

  ###### summary tables of results ######

## summarize huc12s by year and classification
class_smry <- shd_bg %>%
  filter(shed == 'huc12') %>%
  group_by(year, shed, class10) %>%
  summarise(total = sum(total), black = sum(black), white = sum(white), n = n())
write.csv(st_drop_geometry(class_smry), paste0(datadir, 'tables/class_smry.csv'))

## summarize huc12s by watershed type, year, and classification
blk_smry <- shd_bg %>%
  filter(class10 %in% c(3,9), shed %in% c('huc12', 'local')) %>%
  group_by(shed, year, class10) %>%
  summarise(total = sum(total), black = sum(black), nonwhite = sum(total) - sum(white), n = n()) %>%
  mutate(pBlack = (black/total) * 100, pNonwhite = (nonwhite/total) * 100)
write.csv(st_drop_geometry(blk_smry), paste0(datadir, 'tables/summary_reults_blackonly.csv'))

## summarize community watersheds by year and classification; about the same as above
blk_smry2 <- filter(shd_bg, HUC_NO %in% c('WAWA', 'SRWA', 'uFlint')) %>%
  group_by(HUC_NO, year) %>%
  summarise(total = sum(total), black = sum(black), nonwhite = sum(total) - sum(white), n = n()) %>%
  mutate(pBlack = (black/total) * 100, pNonwhite = (nonwhite/total) * 100)

## summarize percent Black pop in community watersheds by year for ARC
blk_community_smry <- shd_bg %>%
  filter(shed %in% c('huc12', 'local')) %>%
  group_by(shed, year) %>%
  summarise(total = sum(total), black = sum(black), nonwhite = sum(total) - sum(white), n = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  # mutate(pBlack = (black/total) * 100, pNonwhite = (nonwhite/total) * 100) %>%
  summarise(pBlack = (black[shed == "local"] / black[shed == "huc12"])*100,
            pNonwhite = (nonwhite[shed == "local"] / nonwhite[shed == "huc12"])*100)

###### graphs of results ######
## bargraph of diversity/seg
# bargraph <- ggplot(data = blk_smry) +
#   geom_col(aes()) +
#   ggtitle(paste("Segregation & Diversity\nby HUC12 Watershed")) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   labs(
#     x = "Segregation and Diversity",
#     y = 'Total Number of HUC 12s') 
# bargraph

## linegraph
linegraph <- ggplot(filter(class_smry, shed == 'huc12')) +
  geom_line(aes(year, n, color = class10)) +
  geom_point(aes(year, n, color = class10)) +
  # ggtitle(paste("Segregation & Diversity\nby HUC12 Watershed")) +
  labs(
    x = "Year",
    y = 'Number of Watersheds (HUC12)') + 
  theme(text = element_text(size = 10),
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

## export summary by class
png(paste0(datadir, 'figures/huc12-mixedmetro-1990-2020.png'), units = 'in', height = '4', width = '6.5', res = 150)
rdkb2
dev.off()

# library(ggpubr)
# ggarrange(fig, rdkb2, ncol = 2, nrow = 1)

## save bargraph
# ggsave(rdkb2, file=paste('figures/barchart-', sheds[[z]], '-', blk_grps[[i]], ".png", sep=''), width = 6, height = 5, units = 'in', scale=2)

#### maps ####
###### facet map ######
# for (i in 1:length(dec_year)) {
# 
# assign(paste0("variable_", i), 
#        
#   tm_shape(filter(shd_bg, shed == 'huc12' & year == dec_year[[i]])) +
#   tm_fill('class10', legend.show = FALSE, palette = leg_col2) + 
#   tm_shape(atl) + 
#   tm_borders(col = 'grey30', lty = 'solid') + 
#   tm_shape(local) + 
#   tm_borders(col = 'black', lwd = 1.5) + 
#   tm_shape(rd) + 
#   tm_lines(col = "gray60") +
#   # tm_compass(type = "arrow", size = 3, position = c(0.78, 0.1)) +
#   # tm_scale_bar(breaks = c(0,20), text.size = 1.1, position= c(0.8, 0.0)) +
#   # tm_add_legend(type = c("fill"), labels = lbl2, col = leg_col2, 
#   #               title = paste('Race (Diversity)')) +
#   # tm_legend(position = c(0.025, 0.05),
#   #           bg.color = "white",
#   #           frame = TRUE,
#   #           legend.text.size = .9,
#   #           legend.title.size = 1.1) + 
#   tm_layout(frame = TRUE, 
#             outer.margins=c(0,0,0,0), 
#             inner.margins=c(0,0,0,0), asp=0,
#             title = dec_year[[i]])
# )
# }

# png(paste0('figures/map-huc12', shd_bg$year,'.png'), units = 'in', width = 5, height = 5, res = 150)
# shd
# dev.off()

## save map of diversity/seg
# tmap_save(shd, paste0("figures/map-", sheds[[z]], '-', blk_grps[[i]], '.png'), width=1920, height=1080, asp=0)

tf.col <- c("#ff9900","#66cc00","#ffcc99", "#99ff99", "#cc99ff","#99752e")
tf.lbl <- c("White (Low)","Black (Low)","White (Mod)",
          "Black (Mod)","Latinx (Mod)","High Diversity")
tf.lbl2 <- c("LDW","LDB","MDW","MDB","MDL","HD")

shd_bg$category <- factor(shd_bg$category, levels = c("LDW","LDB","MDW","MDB","MDL","HD"))
  
## working on tmap facet
tf <- 
  tm_shape(filter(shd_bg, shed == 'huc12')) +
    tm_polygons(col = 'grey80', fill = 'category', fill.scale = tm_scale_categorical(values = tf.col),
                # tm_legend_hide()
                lwd = 0.5,
                fill.legend =
                  tm_legend(
                    type = "fill",
                    labels = tf.lbl,
                    fill = tf.col,
                    orientation = 'landscape',
                    # position = 'bottom',
                    title = "")
                ) + 
    tm_facets(by = 'year', ncol = 2) + 
  tm_shape(rd) + 
  tm_lines(col = "gray40") +
  tm_shape(local) +
    tm_borders(col = 'grey10', lwd = 2, lty = 'solid', fill_alpha = NA, tm_legend_hide()) +
    # tm_text('HUC_NO', case = 'upper', size = 0.5) +
  tm_compass(type = "arrow", size = 1.5, position = c(0.15,0.26)) +
  tm_scalebar(breaks = c(0,50), text.size = 0.8, position = c(0,0.13))
  # tm_comp_group("A", position = tm_pos_in("left", "bottom"), stack = 'vertical') + 
  # tm_add_legend(type = "polygons", labels = lbl2, fill = leg_col2,
  #               title = paste('Race (Diversity)'))
  # tm_layout(frame = TRUE, 
  #           outer.margins=c(0,0,0,0), 
  #           inner.margins=c(0,0,0,0), asp=0)

## save map of diversity/seg
tmap_save(tf, paste0(datadir, "figures/tmap-facets-huc12.png"), units = 'in', width=6.5, height=7)

   ######## site map of ATL #########
sitemap <- 
  # tm_shape(filter(shd_bg, shed == 'huc12' & year == 2020)) +
  # tm_fill('class10', legend.show = FALSE, palette = leg_col2) + 
  tm_shape(atl) +
  tm_borders(col = 'grey90', lty = 'solid') +
  tm_shape(mm2) + 
  tm_fill('class_2020', legend.show = F, palette = leg_mm) + 
  # tm_shape(cnty) + 
  # tm_borders(col = 'gray60', lty = 'dashed') + 
  # tm_shape(lakes) + 
  # tm_polygons(col = 'deepskyblue') + 
  tm_shape(rvr2) +
  tm_lines(col = 'deepskyblue', lwd = 2) +
  tm_shape(local) +
  tm_borders(col = 'grey10', lwd = 2, lty = 'solid', fill_alpha = NA, tm_legend_hide()) +
  tm_text('HUC_NO', case = 'upper', size = 1) +
  # tm_shape(rd) + 
  # tm_lines(col = "gray40") +
  # tm_text('FULLNAME', col = 'gray40') +
  tm_compass(type = "arrow", size = 3, position = c(0.17, 0.17)) +
  # tm_scale_bar(breaks = c(0,20), text.size = 0.8, position= c(0.12, 0.07)) +
  tm_add_legend(type = c("fill"), labels = lbl_mm, col = leg_mm,
                title = paste('Race (Diversity)')) +
  tm_legend(position = c(0.8, 0.3),
            bg.color = "white",
            frame.lwd = 1,
            legend.text.size = .8,
            legend.title.size = 1) +
  tm_layout(frame = TRUE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=0)
sitemap

## https://ecodiv.earth/post/creating-a-map-with-inset-using-tmap/index.html
insetmap <- 
  tm_shape(ga) +
  tm_borders() + 
  tm_shape(sts) + 
  tm_borders() + 
  tm_text('STUSPS') + 
  tm_shape(atl) + 
  tm_fill(fill = 'grey80') + 
  tm_text('name', col = 'black') +
  tm_layout(frame = TRUE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0.2,0.2,0.2,0.2), asp=0)
insetmap  

library(grid)
xy <- st_bbox(atl)
asp <- (xy$ymax - xy$ymin)/(xy$xmax - xy$xmin)
asp2 <- (xy$xmax - xy$xmin)/(xy$ymax - xy$ymin)
w <- 0.25
h <- asp2 * w
vp <- viewport(x=0.27, y=0.99, width = w, height=h, just=c("right", "top"))

## save map of diversity/seg
tmap_save(sitemap, paste0(datadir, "figures/site-map.png"), insets_tm = insetmap, insets_vp = vp, units = 'in', width=6.5, height=6.5)


######## site map of ATL #########
comm.map <- 
  tm_shape(local) + 
    tm_borders(col = "grey40", lwd = 1, lty = "solid", fill_alpha = NA, tm_legend_hide()) +
  tm_shape(filter(shd_bg, shed == 'huc12' & year == '2020')) +
    tm_polygons(col = 'grey70', fill = 'class10', fill.scale = tm_scale_categorical(values = leg_col2),
                tm_legend_hide()) + 
  # tm_shape(rd) + 
  #   tm_lines(col = "gray90") +
  tm_shape(local) +
    tm_borders(col = 'grey20', lwd = 2, lty = 'solid', fill_alpha = NA, tm_legend_hide()) +
    tm_text('HUC_NO', case = 'upper') +
  tm_compass(type = "arrow", size = 3, position = c(0.16, 0.35)) +
  tm_scalebar(breaks = c(0,5), text.size = 0.8, position= c(0.12, 0.17)) +
  tm_add_legend(type = "polygons", labels = lbl2, fill = leg_col2,
                title = paste('Race (Diversity)')) +
  tm_legend(position = c(0.8, 0.3),
            bg.color = "white",
            frame = TRUE,
            frame.lwd = 1,
            legend.text.size = .8,
            legend.title.size = 1) +
  tm_layout(frame = TRUE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=0)
comm.map

## save map of diversity/seg
tmap_save(comm.map, paste0(datadir, "figures/community-map.png"), insets_tm = insetmap, insets_vp = vp, units = 'in', width=6.5, height=6.5)
