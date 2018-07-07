library(tidyverse)
library(sf)

# read in three Huc10 files, and combine.

huc10_10 <- st_read("data/geojson/huc10_bg10.geojson")
huc10_00 <- st_read("data/geojson/huc10_bg00.geojson")
huc10_90 <- st_read("data/geojson/huc10_bg90.geojson")


data = within(data, interval = Upperbound - Lowerbound)
filenames = mget(paste0('DF', 1 : 7))
filenames = lapply(filenames, function (data) within(data, interval = Upperbound - Lowerbound))

huc10 <- rbind(huc10_10, huc10_00)

bind_rows(huc10_10, huc10_00)

vignette("tibble")
huc10 %>%
  filter(YEAR ==)) +
  tm_shape(huc10) +
  tm_polygons("class00")



huc12_10 <- st_read("data/geojson/huc12_bg10.geojson")
huc12_00 <- st_read("data/geojson/huc12_bg00.geojson")
huc12_90 <- st_read("data/geojson/huc12_bg90.geojson")

library(tmap)

tm_shape(huc12_10) +
  tm_fill(col = "class10")

library(tigris)
rd <- primary_roads(year = 2016)

## create custom palette with custom legend labels for seg indices
race_mm_col <- c("#ff9900","#66cc00","#ffcc99", "#99ff99", "#cc99ff","#99752e")
leg_col2 <- c("#ff9900","#66cc00","#ffcc99", "#99ff99", "#cc99ff","#99752e")
lbl2 <- c("White (Low)","Black (Low)","White (Mod)",
          "Black (Mod)","Latinx (Mod)","High Diversity")

rd <- primary_roads(year = 2016)

huc12 <- 
  tm_shape(huc12_10) +
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

str(huc12_10)
