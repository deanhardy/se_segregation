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