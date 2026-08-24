## USGS tutorial online for mapping rivers https://waterdata.usgs.gov/blog/nhd-viz-demo/ using soon to be deprecated nhdplustool package
## hydrogeofetch replacement package vignette https://doi-usgs.github.io/nhdplusTools/articles/hydrogeofetch.html

rm(list=ls())

## define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/se_segregation/')

library(hydrogeofetch)
library(sf)
library(tigris)
library(tidyverse)

## set global parameters
# colors
river_blue <- "#4783A9" # for waterbodies and river lines
city_grey <- "#EEEEEE" # for cities
png_bkg <- "#FFFFFF" # an off-white background
text_color <- "black" 

shd.fill <- c("#ff9900","#66cc00","#ffcc99", "#99ff99", "#cc99ff","#99752e")
shd.lbl <- c("White (Low)","Black (Low)","White (Mod)",
            "Black (Mod)","Latinx (Mod)","High Diversity")
shd.lbl2 <- c("LDW","LDB","MDW","MDB","MDL","HD")

# ggsave parameters
png_width <- 7 #inches
png_height <- 7 #inches
dpi <- 300 #dots per inch

## download ancillary data for map
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

## import watershed results
shd_bg <- st_read(paste0(datadir, 'data/spatial/hucMixedMetro.GEOJSON'))

shd_bg$category <- factor(shd_bg$category, levels = c("LDW","LDB","MDW","MDB","MDL","HD"))

## filter to custom/local watersheds
local <- shd_bg %>% filter(shed == 'local' & year == 2020)

## define start points & import NHD data using hydrogeofetch
coord_list <- list(c(-83.7271944444445, 33.0168611111111), ## Ocmulgee at Dames Ferry gauge #02212735
                   c(-85.1815833333333, 32.8866388888889), ## Chattachoochee near at West Point gauge #02339500
                   c(-84.97875, 34.2093055555556), ## Etowah near Kingston gauge #02395000
                   c(-85.3361388888889, 33.7413888888889), ## Tallapoosa at US 78 gauge #02411930
                   c(-85.25641666666667, 34.200500000000005), ## Coosa near Rome gauge #02397000
                   c(-83.2147777777778, 33.0901666666667), ## Oconee near Milledgeville gauge #02223000
                   c(-84.5266388888889, 33.0476944444444)) ## Flint near Molena gauge #02344872

flowlineOUT <- NULL
waterbodyOUT <- NULL

for (i in seq_along(coord_list)) {
start_point <- st_sfc(st_point(coord_list[[i]]), crs = 4326) 
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid", 
                               featureID = start_comid), 
                          mode = "upstreamTributaries", 
                          distance_km = 1000)

subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.integer(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download", 
                         flowline_only = FALSE,
                         return_data = TRUE, overwrite = TRUE)

flowline <- subset$NHDFlowline_Network
# catchment <- subset$CatchmentSP
waterbody <- subset$NHDWaterbody

flowlineOUT <- rbind(flowlineOUT, flowline)
waterbodyOUT <- rbind(waterbodyOUT, waterbody)
}

## Or using a file:

# flowline <- sf::read_sf(subset_file, "NHDFlowline_Network")
# catchment <- sf::read_sf(subset_file, "CatchmentSP")
# waterbody <- sf::read_sf(subset_file, "NHDWaterbody")

## plot data using Althea Archer's blog post for hydrology dataviz 

# default ggplot2 map
ggplot(data = flowlineOUT) +
  geom_sf() +
  geom_sf(data = start_point, color = "white", fill = "black",
          shape = 21, stroke = 1, size = 3) 

# load custom font
font_title <- 'Source Code Pro'
sysfonts::font_add_google(font_title)
showtext::showtext_opts(dpi = dpi, regular.wt = 200, bold.wt = 700)
showtext::showtext_auto(enable = TRUE)

## set map bbox limits
bbox <- sf::st_bbox(atl)

# update map by adding nice colors and line thicknesses
ggplot() +
  # hydrolines: map stream order categories to factor labels
  # geom_sf(
  #   data = atl, color = text_color,
  #   fill = city_grey, linewidth = 0.01
  # ) +
  geom_sf(
    data = atl, fill = '#eeeeee', color = NA
  ) +
  geom_sf(
    data = cnty, linetype = 'dashed', linewidth = 0.3
  ) +
  geom_sf(
    data = local, fill = NA, color = 'black', linewidth = 0.5
  ) +
  geom_sf(
    data = flowlineOUT,
    aes(
      linewidth = factor(case_when(
        streamorde >= 5 ~ "major",   
        streamorde == 4 ~ "large",
        streamorde == 3 ~ "medium",
        streamorde == 2 ~ "small",
        TRUE ~ "tiny"))
    ),
    color = river_blue) +
  # assign linewidth values using `scale_linewidth_manual()`
  scale_linewidth_manual(
    values = c(
      major = 0.3,
      large = 0.2,
      medium = 0.1,
      small = 0.07,
      tiny = 0.04),
    # hide legend
    guide = "none") +
  geom_sf(data = filter(waterbodyOUT, areasqkm >= 10) , color = river_blue,
          fill  = river_blue, linewidth = 0.01) +
  # force coordinates to match the first layer's explicit limits
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) + 
  # labels 
  geom_sf_text(data = cnty, aes(label = NAME),
               size = 3.5, color = "gray50", 
               family = font_title, fontface = "plain") +
  geom_sf_text(data = local, aes(label = HUC_NO),
               size = 4, color = "gray20", 
               family = font_title, fontface = "bold") +
  # water bodies
  theme_void()

# example of ggsave
ggsave(filename = paste0(datadir, "figures/atl_rivers_map.png"),
       # plot = plot_name, # leave blank to print last map created
       width = png_width, height = png_height, 
       dpi = dpi, units = "in")
