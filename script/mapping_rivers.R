## USGS tutorial online for mapping rivers https://waterdata.usgs.gov/blog/nhd-viz-demo/ using soon to be deprecated nhdplustool package
## hydrogeofetch replacement package vignette https://doi-usgs.github.io/nhdplusTools/articles/hydrogeofetch.html

rm(list=ls())

## define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/se_segregation/')

library(hydrogeofetch)
library(sf)
library(tigris)

## set global parameters
# colors
river_blue <- "#4783A9" # for waterbodies and river lines
city_grey <- "#DDDDDD" # for cities
png_bkg <- "#EDEEEE" # an off-white background
text_color <- "black" 

shd.fill <- c("#ff9900","#66cc00","#ffcc99", "#99ff99", "#cc99ff","#99752e")
shd.lbl <- c("White (Low)","Black (Low)","White (Mod)",
            "Black (Mod)","Latinx (Mod)","High Diversity")
shd.lbl2 <- c("LDW","LDB","MDW","MDB","MDL","HD")

# ggsave parameters
png_width <- 7 #inches
png_height <- 7 #inches
dpi <- 300 #dots per inch

# import ancillary map data
atl <-urban_areas(year = '2020') %>%
  filter(NAME10 == 'Atlanta, GA') %>%
  mutate(name = 'Atlanta')

## import results
shd_bg <- st_read(paste0(datadir, 'data/spatial/hucMixedMetro.GEOJSON'))

shd_bg$category <- factor(shd_bg$category, levels = c("LDW","LDB","MDW","MDB","MDL","HD"))

## define start points & import NHD data using hydrogeofetch
coord_list <- list(c(-83.8373611111111, 33.3126111111111), ## Ocmulgee near Jackson gauge #02210500
                   c(-84.9011944444445, 33.4765277777778), ## Chattachoochee near Whitesburg gauge #02338000
                   c(-84.97875, 34.2093055555556), ## Etowah near Kingston gauge #02395000
                   c(-84.5266388888889, 33.0476944444444)) ## Flint near Molena gauge #02344872

flowlineOUT <- NULL

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
# waterbody <- subset$NHDWaterbody

flowlineOUT <- rbind(flowlineOUT, flowline)
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

# update map by adding nice colors and line thicknesses
ggplot() +
  # hydrolines: map stream order categories to factor labels
  # geom_sf(
  #   data = atl, color = text_color,
  #   fill = city_grey, linewidth = 0.01
  # ) +
  geom_sf(
    data = filter(shd_bg, year == 2020), aes(fill = category)
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
  # water bodies
  # geom_sf(data = waterbody, color = river_blue,
  #         fill  = river_blue, linewidth = 0.01) +
  # geom_sf(data = waterbody2, color = river_blue,
  #         fill  = river_blue, linewidth = 0.01) +
  # dot in the middle for our main location of interest
  # geom_sf(data = start_point, color = "white", fill = "black",
  #         shape = 21, stroke = 1, size = 3) +
  theme_void()

# example of ggsave
ggsave(filename = paste0(datadir, "figures/atl_rivers_map.png"),
       # plot = plot_name, # leave blank to print last map created
       width = png_width, height = png_height, 
       dpi = dpi, units = "in")



## download ancillary data for map
rd <- primary_roads(year = 2022) %>%
  filter(RTTYP == 'I')
cnty <- counties(state = 'GA')
cnty_list <- list_counties('GA')
arc_list <- c("Cherokee","Clayton","Cobb", "DeKalb", "Douglas",
              "Fayette","Forsyth", "Fulton","Gwinnett", "Henry"
              # "Rockdale"
)
arc <- filter(cnty_list, county %in% arc_list)
sts <- states(year = 2020) 
ga <- filter(sts, STUSPS == "GA")

