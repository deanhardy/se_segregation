#### HUC Mixed Metro (aka Mixed Hydro) ####
# created: May 20, 2025
# Authors: Dean Hardy
# git: deanhardy/se_segregation
# helpful: https://www.rigordatasolutions.com/post/how-to-create-sankey-diagram-in-r-with-networkd3

rm(list=ls())

#### load necessary libraries ####
library(tidyverse)
library(ggplot2)
library(tidycensus)
library(plotly)
library(networkD3)
library(sf)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/se_segregation/')

## import results & reshape
mh <- st_read(paste0(datadir, 'data/spatial/hucMixedMetro.GEOJSON')) %>%
  st_drop_geometry() %>%
  filter(shed == 'huc12') %>%
  select(HUC_NO, year, category) %>%
  pivot_wider(names_from = year, values_from = category) %>%
  select(1,5,4,3,2) %>%
  rename(huc = HUC_NO, y90=2, y00=3, y10=4, y20=5)

## derived from AI generated script by googling
## "dplyr summarize by columns 2 and 3, then 3 and 4, etc"
# function to summarize by consecutive columns
summarize_consecutive <- function(df, start_col, end_col) {
  df %>%
    group_by(across(start_col:end_col)) %>%
    summarize(n = n()) %>% # Example summary
    mutate(prd = paste(colnames(.[1]), colnames(.[2]), sep = '_')) %>%
    rename(source = 1, target = 2, value = n) %>%
    mutate(source.ch = str_c(str_sub(prd, 1L, 3L), source),
           target.ch = str_c(str_sub(prd, 5L, 7L), target)) %>%
    relocate(prd) %>%
    ungroup() %>%
    group_by(prd) %>%
    mutate(group = source)
}

# Iterate through consecutive column pairs
column_pairs <- list(c(2, 3), c(3, 4), c(4, 5)) # Columns as characters

OUT <- NULL
links <- NULL
for (pair in column_pairs) {
  OUT <- mh %>%
    summarize_consecutive(start_col = pair[1], end_col = pair[2])
  
  links <- rbind(links, OUT)
}

## create nodes
nodes <- data.frame(
  name=c(as.character(links$source.ch),
         as.character(links$target.ch)) %>% unique()
) 

## add unique links pairing each decadal transiion
links$IDsource <- match(links$source.ch, nodes$name)-1
links$IDtarget <- match(links$target.ch, nodes$name)-1

## add Mixed Metro color scheme to nodes
nodes <- nodes %>%
  mutate(across(c('name'), substr, 4, nchar(name)),
         group = name,
         colors = if_else(name == 'HD', "#99752e",
                          if_else(name == 'LDB', "#66cc00",
                                  if_else(name == 'MDB', "#99ff99",
                                          if_else(name == 'LDW', "#ff9900",
                                                  if_else(name == 'MDW',"#ffcc99", 
                                                          if_else(name == 'HDL', "#9966ff", 
                                                                  if_else(name == 'MDL',"#cc99ff",
                                                                          if_else(name == 'MDA', "#ff9999", name)))))))))

## create colors for decadal transition pairings
colors <- paste(nodes$colors, collapse = '", "')
colorJS <- paste('d3.scaleOrdinal(["', colors, '"])')

## plot sankey diagram
fig <- sankeyNetwork(Links = links, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name",
              NodeGroup = 'group', LinkGroup = "group",
              colourScale = colorJS,
              fontSize= 12, nodeWidth = 20)
fig

## export as html file
saveNetwork(
 fig,
 paste0(datadir, 'figures/sankey1990_2020.html'),
 selfcontained = TRUE
 )

library(webshot)
webshot(paste0(datadir, 'figures/sankey1990_2020.html'), paste0(datadir, 'figures/sankey1990_2020.png'), vwidth = 1920, vheight = 1080, zoom = 2)

## NOT WOKRING for some reason
## export as png file 
# ggsave(file.path(datadir, 'figures/sankey1990_2020.png'), fig, device = 'png', units = c('in'), width = 7, height = 5)
