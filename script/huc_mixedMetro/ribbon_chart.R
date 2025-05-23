#### HUC Mixed Metro (aka Mixed Hydro) ####
# created: May 20, 2025
# Authors: Dean Hardy
# git: deanhardy/se_segregation

rm(list=ls())

#### load necessary libraries ####
library(tidyverse)
library(tidycensus)
library(plotly)
library(networkD3)
library(sf)

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/se_segregation/')

#### import and organize data ####

## import results & reshape
# mh <- st_read(paste0(datadir, 'data/spatial/hucMixedMetro.GEOJSON')) %>%
#   st_drop_geometry() %>%
#   filter(shed == 'huc12') %>%
#   select(HUC_NO, year, category) %>%
#   # mutate(name = paste(category, year, sep = '_')) %>%
#   pivot_wider(names_from = year, values_from = category) %>%
#   select(1,5,4,3,2) %>%
#   rename(huc = HUC_NO, y90=2, y00=3, y10=4, y20=5) %>%
#   mutate(name = paste(y90))

## import results & reshape
mh <- st_read(paste0(datadir, 'data/spatial/hucMixedMetro.GEOJSON')) %>%
  st_drop_geometry() %>%
  filter(shed == 'huc12') %>%
  select(HUC_NO, year, category) %>%
  pivot_wider(names_from = year, values_from = category) %>%
  select(1,5,4,3,2) %>%
  rename(huc = HUC_NO, y90=2, y00=3, y10=4, y20=5)

# Function to summarize by consecutive columns
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

nodes <- data.frame(
  name=c(as.character(links$source.ch),
         as.character(links$target.ch)) %>% unique()
) 

links$IDsource <- match(links$source.ch, nodes$name)-1
links$IDtarget <- match(links$target.ch, nodes$name)-1

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
              fontSize= 12, nodeWidth = 30)
fig

saveNetwork(
 fig,
 paste0(datadir, 'figures/sankey1990_2020.html'),
 selfcontained = TRUE
 )

library(webshot)
webshot(paste0(datadir, 'figures/sankey1990_2020.html'),"sankey1020.png", vwidth = 1920, vheight = 1080, zoom = 2)

ggsave(file.path(datadir, 'figures/sankey1990_2020.png'), fig, device = 'png', units = c('in'), width = 7, height = 5)

################################################################
## MANUALLY assess decadal classification changes
################################################################

## assess source and target classifications
## then summarize by transition pairing
## then label source/target with unique identifying number 
# links10 <- mh %>%
#   select(1,2+1,3+1) %>%
#   group_by(across(2), across(3)) %>%
#   summarise(n = n()) %>%
#   # mutate(source.ch = paste0(y90, '90'), target.ch = paste0(y00, '00')) %>%
#   rename(value = n, source.ch = 1, target.ch = 2) %>%
#   ungroup() %>%
#   select(source.ch, target.ch, value) %>%
#   dplyr::arrange((source.ch)) %>%
#   mutate(source.no = consecutive_id(source.ch)-1)  %>%
#   dplyr::arrange((target.ch)) %>%
#   mutate(target.no = consecutive_id(target.ch)+max(source.no),
#          group = if_else(str_length(source.ch) == 5, str_sub(source.ch, 1L, 3L),
#                                if_else(str_length(source.ch) == 4, str_sub(source.ch, 1L, 2L), source.ch))
#          ) %>%
#   select(source.ch, target.ch, source.no, target.no, value, group) %>%
#   arrange(source.ch)
# 
# ## nodes
# nodes1 <- links10 %>% arrange(source.ch) %>% distinct(source.ch) %>% rename (name.ch = 1)
# nodes2 <- links10 %>% arrange(target.ch) %>% distinct(target.ch) %>% rename (name.ch = 1)
# nodes10 <- 
#   rbind(nodes1, nodes2) %>%
#   mutate(name = if_else(str_length(name.ch) == 5, str_sub(name.ch, 1L, 3L),
#                         if_else(str_length(name.ch) == 4, str_sub(name.ch, 1L, 2L), name.ch)),
#          colors = if_else(name == 'HD', "#99752e",
#                           if_else(name == 'LDB', "#66cc00",
#                                   if_else(name == 'MDB', "#99ff99",
#                                           if_else(name == 'LDW', "#ff9900",
#                                                   if_else(name == 'MDW',"#ffcc99", 
#                                                           if_else(name == 'HDL', "#9966ff", 
#                                                                   if_else(name == 'MDL',"#cc99ff",
#                                                                           if_else(name == 'MDA', "#ff9999", name)))))))),
#          group = name
#   ) %>%
#   select(-name.ch)


###########################################
## create Sankey diagram, aka ribbon chart
## https://www.rigordatasolutions.com/post/how-to-create-sankey-diagram-in-r-with-networkd3
## https://plotly.com/r/sankey-diagram/
## https://www.displayr.com/sankey-diagrams-r/
## https://www.sankeyart.com/content/blog/why-a-sankey-diagram-is-the-best-way-to-visualize-an-income-statement/
###########################################

## create colors for decadal transition pairings
# colors <- paste(nodes$colors, collapse = '", "')
# colorJS <- paste('d3.scaleOrdinal(["', colors, '"])')

## plot sankey diagram
# fig <- sankeyNetwork(Links = links, Nodes = nodes,
#               Source = "source.no", Target = "target.no",
#               Value = "value", NodeID = "name",
#               NodeGroup = 'group', LinkGroup = "group",
#               colourScale = colorJS,
#               fontSize= 12, nodeWidth = 30)
# 
# saveNetwork(
#   fig,
#   paste0(datadir, 'figures/sankey1020.html'),
#   selfcontained = TRUE
# )
# 
# library(webshot)
# webshot(paste0(datadir, 'figures/sankey1020.html'),"sankey1020.png", vwidth = 1000, vheight = 900)

# ggsave(file.path(datadir, 'figures/sankey1020.png'), fig, device = 'png', units = c('in'), width = 7, height = 5)
# png(file.path(datadir, 'figures/sankey1020.png'), units = 'in', width = 7, height = 5, res = 150)
# fig
# dev.off()

################################################################################################
## AUTOMATE calculation transition between decades for each pairing through iterative process
## still working out how to do this
################################################################################################
## assess source and target classifications
## then summarize by transition pairing
## then label source/target with unique identifying number 
# links <- mh2 %>%
#   # filter(year %in% c(1990, 2000)) %>%
#   pivot_wider(names_from = year, values_from = category) %>%
#   select(1,5,4,3,2) %>%
#   rename(huc = HUC_NO, y90=2, y00=3, y10=4, y20=5) %>%
#   group_by(across(2), across(3), across(4), across(5)) %>%
#   # summarise_at(.vars = names(.)[2:5],
#   #              .funs = c(n="n"))
#   summarise(y90_00 = n())
# 
#   # mutate(source.ch = paste0(y90, '90'), target.ch = paste0(y00, '00')) %>%
#   rename(value = n, source.ch = 1, target.ch = 2) %>%
#   ungroup() %>%
#   select(source.ch, target.ch, value) %>%
#   dplyr::arrange((source.ch)) %>%
#   mutate(source.no = consecutive_id(source.ch)-1)  %>%
#   dplyr::arrange((target.ch)) %>%
#   mutate(target.no = consecutive_id(target.ch)+max(source.no),
#          group = if_else(str_length(source.ch) == 5, str_sub(source.ch, 1L, 3L),
#                          if_else(str_length(source.ch) == 4, str_sub(source.ch, 1L, 2L), source.ch))
#   ) %>%
#   select(source.ch, target.ch, source.no, target.no, value, group) %>%
#   arrange(source.ch)
# 
# ## nodes
# nodes1 <- links20 %>% arrange(source.ch) %>% distinct(source.ch) %>% rename (name.ch = 1)
# nodes2 <- links20 %>% arrange(target.ch) %>% distinct(target.ch) %>% rename (name.ch = 1)
# nodes20 <- 
#   rbind(nodes1, nodes2) %>%
#   mutate(name = if_else(str_length(name.ch) == 5, str_sub(name.ch, 1L, 3L),
#                         if_else(str_length(name.ch) == 4, str_sub(name.ch, 1L, 2L), name.ch)),
#          colors = if_else(name == 'HD', "#99752e",
#                           if_else(name == 'LDB', "#66cc00",
#                                   if_else(name == 'MDB', "#99ff99",
#                                           if_else(name == 'LDW', "#ff9900",
#                                                   if_else(name == 'MDW',"#ffcc99", 
#                                                           if_else(name == 'HDL', "#9966ff", 
#                                                                   if_else(name == 'MDL',"#cc99ff",
#                                                                           if_else(name == 'MDA', "#ff9999", name)))))))),
#          group = name
#   ) %>%
#   select(-name.ch)
# 

## AI generated script for query
## "dplyr summarize by columns 2 and 3, then 3 and 4, etc"


