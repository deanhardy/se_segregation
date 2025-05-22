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
mh <- st_read(paste0(datadir, 'data/spatial/hucMixedMetro.GEOJSON')) %>%
  st_drop_geometry() %>%
  filter(shed == 'huc12') %>%
  select(HUC_NO, year, category) %>%
  # mutate(name = paste(category, year, sep = '_')) %>%
  pivot_wider(names_from = year, values_from = category) %>%
  select(1,5,4,3,2) %>%
  rename(huc = HUC_NO, y90=2, y00=3, y10=4, y20=5) %>%
  mutate(name = paste(y90))

################################################################
## MANUALLY assess decadal classification changes
################################################################

## assess source and target classifications
## then summarize by transition pairing
## then label source/target with unique identifying number 
links <- mh %>%
  select(1,2,3) %>%
  group_by(y90, y00) %>%
  summarise(n = n()) %>%
  mutate(source.yr = paste0(y90, '90'), target.yr = paste0(y00, '00')) %>%
  rename(value = n) %>%
  ungroup() %>%
  select(source.yr, target.yr, value) %>%
  dplyr::arrange((source.yr)) %>%
  mutate(source = consecutive_id(source.yr)-1)  %>%
  dplyr::arrange((target.yr)) %>%
  mutate(target = consecutive_id(target.yr)+max(source),
         group = if_else(str_length(source.yr) == 5, str_sub(source.yr, 1L, 3L),
                               if_else(str_length(source.yr) == 4, str_sub(source.yr, 1L, 2L), source.yr))
         ) %>%
  select(source.yr, target.yr, source, target, value, group) %>%
  arrange(source)

## nodes
nodes1 <- links %>% distinct(source.yr) %>% rename (name.yr = 1)
nodes2 <- links %>% arrange(target.yr) %>% distinct(target.yr) %>% rename (name.yr = 1)
nodes <- 
  rbind(nodes1, nodes2) %>%
  mutate(name = if_else(str_length(name.yr) == 5, str_sub(name.yr, 1L, 3L),
                        if_else(str_length(name.yr) == 4, str_sub(name.yr, 1L, 2L), name.yr)),
         colors = if_else(name == 'HD', "#99752e",
                          if_else(name == 'LDB', "#66cc00",
                                  if_else(name == 'MDB', "#99ff99",
                                          if_else(name == 'LDW', "#ff9900",
                                                  if_else(name == 'MDW',"#ffcc99", 
                                                          if_else(name == 'HDL', "#9966ff", 
                                                                  if_else(name == 'MDL',"#cc99ff",
                                                                          if_else(name == 'MDA', "#ff9999", name)))))))),
         group = name
  ) %>%
  select(-name.yr)


###########################################
## create Sankey diagram, aka ribbon chart
## https://plotly.com/r/sankey-diagram/
## https://www.displayr.com/sankey-diagrams-r/
## https://www.sankeyart.com/content/blog/why-a-sankey-diagram-is-the-best-way-to-visualize-an-income-statement/
###########################################

## create colors for decadal transition pairings
colors <- paste(nodes$colors, collapse = '", "')
colorJS <- paste('d3.scaleOrdinal(["', colors, '"])')

## plot sankey diagram
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              NodeGroup = 'group', LinkGroup = "group",
              colourScale = colorJS,
              fontSize= 12, nodeWidth = 30)


################################################################################################
## AUTOMATE calculation transition between decades for each pairing through iterative process
## still working out how to do this
################################################################################################
# S <- c('y00')
# T <- c('y10')
# OUT <- NULL
# lk.df <- NULL # used in loop
# 
# for (i in seq_along(S)) {
#   for (z in seq_along(T)) {
#   
# OUT <- mh2 %>%
#   select(huc, S[[i]], T[[z]]) %>%
#   group_by(pick(2),pick(3)) %>%
#   summarise(n = n()) %>%
#   ungroup() %>%
#   mutate(s.yr = S[[i]], t.yr = T[[z]])
# 
# names(OUT) = c("source", "target", "value", 's.yr', 't.yr')
# 
# lk.df <- rbind(OUT, lk.df)
#   }}


