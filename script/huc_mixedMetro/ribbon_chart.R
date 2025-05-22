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
  select(1,2+2,3+2) %>%
  group_by(across(2), across(3)) %>%
  summarise(n = n()) %>%
  # mutate(source.ch = paste0(y90, '90'), target.ch = paste0(y00, '00')) %>%
  rename(value = n, source.ch = 1, target.ch = 2) %>%
  ungroup() %>%
  select(source.ch, target.ch, value) %>%
  dplyr::arrange((source.ch)) %>%
  mutate(source.no = consecutive_id(source.ch)-1)  %>%
  dplyr::arrange((target.ch)) %>%
  mutate(target.no = consecutive_id(target.ch)+max(source.no),
         group = if_else(str_length(source.ch) == 5, str_sub(source.ch, 1L, 3L),
                               if_else(str_length(source.ch) == 4, str_sub(source.ch, 1L, 2L), source.ch))
         ) %>%
  select(source.ch, target.ch, source.no, target.no, value, group) %>%
  arrange(source.ch)

## nodes
nodes1 <- links %>% arrange(source.ch) %>% distinct(source.ch) %>% rename (name.ch = 1)
nodes2 <- links %>% arrange(target.ch) %>% distinct(target.ch) %>% rename (name.ch = 1)
nodes <- 
  rbind(nodes1, nodes2) %>%
  mutate(name = if_else(str_length(name.ch) == 5, str_sub(name.ch, 1L, 3L),
                        if_else(str_length(name.ch) == 4, str_sub(name.ch, 1L, 2L), name.ch)),
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
  select(-name.ch)


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
fig <- sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source.no", Target = "target.no",
              Value = "value", NodeID = "name",
              NodeGroup = 'group', LinkGroup = "group",
              colourScale = colorJS,
              fontSize= 12, nodeWidth = 30)

saveNetwork(
  fig,
  paste0(datadir, 'figures/sankey1020.html'),
  selfcontained = TRUE
)

library(webshot)
webshot(paste0(datadir, 'figures/sankey1020.html'),"sankey1020.png", vwidth = 1000, vheight = 900)

# ggsave(file.path(datadir, 'figures/sankey1020.png'), fig, device = 'png', units = c('in'), width = 7, height = 5)
# png(file.path(datadir, 'figures/sankey1020.png'), units = 'in', width = 7, height = 5, res = 150)
# fig
# dev.off()

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
#   mutate(s.ch = S[[i]], t.ch = T[[z]])
# 
# names(OUT) = c("source", "target", "value", 's.ch', 't.ch')
# 
# lk.df <- rbind(OUT, lk.df)
#   }}


