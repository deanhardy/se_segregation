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

#define data directory
datadir <- file.path('/Users/dhardy/Dropbox/r_data/se_segregation/')

#### import and organize data ####

## import results & reshape
mh <- st_read(paste0(datadir, 'data/spatial/hucMixedMetro.GEOJSON')) %>%
  st_drop_geometry() %>%
  filter(shed == 'huc12') %>%
  select(HUC_NO, year, category) %>%
  # mutate(name = paste(category, year, sep = '_')) %>%
  pivot_wider(names_from = year, values_from = category)

## reorder years and rename columns
mh <- mh[,c(1,5,4,3,2)]
mh2 <- mh%>%
  rename(huc = HUC_NO, y90=2, y00=3, y10=4, y20=5) %>%
  mutate(name = paste(y90))

## calculate transition between decades for each pairing
## definitely better way to do this, transition matrices?
links1 <- mh2 %>%
  select(1:3) %>%
  group_by(y90, y00) %>%
  summarise(n = n()) %>%
  mutate(source = paste0(y90, '90'), target = paste0(y00, '00')) %>%
  rename(value = n) %>%
  ungroup() %>%
  select(source, target, value) %>%
  dplyr::arrange((source)) %>%
  mutate(source2 = consecutive_id(source)-1)

nodes1 <- data.frame(name = unique(links1$source))

links2 <- mh2 %>%
  select(1:3) %>%
  group_by(y90, y00) %>%
  summarise(n = n()) %>%
  mutate(source = paste0(y90, '90'), target = paste0(y00, '00')) %>%
  rename(value = n) %>%
  ungroup() %>%
  select(source, target, value) %>%
  dplyr::arrange((source)) %>%
  mutate(source2 = consecutive_id(source)-1) %>%
  dplyr::arrange((target)) %>%
  mutate(target2 = consecutive_id(target)+3)

nodes2 <- data.frame(name = unique(links2$target))

nodes <- rbind(nodes1, nodes2) %>%
  mutate(name2 = str_sub(name, 1L, 3L),
         group = str_sub(name, 1L, 3L),
         colors = c("#66cc00", "#ff9900","#99ff99", "#ffcc99",
                    "#99752e", "#66cc00", "#ff9900","#99ff99", "#ffcc99"))

links <- links2 %>%
  select(source2, target2, value) %>%
  arrange(source2) %>%
  mutate(group = str_sub(links1$source, 1L, 3L))

# mh0010 <- mh2 %>%
#   select(huc,y00,y10) %>%
#   group_by(y00,y10) %>%
#   summarise(n = n())
# 
# mh1020 <- mh2 %>%
#   select(huc,y10,y20) %>%
#   group_by(y10,y20) %>%
#   summarise(n = n())

## merge all three transitions


## define colors
clr <- c("#ff9900","#66cc00", "#9966ff",
                   "#ffcc99", "#99ff99", "#ff9999","#cc99ff",
                   "#99752e")

## create Sankey diagram, aka ribbon chart
## https://plotly.com/r/sankey-diagram/
## https://www.displayr.com/sankey-diagrams-r/
## https://www.sankeyart.com/content/blog/why-a-sankey-diagram-is-the-best-way-to-visualize-an-income-statement/
# fig <- 
#   plot_ly(
#     data = mh9000,
#     type = "sankey",
#     orientation = "h",
#     node = list(
#         label = y00,
#         color = clr,
#         pad = 15,
#         thickness = 20,
#         line = list(
#             color = "black",
#             width = 0.5
#           )
#       ),
#       link = list(
#         source = y90,
#         target = y00,
#         value = n
#       )
#   )

colors <- paste(nodes$colors, collapse = '", "')
colorJS <- paste('d3.scaleOrdinal(["', colors, '"])')
##
# nodes = data.frame("name" = 
#                      c("Node A", # Node 0
#                        "Node B", # Node 1
#                        "Node C", # Node 2
#                        "Node D"))# Node 3
# links = as.data.frame(matrix(c(
#   0, 1, 10, # Each row represents a link. The first number
#   0, 2, 20, # represents the node being conntected from. 
#   1, 3, 30, # the second number represents the node connected to.
#   2, 3, 40),# The third number is the value of the node
#   byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value", 'group')
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name2",
              NodeGroup = 'group', LinkGroup = "group",
              colourScale = colorJS,
              fontSize= 12, nodeWidth = 30)
