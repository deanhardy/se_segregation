rm(list=ls())

library(tidyverse)
library(tigris)
library(FedData)
library(sf)

atl <-urban_areas('2010') %>%
  st_as_sf() %>%
  filter(NAME10 == 'Atlanta, GA')
