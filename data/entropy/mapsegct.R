atl15 <- read_csv("data/atl_ct15.csv")
atl10 <- read_csv("data/atl_ct10.csv")
atl_ctread <- read_csv("data/atl_ct.csv")


library(rgdal)
library(sf)

#atl15 <- st_read("Data/arc15.geojson")
#atl10 <- st_read("Data/arc10.geojson")

atl15shp <- st_read("Data/atl15.shp")
atl10shp <- st_read("Data/atl10.shp")

class(atl15shp)

atl15shp$ic <- as.factor(atl15shp$ic)
atl15shp$rc <- as.factor(atl15shp$rc)

library(tmap)

tm_shape(atl15shp) +
  tm_polygons("ic")
tm_shape(atl15shp) +
  tm_polygons("rc")

tm_shape(atl10shp) +
  tm_polygons("ic")
tm_shape(atl10shp) +
  tm_polygons("rc")


tapply(atl15$ic, atl15$ic, length)
tapply(atl10$ic, atl10$ic, length)

tapply(atl15$rc, atl15$rc, length)
tapply(atl10$rc, atl10$rc, length)
#visuals

rAll <- ggplot(data = atl_ctread) +
  geom_bar(mapping = aes(x = YEAR.x, fill = as.factor(rc)),
           position = 'fill') +
  ggtitle("Racial Diversity, 1990-2015") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    x = "Year",
    y = '% block groups')

rAll + scale_fill_brewer(palette = "PRGn", name = "Race")

rAll2 <- rAll + scale_fill_manual(values = c("#ff7f00","#33a02c","#ffffb3","#6a3d9a","#fdbf6f","#b2df8a","#cab2d6","#1f78b4"),
                                  limits = c("2","3","4","7","8","9","13","14"),
                                  name = "Racial diversity X \nDominant racial group",
                                  labels = c("Low diversity, white dominant ",
                                             "Low, Black",
                                             "Low, Asian",
                                             "Low, Latino/a",
                                             "mod, white",
                                             "mod, Black",
                                             "mod, Latino/a",
                                             "high racial diversity"))
rAll2

#ggsave("rAll2.png", width = 3, height = 3)

iAll <- ggplot(data = atl_ctb) +
  geom_bar(mapping = aes(x = YEAR.x, fill = as.factor(ic)),
           position = 'fill') +
  ggtitle("Income Diversity, 1990-15") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    x = "Year",
    y = '% block groups')

iAll + scale_fill_brewer(palette = "RdYlBu", name = "category")

iAll2 <- iAll + scale_fill_manual(values = c("#e31a1c","#33a02c","#ff7f00","#cab2d6","#fdbf6f","#a6cee3","#b2df8a","#fb9a99","#1f78b4"),
                                  limits = c("20","21","22","23","24","25","26","27","28"),
                                  name = "Income diversity X \nDominant income group",
                                  labels = c("Low diversity, low-income", 
                                             "Low, mid-low",
                                             "Low, mid-upper",
                                             "Low, high",
                                             "Mod, low",
                                             "Mod, mid-low",
                                             "Mod, mid-upper", 
                                             "Mod, high",
                                             "high income diversity"))
iAll2







################################
## visuals for DeKalb County ###
################################

dek <- data.frame(county = 89)

#ga3xx <- left_join(ga3, ga3c, by = "GISJOIN")
dkb <- atl_ctread[atl_ctread$COUNTYA %in% dek$county,]
dkb2 <- arcbg2[arcbg2$COUNTYA %in% dek$county,]

rdkb <- ggplot(data = dkb) +
  geom_bar(mapping = aes(x = YEAR.x, fill = as.factor(rc)),
           position = 'fill') +
  ggtitle("Racial Diversity X Segregation,\nDeKalb County,GA 1990-2015") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    x = "Year",
    y = '% block groups')

rdkb + scale_fill_brewer(palette = "PRGn", name = "Race")

rdkb2 <- rdkb + scale_fill_manual(values = c("#ff7f00","#33a02c","#ffffb3","#6a3d9a","#fdbf6f","#b2df8a","#cab2d6","#1f78b4"),
                                  limits = c("2","3","4","7","8","9","13","14"),
                                  name = "Racial diversity X \nDominant racial group",
                                  labels = c("Low diversity, white dominant ",
                                             "Low, Black",
                                             "Low, Asian",
                                             "Low, Latino/a",
                                             "mod, white",
                                             "mod, Black",
                                             "mod, Latino/a",
                                             "high racial diversity"))
rdkb2

idkb <- ggplot(data = dkb) +
  geom_bar(mapping = aes(x = YEAR.x, fill = as.factor(ic)),
           position = 'fill') +
  ggtitle("Income Diversity X Segregation,\nDeKalb County,GA 1990-2015") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    x = "Year",
    y = '% block groups')

idkb + scale_fill_brewer(palette = "RdYlBu", name = "category")

idkb2 <- idkb + scale_fill_manual(values = c("#e31a1c","#33a02c","#ff7f00","#fdbf6f","#a6cee3","#b2df8a","#fb9a99","#1f78b4"),
                                  limits = c("20","21","22","24","25","26","27","28"),
                                  name = "Income diversity X \nDominant income group",
                                  labels = c("Low diversity, low-income", 
                                             "Low, mid-low",
                                             "Low, mid-upper",
                                             "Mod, low",
                                             "Mod, mid-low",
                                             "Mod, mid-upper", 
                                             "Mod, high",
                                             "high income diversity"))
idkb2
