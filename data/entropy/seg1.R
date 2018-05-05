


library(tidyverse)
library(data.table)
library(stargazer)


gabg10 <- read.csv("Data/nhgis0057_ds176_20105_2010_gatract.csv")
gabg10a <- read.csv("data/nhgis0058_ds176_20105_2010_gatract.csv")

gabg15 <- read.csv("Data/nhgis0057_ds215_20155_2015_gatract.csv")
gabg15a <- read.csv("data/nhgis0058_ds215_20155_2015_gatract.csv")

# clean up df's. cut MoE and uneccessary, blank, and repetitive columns
gabg10 <- gabg10[,-(c(3:5,9,10,13:35,60:83))]
gabg10a <- gabg10a[,-(c(2:36,58:79))]

gabg15 <- gabg15[,-(c(3:5,9,10,13:36,61:84))]
gabg15a <- gabg15a[,-(c(2:37,59:80))]

#merge and remove uneccesary df's
gabg10 <- left_join(gabg10,gabg10a, by = "GISJOIN")
rm(gabg10a)

gabg15 <- left_join(gabg15,gabg15a, by = "GISJOIN")
rm(gabg15a)


###############################################################
####### calculate housing tenure and poverty rates
################################################################

# housing tenure for 2006-10
gabg10$pctrent <- gabg10$JRKE003/gabg10$JRJE001*100
gabg10$pctown <- gabg10$JRKE002/gabg10$JRJE001*100
gabg10$pctrentocc <- gabg10$JRKE003/gabg10$JRKE001*100
gabg10$pctownocc <- gabg10$JRKE002/gabg10$JRKE001*100
gabg10$pctvac <- gabg10$JRJE003/gabg10$JRJE001*100
gabg10$pctocc <- gabg10$JRJE002/gabg10$JRJE001*100

# housing tenure for 2011-15
gabg15$pctrent <- gabg15$ADP0E003/gabg15$ADPZE001*100
gabg15$pctown <- gabg15$ADP0E002/gabg15$ADPZE001*100
gabg15$pctrentocc <- gabg15$ADP0E003/gabg15$ADP0E001*100
gabg15$pctownocc <- gabg15$ADP0E002/gabg15$ADP0E001*100
gabg15$pctvac <- gabg15$ADPZE003/gabg15$ADPZE001*100
gabg15$pctocc <- gabg15$ADPZE002/gabg15$ADPZE001*100

##############################################################
######### segregation X diversity (RACE) Entropy Calculation
##############################################################

#2006-10 BG entropoy calculation
gabg10$asian <- gabg10$JMJE006 + gabg10$JMJE007
gabg10$other <- gabg10$JMJE008 + gabg10$JMJE018

gabg10$pctwht <- (gabg10$JMJE003/gabg10$JMJE001)
gabg10$pctblk <- (gabg10$JMJE004/gabg10$JMJE001)
gabg10$pcthisp <- (gabg10$JMJE012/gabg10$JMJE001)
gabg10$pctntv <- (gabg10$JMJE005/gabg10$JMJE001)
gabg10$pctoth <- (gabg10$other/gabg10$JMJE001)
gabg10$pctasi <- (gabg10$asian/gabg10$JMJE001)

gabg10[, 61:66][gabg10[, 61:66] == 0] <- 0.0000000001

gabg10$E <- -((gabg10$pctwht*log(gabg10$pctwht) + gabg10$pctblk*log(gabg10$pctblk) + gabg10$pcthisp*log(gabg10$pcthisp) + gabg10$pctasi *log(gabg10$pctasi) + gabg10$pctntv*log(gabg10$pctntv)))
gabg10$E <- gabg10$E/log(5)


#2011-15 BG entropoy calculation
gabg15$asi <- gabg15$ADK5E006 + gabg15$ADK5E007
gabg15$oth <- gabg15$ADK5E008 + gabg15$ADK5E018

gabg15$pctwht <- (gabg15$ADK5E003/gabg15$ADK5E001)
gabg15$pctblk <- (gabg15$ADK5E004/gabg15$ADK5E001)
gabg15$pcthisp <- (gabg15$ADK5E012/gabg15$ADK5E001)
gabg15$pctntv <- (gabg15$ADK5E005/gabg15$ADK5E001)
gabg15$pctoth <- (gabg15$oth/gabg15$ADK5E001)
gabg15$pctasi <- (gabg15$asi/gabg15$ADK5E001)

gabg15[, 61:66][gabg15[, 61:66] == 0] <- 0.0000000001

gabg15$E <- -((gabg15$pctwht*log(gabg15$pctwht) + gabg15$pctblk*log(gabg15$pctblk) + gabg15$pcthisp*log(gabg15$pcthisp) + gabg15$pctasi *log(gabg15$pctasi) + gabg15$pctntv*log(gabg15$pctntv)))
gabg15$E <- gabg15$E/log(5)


###############################################
######  Household income entropy #############
###############################################

# Income entropy for 2006-10 - all GA

gabg10c <- gabg10[9:25]
gabg10c <- gabg10c[1:17]/gabg10c$JOHE001
gabg10c[, 1:17][gabg10c[, 1:17] == 0] <- 0.0000000001
gabg10c <- cbind(gabg10$GISJOIN,gabg10c)

#2006-10
gabg10c$low <- rowSums(gabg10c[3:7])
gabg10c$mlow <- rowSums(gabg10c[8:13]) 
gabg10c$mup <- rowSums(gabg10c[14:16])
gabg10c$upp <- rowSums(gabg10c[17:18])

gabg10c$iE <- -1*(rowSums(gabg10c[19:22]*log(gabg10c[19:22])))
gabg10c$iE <- gabg10c$iE /log(4)

#gabg10c$iE10 <- -1*(rowSums(gabg10c[2:17]*log(gabg10c[2:17])))
#gabg10c$iE10 <- gabg10c$iE/log(16)

# Income entropy for 2011-15 - all CO
gabg15c <- gabg15[9:25]
gabg15c <- gabg15c[1:17]/(gabg15c$ADNJE001)
gabg15c <- cbind(gabg15$GISJOIN,gabg15c)
gabg15c[, 1:17][gabg15c[, 1:17] == 0] <- 0.0000000001

gabg15c$low <- rowSums(gabg15c[3:7])
gabg15c$mlow <- rowSums(gabg15c[8:13]) 
gabg15c$mup <- rowSums(gabg15c[14:16])
gabg15c$upp <- rowSums(gabg15c[17:18])

gabg15c$iE <- -1*(rowSums(gabg15c[19:22]*log(gabg15c[19:22])))
gabg15c$iE <- gabg15c$iE /log(4)

#gabg15c$iE15 <- -1*(rowSums(gabg15c[2:17]*log(gabg15c[2:17])))
#gabg15c$iE15 <- gabg15c$iE/log(16)

#testing things out
gabg15$YEAR <- as.integer(2015)
gabg10$YEAR <- as.integer(2010)
str(gabg10)
str(gabg15)

ga3 <- bind_rows(gabg15,gabg10)
#rm(ga1)
#ga2 <- bind_rows(ga1,gabg00)
#ga3 <- bind_rows(ga2,gabg90)


##############################################################
##### assign 'classes' for segregation X diversity - RACE ##
##############################################################

setDT(ga3)[ga3$E <= .3707 & ga3$pctwht > .65 | ga3$pctwht >= .8,  rc := 2 ]
ga3[(E <= .3707 & pctblk > .65) | pctblk >= .8,  rc := 3 ]
ga3[(E > .7414 & pctwht < .46 & pctblk < .46), rc := 14]
ga3[E > .3707 & E < .7414 & pctwht < .8 & pctwht > pctblk & pctwht > pcthisp, rc := 8]
ga3[is.na(rc) & pctwht >.46 & pctwht <.8, rc :=8]
ga3[E > .3707 & E < .7414 & pctblk < .8 & pctblk > pctwht & pctblk > pcthisp, rc := 9]
ga3[is.na(rc) & pctblk >.46 & pctblk <.8, rc :=9]
ga3[E > .3707 & E < .7414 & pcthisp < .8 & pcthisp > pctwht & pcthisp > pctblk, rc := 13]
ga3[(E <= .3707 & pcthisp > .65) | pcthisp >= .8,  rc := 7 ]
ga3[is.na(rc) & pcthisp >.46 & pcthisp <.8, rc :=13]
ga3[(E <= .3707 & pctoth > .65) | pctoth >= .8,  rc := 4 ]
ga3[(E <= .3707 & pctntv > .65) | pctntv >= .8,  rc := 5 ]

arc <- data.frame(county = c(57,63,67,89,97,113,121,135,151,247))
atl_ct <- ga3[ga3$COUNTYA %in% arc$county,]
atl_ct$rc <- as.factor(atl_ct$rc)

summary(as.factor(atl_ct$rc))

atl_ct %>%
  group_by(YEAR) %>%
  summarize(
    count = n(),
    b2 = sum(rc == 2, na.rm = TRUE)
  )

tapply(atl_ct$rc, atl_ct$rc, length)

atl_ct$YEAR <- as.factor(atl_ct$YEAR)

atl_ct15 <- atl_ct %>%
  filter(YEAR == 2015
  )
atl_ct10 <- atl_ct %>%
  filter(YEAR == 2010
  )

write_csv(atl_ct15, "data/atl_ct15.csv")
write_csv(atl_ct10, "data/atl_ct10.csv")

atl_ct10 %>%
  summarize(
    count = n(),
    b2 = sum(rc == 2, na.rm = TRUE)
  )

#### onto income

#testing things out
gabg15c$YEAR <- as.integer(2015)
gabg10c$YEAR <- as.integer(2010)

ga3c <- bind_rows(gabg15c,gabg10c)
#ga2c <- bind_rows(ga1c,gabg00c)
#ga3c <- bind_rows(ga2c,gabg90c)

#####################################################
####### Create classes seg x diversity - INCOME #####
####################################################

# Median HH income 2011-15 Metro: $ 53,889
# threshold above considers 29,999 HH income and below and 'low-income' (< .6x med HH income)
# $30k - 74,999 = 'low-mid' (.6x Med HH - 1.5x med HH income)
# 75,000 - 149,000 = 'mid-upper' (1.5x - 3x med HH income)
# and above $150,000 as 'high-income' (> 3x med HH income)
#https://www.census.gov/sedenh-results.html?stateGeo=none&q=median+household+income&sedenhtype=web&page=9

setDT(ga3c)[(ga3c$iE <= .4238 & ga3c$low > .65) | ga3c$low >= .8,  ic := 20 ]
ga3c[(iE <= .4238 & upp > .65) | upp >= .8,  ic := 23 ]
ga3c[(iE <= .4238 & mup > .65) | mup >= .8,  ic := 22 ]
ga3c[(iE <= .4238 & mlow > .65) | mlow >= .8 ,  ic := 21 ]
ga3c[iE > .4238 & iE < .9 & low < .8 & low > mlow & low > mup & low > upp ,ic := 24]
ga3c[iE > .4238 & iE < .9 & upp < .8 & upp > low & upp > mlow & upp > mup, ic := 27]
ga3c[iE > .4238 & iE < .9 & mup < .8 & mup > low & mup > mlow & mup > upp, ic := 26]
ga3c[iE > .4238 & iE < .9 & mlow < .8 & mlow > low & mlow > mup & mlow > upp, ic := 25]
ga3c[(iE >= .9 & low < .46 & mlow < .46 & mup < .46 & upp < .46), ic := 28]
ga3c[is.na(ic) & low >.46 & low <.8, ic :=24]
ga3c[is.na(ic) & upp >.46 & upp <.8, ic :=27]
ga3c[is.na(ic) & mup >.46 & mup <.8, ic :=26]
ga3c[is.na(ic) & mlow >.46 & mlow <.8, ic :=25]

ga3c <- rename(ga3c, GISJOIN = `gabg15$GISJOIN`)
ga3c$COUNTYA <- ga3$COUNTYA

ga3x <- left_join(ga3, ga3c, by = "GISJOIN")

atl_ctb <- ga3x[ga3x$COUNTYA %in% arc$county,]

class(arcbg2$ic)
arcbg2$ic <- as.factor(arcbg2$ic)
class(arcbg2$ic)
summary(as.factor(arcbg2$ic))

atl_ctb %>%
  group_by(YEAR.x) %>%
  summarize(
    count = n(),
    b2 = sum(rc == 2, na.rm = TRUE)
  )




tapply(arcbg2$ic, arcbg2$ic, length)

arcbg2$YEAR <- as.factor(arcbg$YEAR)
ggplot(data = arcbg2) +
  geom_bar(mapping = aes(x = YEAR, fill = as.factor(ic)))

atl_ct15 <- atl_ctb %>%
  filter(YEAR.x == 2015
  )

atl_ct10 <- atl_ctb %>%
  filter(YEAR.x == 2010
  )

write_csv(atl_ct15, "data/atl_ct15.csv")
write_csv(atl_ct10, "data/atl_ct10.csv")
write_csv(atl_ctb, "data/atl_ct.csv")

library(rgdal)
library(sf)

#atl15 <- st_read("Data/arc15.geojson")
#atl10 <- st_read("Data/arc10.geojson")

atl_shp <- st_read("Data/atl15.shp")
atl10 <- st_read("Data/atl10.shp")

class(atl_shp)

atl_shp$ic <- as.factor(atl_shp$ic)
atl_shp$rc <- as.factor(atl_shp$rc)

library(tmap)

tm_shape(atl_shp) +
  tm_polygons("ic")
tm_shape(atl_shp) +
  tm_polygons("rc")

tm_shape(atl10) +
  tm_polygons("ic")
tm_shape(atl10) +
  tm_polygons("rc")

#visuals

rAll <- ggplot(data = atl_ct) +
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

iAll <- ggplot(data = arcbg2) +
  geom_bar(mapping = aes(x = YEAR, fill = as.factor(ic)),
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
dkb <- arcbg[arcbg$COUNTYA %in% dek$county,]
dkb2 <- arcbg2[arcbg2$COUNTYA %in% dek$county,]

rdkb <- ggplot(data = dkb) +
  geom_bar(mapping = aes(x = YEAR, fill = as.factor(rc)),
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

idkb <- ggplot(data = dkb2) +
  geom_bar(mapping = aes(x = YEAR, fill = as.factor(ic)),
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
