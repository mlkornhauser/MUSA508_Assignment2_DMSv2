# MUSA 508 Assignment 1
# TOD in Pittsburgh, PA
# Divya, Maddy & Sophia

###########
# SET UP
###########

# Load Libraries

library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)
library(sp)
library(rgdal) 

options(scipen=999)
options(tigris_class = "sf")

# ---- Load Styling options -----

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

# Load Quantile break functions

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

# Load hexadecimal color palette

palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac")

# Load census API key

census_api_key("53845fc057d94b7ce8d243f50bd9fa0d9237c612", overwrite = TRUE)

#########################
# 2009 & 2017 CENSUS DATA
#########################

#Pulling 2009 Census data for Allegheny County
tracts09 <-  
  get_acs(geography = "tract", variables = c("B25026_001E", "B02001_002E", "B15001_050E", "B15001_009E",
                                             "B19013_001E", "B25058_001E", "B06012_002E"), 
          year=2009, state=42, county=003, geometry=T, output = "wide") %>% 
  st_transform('ESRI:102728')%>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2017") %>%
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty)  

#MK Note:  Need to select just the tracts within Pittsburgh City limits

summary(tracts09)

#Pulling 2017 Census data for Allegheny County
tracts17 <-  
  get_acs(geography = "tract", variables = c("B25026_001E", "B02001_002E", "B15001_050E", "B15001_009E",
                                             "B19013_001E", "B25058_001E", "B06012_002E"), 
          year=2017, state=42, county=003, geometry=T, output = "wide") %>% 
  st_transform('ESRI:102728') %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2017") %>%
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty)

#MK Note:  Need to select just the tracts within Pittsburgh City limits

head(tracts17)
summary(tracts17)

# Combine 2009 and 2017 census data into single dataframe

allTracts <- rbind(tracts09,tracts17)
summary(allTracts)

###############
# TRANSIT DATA
###############
#Load in lightrail data and rename column headers
LightRailPGH <- read.csv('./data/LightRailPGH.csv')%>%
  select(Stop_name.C.254, Direction.C.254, Routes_ser.C.254, Latitude.N.19.11, Longitude.N.19.11) %>%
  rename(Stopname=Stop_name.C.254, 
         Direction=Direction.C.254, 
         Routes=Routes_ser.C.254, 
         Lat=Latitude.N.19.11, 
         Lon=Longitude.N.19.11) 

#Convert lat and longitude to point geometry
#Stuck on this
#latlong_sf <- st_as_sf(LightRailPGH, coords = ___, crs = ___)


xy <- as.data.frame(cbind(X = LightRailPGH$Lat, Y = LightRailPGH$Lon))
coordinates(xy) <- c("X", "Y")
proj4string(xy) <- CRS("+proj=longlat +datum=WGS84") 
res <- spTransform(xy, CRS("+proj=utm +zone=51 ellps=WGS84"))
head(res)


