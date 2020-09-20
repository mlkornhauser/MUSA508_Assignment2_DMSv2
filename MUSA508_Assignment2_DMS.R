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

###MK NOTE:  Need to select just the tracts within Pittsburgh City limits

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
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty)%>%
  dplyr::mutate(GEOID = as.numeric(GEOID))

#Generate list of Pittsburgh census tracts
pitt_tracts10 <- read.csv('./data/Pitt_2010_Census_Tractsv2.csv')
names(pitt_tracts10)[names(pitt_tracts10) == "geoid10v2"] <- "GEOID"

#Filter Allegheny County census trcts dataframe to only include Pittsburgh tracts
tracts17 <- tracts17[tracts17$GEOID %in% pitt_tracts10$GEOID,]
plot(tracts17)

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
head(LightRailPGH)

#project Pittsburgh coordinates
LightRailPGH_sf <- st_as_sf(LightRailPGH, coords = c("Lon", "Lat"), crs = 4326) %>%
  st_transform('ESRI:102728')
head(LightRailPGH_sf)

####################
# VISUALIZAING DATA
####################

#2017
ggplot() + 
  geom_sf(data=tracts09) +
  geom_sf(data=LightRailPGH_sf, 
          aes(colour = Direction), 
          show.legend = "point", size= 2) +
  labs(title="Light Rail Stops", 
       subtitle="Pittsburgh, PA", 
       caption="Figure 1") +
  mapTheme()


#2017
ggplot() + 
  geom_sf(data=tracts17) +
  geom_sf(data=LightRailPGH_sf, 
          aes(colour = Direction), 
          show.legend = "point", size= 2) +
  labs(title="Light Rail Stops", 
       subtitle="Pittsburgh, PA", 
       caption="Figure 1") +
  mapTheme()

#Relatingg tracts and subway stops using buffers to understand relationship
Railbuffers <- 
  st_buffer(LightRailPGH_sf, 2640) %>%
  mutate(Legend = "Buffer") %>%
  dplyr::select(Legend)
st_union(st_buffer(LightRailPGH_sf, 2640)) %>%
  st_sf() %>%
  mutate(Legend = "Unioned Buffer")

ggplot() +
  geom_sf(data=Railbuffers) +
  geom_sf(data=LightRailPGH_sf, show.legend = "point") +
  facet_wrap(~Legend) + 
  labs(caption = "Figure 2.6") +
  mapTheme()

#Selecting census tracts within the lighrail station buffers
# 3 methods to select census tracts and their corresponding stops;clip, selection, selection of centroids
buffer <- filter(Railbuffers, Legend=="Unioned Buffer")
#Clip slections
clip <- 
  st_intersection(buffer, tracts09) %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Clip")
#Spatial Selection
selection <- 
  tracts09[buffer,] %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Spatial Selection")
#Select Centroids
selectCentroids <-
  st_centroid(tracts09)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(tracts09, GEOID)) %>%
  st_sf() %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Select by Centroids")

#Putting the buffer selections and tract data together
allTracts.group <- 
  rbind(
    st_centroid(allTracts)[buffer,] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(allTracts)[buffer, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD")) %>%
  mutate(MedRent.inf = ifelse(year == "2009", MedRent * 1.42, MedRent))

ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(TotalPop))) +
  scale_fill_manual(values = palette5,
                    labels = qBr(myData, "TotalPop"),
                    name = "Popluation\n(Quintile Breaks)") +
  labs(title = "Total Population", subtitle = "Pittsburgh; 2009") +
  facet_wrap(~Selection_Type)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

TODMap <- ggplot(allTracts.group, show.legend =FALSE) +
  geom_sf(data=pitt_tracts10, show.legend = FALSE) +
  geom_sf(data=LightRailPGH_sf, show.legend = "Stops") +
  facet_wrap(~Legend) + 
  labs(caption = "Figure 2.7") +
  mapTheme()

ggplot() + 
  geom_sf(data=allTracts.group)
  geom_sf(data=tracts09) +
  geom_sf(data=LightRailPGH_sf, 
          aes(colour = Direction), +
          show.legend = "point", size= 2) +
  labs(title="Light Rail Stops", 
       subtitle="Pittsburgh, PA", 
       caption="Figure 1") +
  mapTheme()


