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

v09 <- load_variables(2009, "acs5", cache = TRUE)
View(v09)

#Pulling 2009 Census data for Allegheny County
tracts09 <-  
  get_acs(geography = "tract", variables = c("B25026_001E", "B02001_002E", "B01002_001E", 
                                             "B19013_001E", "B25058_001E"), 
          year=2009, state=42, county=003, geometry=T, output = "wide") %>% 
  st_transform('ESRI:102728')%>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         MedAge = B01002_001E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         year = "2009") %>%
  dplyr::select(-Whites)  

summary(tracts09)

###MK NOTE:  Need to select just the tracts within Pittsburgh City limits

#boundaries <- st_read("./data/Pittsburgh_City_Boundary.geojson") %>%
#  st_transform(st_crs(tracts09))

#tracts_pitt00 <- 
#  tracts09[boundaries,]

#plot(boundaries)
#plot(tracts_pitt00)
#summary(tracts09)

#Pulling 2017 Census data for Allegheny County
tracts17 <-  
  get_acs(geography = "tract", variables = c("B25026_001E", "B02001_002E", "B01002_001E", 
                                             "B19013_001E", "B25058_001E"), 
          year=2017, state=42, county=003, geometry=T, output = "wide") %>% 
  st_transform('ESRI:102728') %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         MedAge = B01002_001E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         year = "2017") %>%
  dplyr::select(-Whites)%>%
  dplyr::mutate(GEOID = as.numeric(GEOID))

summary(tracts17)

#Generate list of Pittsburgh census tracts
#pitt_tracts10 <- read.csv('./data/Pitt_2010_Census_Tractsv2.csv')
#names(pitt_tracts10)[names(pitt_tracts10) == "geoid10v2"] <- "GEOID"

#Filter Allegheny County census trcts dataframe to only include Pittsburgh tracts
#tracts17 <- tracts17[tracts17$GEOID %in% pitt_tracts10$GEOID,]
#plot(tracts17)

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

#2009
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

buffer <- st_union(st_buffer(LightRailPGH_sf, 2640)) %>%
  st_sf() %>%
  mutate(Legend = "Unioned Buffer")

ggplot() +
  geom_sf(data = buffer)

ggplot() +
  geom_sf(data=Railbuffers) +
  geom_sf(data=LightRailPGH_sf, show.legend = "point") +
  facet_wrap(~Legend) + 
  labs(caption = "Figure 2.6") +
  mapTheme()

#Selecting census tracts within the lightrail station buffers
# 3 methods to select census tracts and their corresponding stops;clip, selection, selection of centroids

#Select Centroids
selectCentroids09 <-
  st_centroid(tracts09)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(tracts09, GEOID)) %>%
  st_sf() %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Select by Centroids")

plot(selectCentroids09)

selectCentroids17 <-
  st_centroid(tracts17)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(tracts17, GEOID)) %>%
  st_sf() %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Select by Centroids")

plot(selectCentroids17)

#Putting the buffer selections and tract data together
allTracts.group <- 
  rbind(
    st_centroid(allTracts)[buffer,] %>%
      st_drop_geometry() %>%
      left_join(dplyr::select(allTracts, GEOID)) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(allTracts)[buffer, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(dplyr::select(allTracts, GEOID)) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD")) %>%
  mutate(MedRent.inf = ifelse(year == "2009", MedRent * 1.42, MedRent))

#Median rent maps: 2009 to 2017

ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(MedRent))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "MedRent"),
                    name = "Median Rent\n(Quintile Breaks)") +
  labs(title = "Median Rent 2009-2017", subtitle = "Real Dollars") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

  
  #Percent White population map 2009 to 2017
  
  ggplot(allTracts.group)+
    geom_sf(data = st_union(tracts09))+
    geom_sf(aes(fill = q5(pctWhite))) +
    geom_sf(data = buffer, fill = "transparent", color = "red")+
    scale_fill_manual(values = palette5,
                      labels = qBr(allTracts.group, "pctWhite"),
                      name = "Population Percent White\n(Quintile Breaks)") +
    labs(title = "Population Percent White 2009-2017", subtitle = "Real Dollars") +
    facet_wrap(~year)+
    mapTheme() + 
    theme(plot.title = element_text(size=22))
  
  
  #Median Household Income maps 2009-2017
  
  ggplot(allTracts.group)+
    geom_sf(data = st_union(tracts09))+
    geom_sf(aes(fill = q5(MedHHInc))) +
    geom_sf(data = buffer, fill = "transparent", color = "red")+
    scale_fill_manual(values = palette5,
                      labels = qBr(allTracts.group, "MedHHInc"),
                      name = "Median Household Income\n(Quintile Breaks)") +
    labs(title = "Median Household Income 2009-2017", subtitle = "Real Dollars") +
    facet_wrap(~year)+
    mapTheme() + 
    theme(plot.title = element_text(size=22))
  
  
  #Median Age Maps 2009-2017
  
  ggplot(allTracts.group)+
    geom_sf(data = st_union(tracts09))+
    geom_sf(aes(fill = q5(MedAge))) +
    geom_sf(data = buffer, fill = "transparent", color = "red")+
    scale_fill_manual(values = palette5,
                      labels = qBr(allTracts.group, "MedAge"),
                      name = "Median Rent\n(Quintile Breaks)") +
    labs(title = "Median Rent 2009-2017", subtitle = "Real Dollars") +
    facet_wrap(~year)+
    mapTheme() + 
    theme(plot.title = element_text(size=22))
  


#Time space TOD and non-TOD: 2009 to 2017
ggplot(allTracts.group)+
  geom_sf(data = buffer)+
  geom_sf(aes(fill = TOD)) +
  labs(title = "Time/Space Groups") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))


#Develop tables
allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
  group_by(year, TOD) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T),
            MedAge = mean(MedAge, na.rm = T),
            MedHHInc = mean(MedHHInc, na.rm = T))

kable(allTracts.Summary) %>%
  kable_styling() 
#Add title to this later

allTracts.Summary %>%
  unite(year.TOD, year, TOD, sep = ": ", remove = T) %>%
  gather(Variable, Value, -year.TOD) %>%
  mutate(Value = round(Value, 2)) %>%
  spread(year.TOD, Value) %>%
  kable() %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.3")

#Indicator Plots
allTracts.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=5) +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
  labs(title = "Indicator differences across time and space") +
  plotTheme() + theme(legend.position="bottom")

#Graduate symbols map
ggplot() +
  geom_sf(data = tracts09, fill = "white") +
  geom_sf(data = selectCentroids09, aes(size  = TotalPop), shape = 10, 
          fill = "lightblue", alpha = .5, show.legend = "point") +
  scale_size_continuous(range = c(1, 10))
  
#MK Note:  Action item!  We need to figure out how to make this legible.


#MK NOTE:  Need to figure out the map buffer.
#Multiple ring buffers.
multipleRingBuffer <- function(inputPolygon, maxDistance, interval) 
{
  #create a list of distances that we'll iterate through to create each ring
  distances <- seq(0, maxDistance, interval)
  #we'll start with the second value in that list - the first is '0'
  distancesCounter <- 2
  #total number of rings we're going to create
  numberOfRings <- floor(maxDistance / interval)
  #a counter of number of rings
  numberOfRingsCounter <- 1
  #initialize an otuput data frame (that is not an sf)
  allRings <- data.frame()
  
  #while number of rings  counteris less than the specified nubmer of rings
  while (numberOfRingsCounter <= numberOfRings) 
  {
    #if we're interested in a negative buffer and this is the first buffer
    #(ie. not distance = '0' in the distances list)
    if(distances[distancesCounter] < 0 & distancesCounter == 2)
    {
      #buffer the input by the first distance
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #different that buffer from the input polygon to get the first ring
      buffer1_ <- st_difference(inputPolygon, buffer1)
      #cast this sf as a polygon geometry type
      thisRing <- st_cast(buffer1_, "POLYGON")
      #take the last column which is 'geometry'
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      #add a new field, 'distance' so we know how far the distance is for a give ring
      thisRing$distance <- distances[distancesCounter]
    }
    
    
    #otherwise, if this is the second or more ring (and a negative buffer)
    else if(distances[distancesCounter] < 0 & distancesCounter > 2) 
    {
      #buffer by a specific distance
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #create the next smallest buffer
      buffer2 <- st_buffer(inputPolygon, distances[distancesCounter-1])
      #This can then be used to difference out a buffer running from 660 to 1320
      #This works because differencing 1320ft by 660ft = a buffer between 660 & 1320.
      #bc the area after 660ft in buffer2 = NA.
      thisRing <- st_difference(buffer2,buffer1)
      #cast as apolygon
      thisRing <- st_cast(thisRing, "POLYGON")
      #get the last field
      thisRing <- as.data.frame(thisRing$geometry)
      #create the distance field
      thisRing$distance <- distances[distancesCounter]
    }
    
    #Otherwise, if its a positive buffer
    else 
    {
      #Create a positive buffer
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #create a positive buffer that is one distance smaller. So if its the first buffer
      #distance, buffer1_ will = 0. 
      buffer1_ <- st_buffer(inputPolygon, distances[distancesCounter-1])
      #difference the two buffers
      thisRing <- st_difference(buffer1,buffer1_)
      #cast as a polygon
      thisRing <- st_cast(thisRing, "POLYGON")
      #geometry column as a data frame
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      #add teh distance
      thisRing$distance <- distances[distancesCounter]
    }  
    
    #rbind this ring to the rest of the rings
    allRings <- rbind(allRings, thisRing)
    #iterate the distance counter
    distancesCounter <- distancesCounter + 1
    #iterate the number of rings counter
    numberOfRingsCounter <- numberOfRingsCounter + 1
  }
  
  #convert the allRings data frame to an sf data frame
  allRings <- st_as_sf(allRings)
}


allTracts.rings <-
  st_join(st_centroid(dplyr::select(allTracts.rings, GEOID, year)), 
          multipleRingBuffer(st_union(LightRailPGH_sf), 47520, 2640)) %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(allTracts, GEOID, MedRent, year), 
            by=c("GEOID"="GEOID", "year"="year")) %>%
  st_sf() %>%
  mutate(distance = distance / 5280) #convert to miles


buffers_rings <- multipleRingBuffer(buffer, 10, 1) 
ggplot() + 
  geom_sf(data = tracts09, fill = "white") +
  geom_sf(data = buffers_rings, aes(fill = distance))
