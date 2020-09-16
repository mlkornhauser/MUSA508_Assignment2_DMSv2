# MUSA 508 Assignment 1
# TOD in Pittsburgh, PA
# Divya, Maddy & Sophia

###########
# Set Up 
###########

# Load Libraries

library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)

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

##################
# Year 2009 tracts 
##################

tracts09 <-  
  get_acs(geography = "tract", variables = c("B25026_001E", "B02001_002E", "B15001_050E", "B15001_009E",
                                             "B19013_001E", "B25058_001E", "B06012_002E"), 
          year=2009, state=42, county=003, geometry=T) %>% 
  st_transform('ESRI:102728')  

#MK Note:  Need to select just the tracts within Pittsburgh City limits

head(tracts09)

##################
# Year 2017 tracts 
##################

tracts17 <-  
  get_acs(geography = "tract", variables = c("B25026_001E", "B02001_002E", "B15001_050E", "B15001_009E",
                                             "B19013_001E", "B25058_001E", "B06012_002E"), 
          year=2017, state=42, county=003, geometry=T) %>% 
  st_transform('ESRI:102728')  

head(tracts17)
