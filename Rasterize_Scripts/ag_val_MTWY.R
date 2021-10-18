# Ag Val/Acre for MT and WY
# URL=https://quickstats.nass.usda.gov/results/DF055B44-7F89-3A0D-8FC1-BB77D37FA3C5

library(tigris)
library(ggplot2)
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(dplyr)
library(rgdal)
library(ggmap)
library(usmap)
library(fasterize)

# bring in hsi and temp raster
r <- raster("/Users/jamiefaselt/jf_resist/Data/temp_rstr.tif")
hsi <- raster("/Users/jamiefaselt/jf_resist/Data/hsi_crop_540.tif")
#bring in counties
counties <- tigris::counties()
counties<-counties %>% filter(STATEFP %in%  c("30", "56"))
counties<-st_transform(counties,st_crs(hsi))
st_crs(counties)


# make columns match to ag.val
counties$NAME <- toupper(counties$NAME)
counties <- rename(counties, State.ANSI = STATEFP)
counties <- rename(counties, County.ANSI = COUNTYFP)
counties$State.ANSI <- as.numeric(counties$State.ANSI)
counties$County.ANSI <- as.numeric(counties$County.ANSI)


#plot(counties) #checking and this doesn't have weird gaps yet
# bring in ag val and make values a numeric variable
agval <- read.csv("/Users/jamiefaselt/jf_resist/Data/ag_val_MTWY.csv")
agval$Value <- gsub(",","",agval$Value)
agval$Value <- as.numeric(agval$Value)

# join
agval.spatial <- left_join(counties, agval)

# double check projection
st_crs(counties) == st_crs(agval.spatial) #true

#subset to relevant variables
ag.val.sub <- agval.spatial %>% 
  dplyr::select(geometry,Value,County.ANSI,State.ANSI)

#make this a raster with temp.raster already loaded
rstr<<-fasterize::fasterize(ag.val.sub, r, field = 'Value')

plot(rstr) 

writeRaster(rstr, "ag_land_val.tif", overwrite=TRUE)

