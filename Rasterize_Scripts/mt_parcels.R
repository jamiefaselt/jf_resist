# Cadastral Parcel Data

library(tigris)
library(ggplot2)
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(terra)
library(dplyr)
library(rgdal)

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

#bring in cadastral data
parcels <- st_read("Data/Cadastral/Montana_Cadastral/OWNERPARCEL.shp")
plot(parcels)
