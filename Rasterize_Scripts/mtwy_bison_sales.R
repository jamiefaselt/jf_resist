# bison sales_incl calves
# URL=https://quickstats.nass.usda.gov/results/D0301CC4-05F4-334A-A3FB-EE69CFC4B15E

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
bisonsales <- read.csv("Data/mt_wy_bisonsales.csv")
bisonsales$Value <- gsub(",","",bisonsales$Value)
bisonsales$Value <- as.numeric(bisonsales$Value)

# join
bisonsales.spatial <- left_join(counties, bisonsales)
head(bisonsales.spatial)
#get rid of nas
bisonsales.spatial[is.na(bisonsales.spatial)] = 0
# double check projection
st_crs(counties) == st_crs(bisonsales.spatial) #true

#make this a raster with temp.raster already loaded
rstr<<-fasterize::fasterize(bisonsales.spatial, r, field = 'TOTAL')

plot(rstr) 

writeRaster(rstr, "bison_sales_mtwy.tif", overwrite=TRUE)

