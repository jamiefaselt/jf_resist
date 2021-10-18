# Cattle, Calves, inventory
# URL=https://quickstats.nass.usda.gov/results/BDB8D8CF-5454-3D2C-9097-EF7975BD00AB

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

#this is a projection I'm using from the wildlife pref repo
albers <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

#bring in counties
counties <- tigris::counties()
counties<-counties %>% filter(!STATEFP %in%  c("02", "60", "66", "69", "72", "78", "15"))
counties<-st_transform(counties,st_crs(albers))
st_crs(counties)


# make columns match to ag.val
counties$NAME <- toupper(counties$NAME)
counties <- rename(counties, State.ANSI = STATEFP)
counties <- rename(counties, County.ANSI = COUNTYFP)
counties$State.ANSI <- as.numeric(counties$State.ANSI)
counties$County.ANSI <- as.numeric(counties$County.ANSI)


#plot(counties) #checking and this doesn't have weird gaps yet
# bring in ag val and make values a numeric variable
cattle.inventory<- read.csv("/Users/jamiefaselt/Resistance-Surfaces/Data/cattle_inventory_2019.csv")
cattle.inventory$Value <- gsub(",","",cattle.inventory$Value)
cattle.inventory$Value <- as.numeric(cattle.inventory$Value)

# join
cattle.spatial <- left_join(counties, cattle.inventory) 
#plot(cattle.spatial)
# double check projection
st_crs(counties) == st_crs(cattle.spatial) #true

#subset to relevant variables
#cattle.inventory.sub <- cattle.spatial %>% 
  #dplyr::select(geometry,Value,County.ANSI,State.ANSI)

#create temp raster to make agval a raster
poly <- st_as_sfc(st_bbox(c(xmin = st_bbox(counties)[[1]], xmax = st_bbox(counties)[[3]], ymax = st_bbox(counties)[[4]], ymin = st_bbox(counties)[[2]]), crs = st_crs(counties)))
r <- raster(crs= proj4string(as(poly, "Spatial")), ext=raster::extent(as(poly, "Spatial")), resolution= 270)

rstr<<-fasterize::fasterize(cattle.spatial, r, field = 'Value')

plot(rstr) # counties extend into gr8 lakes dumb

st_write(cattle.spatial,"cattle_inventory.shp", overwrite=TRUE)
writeRaster(rstr, "cattle_inventory.tif", overwrite=TRUE)


#can i crop this to just Montana with a buffer???


