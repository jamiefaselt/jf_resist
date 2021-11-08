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
# make columns match to caddat
#counties$NAME <- toupper(counties$NAME)
counties <- rename(counties, State.ANSI = STATEFP)
counties <- rename(counties, County.ANSI = COUNTYFP)
counties$State.ANSI <- as.numeric(counties$State.ANSI)
counties$County.ANSI <- as.numeric(counties$County.ANSI)

mt.counties<-counties %>% filter(State.ANSI %in%  c("30"))
mt.counties<-st_transform(mt.counties,st_crs(r))
wy.counties<-counties %>% filter(State.ANSI %in%  c("56"))
wy.counties<-st_transform(wy.counties,st_crs(r))

#bring in cadastral data
mt.parcels <-st_read("Data/Cadastral/Montana_Cadastral/OWNERPARCEL.shp")
head(mt.parcels)
mt.parcels <- st_drop_geometry(mt.parcels)
# want to get total number of acres for each county-- subtract public and water land
# get number of unique parcel ids for each county-- total that-- then divide by the acreage to get average parcel size

#subset to make it easier to use
mt.parcels.sub <- mt.parcels %>% 
  dplyr::select(PARCELID,TotalAcres,TotalValue,CountyName)
mt.parcels.drop <- st_drop_geometry(mt.parcels.sub)

#for the project purposes I am just going to look at number of parcels for each county I think
mt.county.parcels <- mt.parcels %>%
  group_by(CountyName) %>%
  summarise(PARCELID = n())    
mt.county.parcels <- rename(mt.county.parcels, TotalParcels= PARCELID)


mt.county.parcel.areas <- mt.parcels %>%
  group_by(CountyName) %>%
  summarise(TotalAcres = sum(TotalAcres))      

sum(mt.parcels$TotalAcres) #check to make sure the math made sense
sum(mt.county.parcel.areas$TotalAcres)

mt.parcels.clean <- left_join(mt.county.parcels, mt.county.parcel.areas)


mt.parcels.clean <- mt.parcels.clean %>%
  mutate(avgsize = TotalAcres/TotalParcels)

mt.counties <- rename(mt.counties, CountyName = NAME)
mt.parcels.spat <- left_join(mt.parcels.clean, mt.counties)
head(mt.parcels.spat)
mt.parcels.spat <- st_as_sf(mt.parcels.spat)
ext(mt.parcels.spat)

mt.parcel.rstr<-fasterize::fasterize(mt.parcels.spat, r, field = 'TotalParcels')
mt.parcel.size.rstr<-fasterize::fasterize(mt.parcels.spat, r, field = 'avgsize')

plot(mt.parcel.rstr) 
writeRaster(mt.parcel.rstr, "mt.total.parcels.tif", overwrite=TRUE)
writeRaster(mt.parcel.size.rstr, "mt.avgsize.parcels.tif", overwrite=TRUE)

############################################3
#WY data
counties <- tigris::counties()
# make columns match to caddat
counties$NAME <- toupper(counties$NAME)
counties <- rename(counties, CountyName = NAME)
counties <- rename(counties, County.ANSI = COUNTYFP)
counties$State.ANSI <- as.numeric(counties$State.ANSI)
counties$County.ANSI <- as.numeric(counties$County.ANSI)
#filter to wy.counties
wy.counties<-counties %>% filter(STATEFP %in%  c("56"))
wy.counties<-st_transform(wy.counties,st_crs(r))

#bring in wy parcel data
wy.parcels <- st_read("Data/Wyoming_Parcels/Wyoming_Parcels.shp")
head(wy.parcels)
wy.parcels.drop <- st_drop_geometry(wy.parcels)

wy.county.parcels <- wy.parcels.drop%>%
  group_by(jurisdicti) %>%
  summarise(parcelnb = n())    
wy.county.parcels <- rename(wy.county.parcels, TotalParcels= parcelnb)

wy.county.parcel.areas <- wy.parcels.drop%>%
  group_by(jurisdicti) %>%
  summarise(landgrossa = sum(landgrossa))      
wy.county.parcel.areas <- rename(wy.county.parcel.areas, TotalAcres= landgrossa)

wy.parcels.clean <- left_join(wy.county.parcels, wy.county.parcel.areas)
wy.parcels.clean <- rename(wy.parcels.clean, CountyName=jurisdicti)
head(wy.parcels.clean)
wy.parcels.clean <- wy.parcels.clean %>%
  mutate(avgsize = TotalAcres/TotalParcels) 


wy.parcels.spat <- left_join(wy.parcels.clean, wy.counties)
head(wy.parcels.spat)
wy.parcels.spat <- st_as_sf(wy.parcels.spat)
ext(wy.parcels.spat)

wy.parcel.rstr<-fasterize::fasterize(wy.parcels.spat, r, field = 'TotalParcels')
wy.parcel.size.rstr<-fasterize::fasterize(wy.parcels.spat, r, field = 'avgsize')

writeRaster(wy.parcel.size.rstr, "wy.avgsize.parcels.tif", overwrite=TRUE)
writeRaster(wy.parcel.rstr, "wy.total.parcels.tif", overwrite=TRUE)



# Combine MT and WY 
mt.total <- raster("mt.total.parcels.tif")
mt.avg <- raster("mt.avgsize.parcels.tif")

mtwy.total.parcels <- merge(mt.total, wy.parcel.rstr)
plot(mtwy.total.parcels)
writeRaster(mtwy.total.parcels, "mtwy.total.parcels.tif")

mtwy.avg.parcels <- merge(mt.avg, wy.parcel.size.rstr)
plot(mtwy.avg.parcels)
writeRaster(mtwy.avg.parcels, "mtwy.avg.parcelsize.tif")

