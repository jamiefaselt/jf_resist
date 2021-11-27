# mt parcel data
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

#bring in cadastral data
mt.parcels <-st_read("Data/Cadastral/Montana_Cadastral/OWNERPARCEL.shp")
#mt.parcels.area <- mt.parcels %>% mutate(., mt.parcels.area = st_area(mt.parcels))
mt.parcels <- st_drop_geometry(mt.parcels)
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


# bring in PADUS shapefile
mt.padus <- st_read("Data/PADUS2_1_StateMT_Shapefile/PADUS2_1Designation_StateMT.shp")
head(mt.padus)
mt.padus.area <- mt.padus %>% mutate(., mt.padus.area = st_area(mt.padus))
mt.padus.area <- st_make_valid(mt.padus.area) 
mt.padus.area <- st_transform(mt.padus.area, crs=st_crs(r))

#pas.in.counties <- st_join(mt.padus.area, mt.counties, join=st_is_within_distance, dist=0)
mt.pas.counties.intersect <- st_intersection(mt.padus.area, mt.counties)
colnames(mt.pas.counties.intersect)
# Calculate the area of each intersection:
mt.pas.counties.intersect$intersection_area <-st_area(mt.pas.counties.intersect)
mt.county.ps.areas <- mt.pas.counties.intersect %>%
  group_by(CountyName) %>%
  summarise(mt.padus.area = sum(mt.padus.area))      
mt.county.pas.area <- st_drop_geometry(mt.county.ps.areas)
head(mt.county.pas.area)
head(mt.parcels.spat)
# join the pa and parcel dataframes and create column for total land- protected
mt.parcels.pas.area<- rename(mt.county.pas.area, NAME = CountyName)
mt.pas.parcels <- left_join(mt.parcels.spat, mt.county.pas.area)

head(mt.pas.parcels)
mt.pas.parcels$ALAND <- as.numeric(mt.pas.parcels$ALAND)
mt.pas.parcels$mt.padus.area <- as.numeric(mt.pas.parcels$mt.padus.area)
mt.pas.parcels <- mt.pas.parcels %>% 
  mutate(availableland= ALAND-mt.padus.area)
colnames(mt.pas.parcels)

# calculate parcel density and rasterize
mt.pd <- mt.pas.parcels %>% 
  mutate(pd = (TotalParcels/availableland)*10000*100) # PD= patches/area then converted to 100 hectares
mt.pd.rstr<-fasterize::fasterize(mt.pd, r, field = 'pd')
plot(mt.pd.rstr)


mt.pd.nopas <- mt.pas.parcels %>% 
  mutate(pd = (TotalParcels/ALAND)*10000*100) # PD= patches/area then converted to 100 hectares
mt.pd.nopas.rstr<-fasterize::fasterize(mt.pd.nopas, r, field = 'pd')
plot(mt.pd.nopas.rstr)
