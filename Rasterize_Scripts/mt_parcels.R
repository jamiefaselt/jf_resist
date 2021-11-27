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
mt.counties.area <- st_area(mt.counties)
head(mt.counties.area)

#bring in cadastral data
mt.parcels <-st_read("Data/Cadastral/Montana_Cadastral/OWNERPARCEL.shp")
mt.parcels.area <- mt.parcels %>% mutate(., mt.parcels.area = st_area(mt.parcels))
head(mt.parcels.area)
mt.parcels.area <- st_make_valid(mt.parcels.area, reason=TRUE) #this does not work
st_make_valid(mt.parcels.area) %>% st_cast("MULTIPOLYGON") #fml
st_is_valid(mt.parcels.area, reason = TRUE)
# function from stack overflow
library(gdalUtilities)
ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}
mt.parcels.corrected <-  ensure_multipolygons(mt.parcels.area)
mt.parcels.valid <- st_make_valid(mt.parcels.corrected) %>% #this did not work
  aggregate(., by = list(.$PARCELID), FUN = dplyr::first)

# bring in PADUS shapefile
mt.padus <- st_read("Data/PADUS2_1_StateMT_Shapefile/PADUS2_1Designation_StateMT.shp")
head(mt.padus)
mt.padus.area <- mt.padus %>% mutate(., mt.padus.area = st_area(mt.padus))
mt.padus.area <- st_make_valid(mt.padus.area) 
mt.padus.area <- st_transform(mt.padus.area, crs=st_crs(r))

#pas.in.counties <- st_join(mt.padus.area, mt.counties, join=st_is_within_distance, dist=0)
mt.pas.counties.intersect <- st_intersection(mt.padus.area, mt.counties)
plot(st_geometry(mt.pas.counties.intersect))
# Calculate the area of each intersection:
mt.pas.counties.intersect$intersection_area <-st_area(mt.pas.counties.intersect)
mt.county.ps.areas <- mt.pas.counties.intersect %>%
  group_by(NAME) %>%
  summarise(mt.padus.area = sum(mt.padus.area))      
mt.county.pas.area <- st_drop_geometry(mt.county.ps.areas)

head(mt.county.ps.areas)


#mt.padus.corrected <-  ensure_multipolygons(mt.padus)
#mt.padus.area <- mt.padus.corrected %>% 
  #group_by(Unit_Nm) %>% 
 # summarize(., mt.padus.area = mean(estimate))
#mt.padus.valid <- st_make_valid(mt.padus.corrected) %>% 
  #aggregate(., by = list(.$Unit_Nm), FUN = dplyr::first)


# want to get total number of acres for each county-- subtract public and water land
# get number of unique parcel ids for each county-- total that-- then divide by the acreage to get average parcel size


#for the project purposes I am just going to look at number of parcels
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
ext(mt.parcels.spat)
st_write(mt.parcels.spat, "mt.parcels.spatial.df.shp")

mt.parcels.spat <- st_read("mt.parcels.spatial.df.shp")
head(mt.parcels.spat)
mt.parcelsable <- left_join(mt.parcels.spat, mt.county.pas.area)
mt.parcels.spat<- rename(mt.county.pas.area, NAME = CountyNm)


head(mt.parcels.spat)
mt.pd <- mt.parcels.spat %>% 
  mutate(pd = (TtlPrcl/(ALAND-(mt.county.ps.areas$mt.padus.area))*10000*100) # PD= patches/area then converted to 100 hectares
mt.pd.rstr<-fasterize::fasterize(mt.pd, r, field = 'pd')
plot(mt.pd.rstr)

mt.parcel.rstr<-fasterize::fasterize(mt.parcels.spat, r, field = 'TotalParcels')
mt.parcel.size.rstr<-fasterize::fasterize(mt.parcels.spat, r, field = 'avgsize')

plot(mt.parcel.rstr) 
writeRaster(mt.parcel.rstr, "mt.total.parcels.tif", overwrite=TRUE)
writeRaster(mt.parcel.size.rstr, "mt.avgsize.parcels.tif", overwrite=TRUE)

#####################