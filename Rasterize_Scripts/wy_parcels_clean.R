# wy parcels clean

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
wy.counties<-counties %>% filter(State.ANSI %in%  c("56"))
wy.counties<-st_transform(wy.counties,st_crs(r))
cty <- wy.counties %>% st_transform(., st_crs(r))

#bring in pas
wy.padus <- st_read("Data/PADUS2_1_StateWY_Shapefile/PADUS2_1Designation_StateWY.shp")
wy.pas <- st_transform(wy.padus, crs=st_crs(r)) %>% 
  st_make_valid(.)
#get the total number of protected acres for each county
s12 = lapply(1:nrow(cty), function(i){st_intersection(cty[i,],wy.pas)})
tst <- lapply(1:length(s12), function(x){
  s12[[x]] %>% 
    group_by(GEOID) %>% 
    summarise(., PAarea = sum(st_area(.))) %>% 
    st_drop_geometry(.)
})
pas.comb <- do.call(rbind, tst)
# join back to geometries from the county data
pa.cty.area <- cty %>% 
  left_join(., pas.comb, by = "GEOID") 
# fix the NAs
pa.cty.area$NAME <- toupper(pa.cty.area$NAME)
pa.cty.area$PAarea <- as.numeric(pa.cty.area$PAarea)
pa.cty.area$PAarea[is.na(pa.cty.area$PAarea)] = 0
# make a new column for the total number of non protected land area
pa.cty.area <- mutate(pa.cty.area, nonPAarea = ALAND - as.numeric(PAarea))
head(pa.cty.area) # this all looks good!

############################################
#bring in wy parcel data
wy.parcels <- st_read("Data/Wyoming_Parcels/Wyoming_Parcels.shp")
head(wy.parcels)
wy.parcels <- rename(wy.parcels,NAME = jurisdicti)
wy.parcels.drop <- st_drop_geometry(wy.parcels)
# get the total number in each jurisdiction
wy.county.parcels <- wy.parcels.drop%>%
  group_by(NAME) %>%
  summarise(parcelnb = n())    
wy.county.parcels <- rename(wy.county.parcels, TotalParcels= parcelnb)
wy.county.parcels$NAME <- gsub("BIGHORN", "BIG HORN", wy.county.parcels$NAME)
wy.county.parcels$NAME <- gsub("HOTSPRINGS", "HOT SPRINGS", wy.county.parcels$NAME)
head(wy.county.parcels)


wy.parcl.jn <- wy.county.parcels %>% 
  left_join(., pa.cty.area) %>% 
  mutate(., parceldensity = TotalParcels/nonPAarea*10000*100) # PD= patches/area then converted to 100 hectares) %>% 
head(wy.parcl.jn)

wy.parcl.jn <- st_as_sf(wy.parcl.jn)  

wy.parcl.rast<-fasterize::fasterize(wy.parcl.jn, r, field = 'parceldensity')
plot(wy.parcl.rast)
writeRaster(wy.parcl.rast, "Raster_Layers/wy.parcel.density.tif")


mt.parcl.rast <- rast("")





#######################old below
wy.parcels <-st_read("Data/Wyoming_Parcels/Wyoming_Parcels.shp")
#wy.parcels.area <- wy.parcels %>% mutate(., wy.parcels.area = st_area(wy.parcels))
head(wy.parcels)
wy.parcels <- st_drop_geometry(wy.parcels)
wy.county.parcels <- wy.parcels %>%
  group_by(CountyName) %>%
  summarise(PARCELID = n())    
wy.county.parcels <- rename(wy.county.parcels, TotalParcels= PARCELID)
wy.county.parcels <- wy.parcels %>%
  group_by(CountyName) %>%
  summarise(TotalAcres = sum(TotalAcres))      
sum(wy.parcels$TotalAcres) #check to make sure the math made sense
sum(wy.county.parcels$TotalAcres)

#we really only want the total number of parcels but i have more data just in case
wy.parcels.clean <- left_join(wy.county.parcels, wy.county.parcel.areas)
parcl <- wy.parcels.clean %>%
  mutate(avgsize = TotalAcres/TotalParcels)

parcl.jn <- parcl %>% 
  left_join(., pa.cty.area, by = c("CountyName"= "NAME")) %>% 
  mutate(., parceldensity = TotalParcels/nonPAarea*10000*100) # PD= patches/area then converted to 100 hectares) %>% 
parcl.jn <- st_as_sf(parcl.jn)  

#st_write(parcl.jn, "parcel.density.wy.shp")
parcl.rast<-fasterize::fasterize(parcl.jn, r, field = 'parceldensity')
plot(parcl.rast)

parcl.jn.stats <- wy.parcels%>% 
  left_join(., pa.cty.area, by = c("CountyName"= "NAME")) %>% 
  mutate(., parcelratio = wy.parcels.area/nonPAarea) %>% 
  group_by(GEOID) %>% 
  summarize(., medratio = median(parcelratio),
            maxratio = max(parcelratio),
            minratio = min(parcelratio),
            sdratio = sd(parcelratio))
parcl.jn.stats <- left_join(cty, parcl.jn.stats)
parcl.stats <- st_as_sf(parcl.jn.stats)

parcl.max.rast<-fasterize::fasterize(parcl.stats, r, field = 'maxratio')
plot(parcl.max.rast)
parcl.min.rast<-fasterize::fasterize(parcl.stats, r, field = 'minratio')
plot(parcl.min.rast)
parcl.sd.rast<-fasterize::fasterize(parcl.stats, r, field = 'sdratio')
plot(parcl.sd.rast)
