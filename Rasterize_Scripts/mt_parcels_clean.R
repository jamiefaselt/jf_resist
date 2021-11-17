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
cty <- mt.counties %>% st_transform(., st_crs(r))

#bring in pas
mt.padus <- st_read("Data/PADUS2_1_StateMT_Shapefile/PADUS2_1Designation_StateMT.shp")
mt.pas <- st_transform(mt.padus, crs=st_crs(r)) %>% 
  st_make_valid(.)
#get the total number of protected acres for each county
s12 = lapply(1:nrow(cty), function(i){st_intersection(cty[i,],mt.pas)})
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
pa.cty.area$PAarea[is.na(pa.cty.area$PAarea)] = 0
# make a new column for the total number of non protected land area
pa.cty.area <- mutate(pa.cty.area, nonPAarea = ALAND - as.numeric(PAarea))
head(pa.cty.area)

############################################
mt.parcels <-st_read("Data/Cadastral/Montana_Cadastral/OWNERPARCEL.shp")
mt.parcels.area <- mt.parcels %>% mutate(., mt.parcels.area = st_area(mt.parcels))
head(mt.parcels.area)
mt.parcels.area <- st_drop_geometry(mt.parcels.area)
mt.county.parcels <- mt.parcels.area %>%
  group_by(CountyName) %>%
  summarise(PARCELID = n())    
mt.county.parcels <- rename(mt.county.parcels, TotalParcels= PARCELID)
mt.county.parcel.areas <- mt.parcels.area %>%
  group_by(CountyName) %>%
  summarise(TotalAcres = sum(TotalAcres))      
sum(mt.parcels$TotalAcres) #check to make sure the math made sense
sum(mt.county.parcel.areas$TotalAcres)

#we really only want the total number of parcels but i have more data just in case
mt.parcels.clean <- left_join(mt.county.parcels, mt.county.parcel.areas)
parcl <- mt.parcels.clean %>%
  mutate(avgsize = TotalAcres/TotalParcels)

parcl.jn <- parcl %>% 
  left_join(., pa.cty.area, by = c("CountyName"= "NAME")) %>% 
  mutate(., parceldensity = TotalParcels/nonPAarea*10000*100) # PD= patches/area then converted to 100 hectares) %>% 
parcl.jn <- st_as_sf(parcl.jn)  

#st_write(parcl.jn, "parcel.density.mt.shp")
parcl.rast<-fasterize::fasterize(parcl.jn, r, field = 'parceldensity')
plot(parcl.rast)
writeRaster(parcl.rast, "Raster_Layers/mt.parcel.density.tif")
wy.parcl.rast <- raster("Raster_Layers/wy.parcel.density.tif")
mtwy.pd <- merge(parcl.rast, wy.parcl.rast)
plot(mtwy.pd)

writeRaster(mtwy.pd, "Raster_Layers/mtwy.parcel.density.tif")


##############3

parcl.jn <- st_drop_geometry(mt.parcels) %>% 
  left_join(., pa.cty.area, by = c("CountyName"= "NAME")) %>% 
  mutate(., parcelratio = TotalAcres/nonPAarea) %>% 
  group_by(GEOID) %>% 
  summarize(., medratio = median(parcelratio),
            maxratio = max(parcelratio),
            minratio = min(parcelratio),
            sdratio = sd(parcelratio))


parcl.jn <- st_drop_geometry(mt.parcels)%>% 
  left_join(., pa.cty.area, by = c("CountyName"= "NAME")) %>% 
  mutate(., parcelratio = TotalAcres/nonPAarea) %>% 
  group_by(GEOID) %>% 
  summarize(., medratio = median(parcelratio),
            maxratio = max(parcelratio),
            minratio = min(parcelratio),
            sdratio = sd(parcelratio))
parcl.jn.stats <- left_join(cty, parcl.jn)
parcl.stats <- st_as_sf(parcl.jn.stats)

parcl.max.rast<-fasterize::fasterize(parcl.stats, r, field = 'maxratio')
plot(log(parcl.max.rast))
parcl.med.rast<-fasterize::fasterize(parcl.stats, r, field = 'medratio')
plot(log(parcl.med.rast))
parcl.min.rast<-fasterize::fasterize(parcl.stats, r, field = 'minratio')
plot(log(parcl.min.rast))
parcl.sd.rast<-fasterize::fasterize(parcl.stats, r, field = 'sdratio')
plot(log(parcl.sd.rast))


