# Wyoming Parcel Data
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
# bring in counties
counties <- tigris::counties()
counties$NAME <- toupper(counties$NAME)
counties <- rename(counties, CountyName = NAME)
counties <- rename(counties, County.ANSI = COUNTYFP)
counties$State.ANSI <- as.numeric(counties$State.ANSI)
counties$County.ANSI <- as.numeric(counties$County.ANSI)
#filter to wy.counties
wy.counties<-counties %>% filter(STATEFP %in%  c("56"))
wy.counties<-st_transform(wy.counties,st_crs(r))

#bring in pas
wy.padus <- st_read("Data/PADUS2_1_StateWY_Shapefile/PADUS2_1Designation_StateWY.shp")
wy.pas <- st_transform(wy.padus, crs=st_crs(r)) %>% 
  st_make_valid(.)
#get the total number of protected acres for each county
s12 = lapply(1:nrow(wy.counties), function(i){st_intersection(wy.counties[i,],wy.pas)})
tst <- lapply(1:length(s12), function(x){
  s12[[x]] %>% 
    group_by(GEOID) %>% 
    summarise(., PAarea = sum(st_area(.))) %>% 
    st_drop_geometry(.)
})
pas.comb <- do.call(rbind, tst)
# join back to geometries from the county data
pa.cty.area <- wy.counties %>% 
  left_join(., pas.comb, by = "GEOID") 
# fix the NAs
pa.cty.area$PAarea[is.na(pa.cty.area$PAarea)] = 0
# make a new column for the total number of non protected land area
pa.cty.area <- mutate(pa.cty.area, nonPAarea = ALAND - as.numeric(PAarea))
head(pa.cty.area)

#bring in wy parcel data and calculate total area of the parcels
wy.parcels <- st_read("Data/Wyoming_Parcels/Wyoming_Parcels.shp") %>% 
  st_make_valid()
st_is_valid(wy.parcels)
wy.parcels <- st_transform(wy.parcels,st_crs(r))
wy.parcels.area <- wy.parcels %>% mutate(., parcel.area = st_area(wy.parcels))
wy.parcels.area <- rename(wy.parcels.area, CountyName = jurisdicti)

# calculate parcel density
wy.parcels.drop <- st_drop_geometry(wy.parcels.area)
# get the total number in each jurisdiction
wy.county.parcels <- wy.parcels.drop%>%
  group_by(CountyName) %>%
  summarise(parcelnb = n())    
wy.county.parcels <- rename(wy.county.parcels, TotalParcels= parcelnb)
head(wy.county.parcels)
parcl.dens <- wy.county.parcels %>% 
  left_join(., pa.cty.area) %>% 
  mutate(., parceldensity = TotalParcels/nonPAarea*10000*100) # PD= patches/area then converted to 100 hectares) %>% 
wy.parcl.dens <- st_as_sf(parcl.dens)  
#st_write(parcl.jn, "parcel.density.wy.shp")
parcl.dens.rast<-fasterize::fasterize(wy.parcl.dens, r, field = 'parceldensity')
plot(parcl.dens.rast)

pd <- raster("Raster_Layers/singlestate/wy.parcel.density.tif")
plot(pd)

# get statistics on parcel sizes
colnames(wy.parcels.area)
parcl.jn <- st_drop_geometry(wy.parcels.area)%>% 
  left_join(., pa.cty.area)%>% 
  mutate(., parcelratio = wy.parcels.area/nonPAarea) %>% 
  group_by(GEOID) %>% 
  summarize(., medratio = median(parcelratio),
            maxratio = max(parcelratio),
            minratio = min(parcelratio),
            sdratio = sd(parcelratio))
parcl.jn.stats <- left_join(wy.counties, parcl.jn)
parcl.stats <- st_as_sf(parcl.jn.stats)
parcl.max.rast<-fasterize::fasterize(parcl.stats, r, field = 'maxratio')
plot(parcl.max.rast)
parcl.min.rast<-fasterize::fasterize(parcl.stats, r, field = 'minratio')
plot(parcl.min.rast)
parcl.sd.rast<-fasterize::fasterize(parcl.stats, r, field = 'sdratio')
plot(parcl.sd.rast)

unique(wy.parcels.area$CountyName)





head(wy.parcels)
wy.parcels.drop <- st_drop_geometry(wy.parcels)
# get the total number in each jurisdiction
wy.county.parcels <- wy.parcels.drop%>%
  group_by(CountyName) %>%
  summarise(parcelnb = n())    
wy.county.parcels <- rename(wy.county.parcels, TotalParcels= parcelnb)
############################



head(wy.county.parcels)

parcl.jn <- wy.county.parcels %>% 
  left_join(., pa.cty.area, by = c("jurisdicti"= "NAME")) %>% 
  mutate(., parceldensity = TotalParcels/nonPAarea*10000*100) # PD= patches/area then converted to 100 hectares) %>% 
wy.parcl <- st_as_sf(parcl.jn)  

#st_write(parcl.jn, "parcel.density.wy.shp")
parcl.rast<-fasterize::fasterize(parcl.jn, r, field = 'parceldensity')
plot(parcl.rast)

parcl.jn.stats <- wy.parcels.area%>% 
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



###############################3old below
wy.county.parcel.areas <- wy.county.parcels%>%
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

mt.total <- raster("mt.total.parcels.tif")
mt.avg <- raster("mt.avgsize.parcels.tif")

mtwy.total.parcels <- merge(mt.total, wy.parcel.rstr)
plot(mtwy.total.parcels)
writeRaster(mtwy.total.parcels, "mtwy.total.parcels.tif")


mtwy.avg.parcels <- merge(mt.avg, wy.parcel.size.rstr)
plot(mtwy.avg.parcels)
writeRaster(mtwy.avg.parcels, "mtwy.avg.parcelsize.tif")

###################################################################

