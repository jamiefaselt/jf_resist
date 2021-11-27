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

mt.max<- raster("Raster_Layers/singlestate/mt.parcel.maxratio.tif")
mt.med <- raster("Raster_Layers/singlestate/mt.parcel.medratio.tif")
mt.sd <- raster("Raster_Layers/singlestate/mt.parcel.sdratio.tif")

wy.max <- raster("Raster_Layers/singlestate/wy.parcel.maxratio.tif")
wy.med <- raster("Raster_Layers/singlestate/wy.parcel.medratio.tif")
wy.sd <- raster("Raster_Layers/singlestate/wy.parcel.sdratio.tif")

mtwy.max <- merge(mt.max, wy.max)
plot(log(mtwy.max))

mtwy.med <- merge(mt.med, wy.med)
plot(log(mtwy.med))
writeRaster(mtwy.med, "Raster_Layers/mtwy.medratio.tif")

wy.pd <- raster("Raster_Layers/singlestate/wy.parcel.density.tif")
mt.pd <- raster("Raster_Layers/mt.parcel.density.tif")
mtwy.pd <- merge(mt.pd, wy.pd)
plot(mtwy.pd)
# writeRaster(mtwy.pd, "Raster_Layers/mtwy.parcel.density.tif")
