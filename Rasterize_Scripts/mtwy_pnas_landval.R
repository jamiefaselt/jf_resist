# private land value
# https://www.pnas.org/content/117/47/29577

library(sf)
library(tidyverse)
library(tidycensus)
library(tmap)
library(tigris)
library(car)
library(rgdal)
library(raster)
library(dplyr)

r <- raster("Data/temp_rstr.tif")
landval <- raster("Data/places_fmv_all.tif")
landval <- projectRaster(from = landval, to= r)
plot(landval)
writeRaster(landval, "Raster_Layers/mtwy_pnas_landval.tif")
