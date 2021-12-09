# Scaling rasters from 0-1

install.packages("climateStability")
library(climateStability)
library(raster)
library(dplyr)

# median ratio for parcels
resaclemtwy.med <- rescale0to1(mtwy.med)%>% 
  writeRaster(., "Raster_Layers/parcelratio.med.rescale.tif")
plot(log(resaclemtwy.med))

# parcel density
mtwy.pd.rescale <- rescale0to1(mtwy.pd) %>% 
  writeRaster(., "Raster_Layers/parceldensity.rescale.tif")
plot(log(mtwy.pd.rescale))

# ag land value/acre NASS
mtwy.landval <- raster("Raster_Layers/mtwy_landval.tif")
landval.rescale <- rescale0to1(mtwy.landval)
plot(landval.rescale)
writeRaster(landval.rescale, "Raster_Layers/landval.rescale.tif")

# cattle sales
mtwy.cattlesales.rescale <- raster("Raster_Layers/mtwy_cattle_sales.tif") %>% 
  rescale0to1(.) %>% 
  writeRaster(., "Raster_Layers/cattlesales.rescale.tif")
plot(mtwy.cattlesales.rescale)

# unemployment
unempl.rescale <- raster("Raster_Layers/mtwy.unempl.tif") %>% 
  rescale0to1(.) %>% 
  writeRaster(., "Raster_Layers/unemployment.rescale.tif")
plot(unempl.rescale)

# poverty
poverty.rescale <- raster("Raster_Layers/mtwy.pov.tif") %>% 
  rescale0to1(.) %>% 
  writeRaster(., "Raster_Layers/poverty.rescale.tif")
plot(poverty.rescale)

# percent republican vote
vote.rescale <- raster("Raster_Layers/mtwy_avgrepub_00-20.tif") %>% 
  rescale0to1(.) %>% 
  writeRaster(., "Raster_Layers/avgrepubvote.rescale.tif")
plot(vote.rescale)

# land value zillow
pnas.rescale <- raster("Raster_Layers/mtwy_pnas_landval.tif") %>% 
  rescale0to1(.) %>% 
  writeRaster(., "Raster_Layers/zillowlandval.rescale.tif")
plot(pnas.rescale)

# habitat suitabliity model
hsi.crop <- raster("hsi_540.tif")
# rescaled this after taking the inverse wlesehwere



