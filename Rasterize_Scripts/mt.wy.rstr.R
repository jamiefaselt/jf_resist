
landval <- raster("Data/places_fmv_all.tif")
plot(landval)

tempr <- raster("temp_rstrmtwy.tif")
landvalmtwy <- projectRaster(from = landval, to= tempr)
plot(landvalmt)
st_crs(landval)

plot(landvalmt)
plot(mt.counties, add=TRUE)

writeRaster(landvalmtwy, "landvalrastmtwy.tif")


coneasements <- st_read("Data/Cadastral/Montana_Cadastral/CONSERVATIONEASEMENTS.shp")
