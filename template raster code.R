# making my template raster

r <- raster("/Users/jamiefaselt/jf_resist/Data/temp_rstr.tif")
hsi.resamp <- resample(hsi, r)
# load the nodes
# APR Shapefile???

mt_reservations <- st_read("/Users/jamiefaselt/Research/Data/MontanaReservations_shp 3/MontanaReservations.shp")
mt_fws <- st_read("/Users/jamiefaselt/Research/Data//MT Data/MT_FWS.shp")
mt_CMR <- mt_fws %>% 
  filter(., ORGNAME=="CHARLES M. RUSSELL NATIONAL WILDLIFE REFUGE",  drop=TRUE)
mt_NPS <- st_read("/Users/jamiefaselt/Research/Data/NationalParkServiceAdminBoundaries_shp 2")
yellowstone <- mt_NPS %>% 
  filter(., UNIT_NAME=="Yellowstone National Park",  drop=TRUE)
### need to go back and combine all of these polygons into one then create buffer on southern border then make this my extent/resolution raster to match my future rasters to (new on 10.13)
# combine 

#make sure all the projections are the same
reservations <- mt_reservations %>% st_transform(crs=st_crs(hsi)) %>% 
  st_make_valid()
cmr <- mt_CMR %>% st_transform(crs=st_crs(hsi)) %>% 
  st_make_valid()
yellowstone <- yellowstone %>% st_transform(crs=st_crs(hsi)) %>% 
  st_make_valid()
mt <- mt %>% st_transform(crs=st_crs(hsi))

#combine them into one shapefile
#reservations<- subset(reservations, select=c(geometry, NAME))
#cmr <- subset(cmr, select=c(geometry, ORGNAME))
#yellowstone <- subset(yellowstone, select=c(geometry, UNIT_NAME))

yellowstone <- yellowstone %>%
  rename(NAME = UNIT_NAME)
cmr <- cmr %>% 
  rename(NAME = ORGNAME)

all.nodes <- bind_rows(reservations, cmr, yellowstone)
class(all.nodes)

#create a bounding box and buffer 50km to the south
ext <- st_bbox(all.nodes)

poly <- st_as_sfc(st_bbox(c(xmin = st_bbox(mt)[[1]], xmax = st_bbox(mt)[[3]], ymax = st_bbox(mt)[[4]], ymin = st_bbox(all.nodes)[[2]]-50000), crs = st_crs(hsi)))
r <- raster(crs= proj4string(as(poly, "Spatial")), ext=raster::extent(as(poly, "Spatial")), resolution= 540)
hsi.crop <- crop(hsi, as(poly, "Spatial"))
plot(mt$geometry)
extent(r)
extent(all.nodes)
st_crs(r)==st_crs(hsi)
