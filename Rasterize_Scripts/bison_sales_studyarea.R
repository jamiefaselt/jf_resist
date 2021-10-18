# Bison sales in $ study area
# URL=https://quickstats.nass.usda.gov/results/0F04B3D3-BDFF-3176-B261-1B4D99887288


library(tigris)
library(ggplot2)
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(dplyr)
library(rgdal)
library(ggmap)
library(stars)
library(fasterize)


counties <- tigris::counties()
counties<-counties %>% filter(STATEFP %in%  c("16", "30", "38", "46", "56"))
counties<-st_transform(counties,st_crs(albers))
st_crs(counties)

counties <- rename(counties, County = NAME)
counties$County <- toupper(counties$County)

bisonsales <- read.csv("Data/bison_sales_dollar_studarea.csv")
str(bisonsales)
bisonsales$Value <- gsub(",","",bisonsales$Value) %>% 
  as.numeric(bisonsales$Value)

bison.sales.sp <- left_join(counties, bisonsales)

plot(bison.sales.sp)
st_crs(counties) == st_crs(bison.sales.sp)


rtemp <- raster(crs=counties, extent(counties), resolution=30)


bison.sales <- fasterize(bison.sales.sp, rtemp, field = "Value")
plot(bison.sales)




rstr<-fasterize::fasterize(ag.val.sub, temp.rast, field = "Value")
plot(rstr)
write_csv(ag.val.sub, "2017_ag_land_value.csv")
st_write(ag.val.sub,"2017_ag_land_value.shp")
writeRaster(ag.val, "2017_ag_land_val.tif", overwrite=TRUE)

ag.val <- raster("2017_ag_land_val.tif")
ag.val.sub <- st_read("2017_ag_land_value.shp")
