# cattle sales_incl calves
# URL=https://quickstats.nass.usda.gov/results/82C79A41-78F2-3241-A349-33BFD4790E4B

library(tigris)
library(ggplot2)
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(dplyr)
library(rgdal)
library(ggmap)
library(usmap)
library(fasterize)

# bring in hsi and temp raster
r <- raster("/Users/jamiefaselt/Resistsance_Surfaces/temp_rstr.tif")
hsi <- raster("hsi_540_cropped")
#bring in counties
counties <- tigris::counties()
counties<-counties %>% filter(STATEFP %in%  c("30", "56"))
counties<-st_transform(counties,st_crs(hsi))
st_crs(counties)


# make columns match to ag.val
counties$NAME <- toupper(counties$NAME)
counties <- rename(counties, State.ANSI = STATEFP)
counties <- rename(counties, County.ANSI = COUNTYFP)
counties$State.ANSI <- as.numeric(counties$State.ANSI)
counties$County.ANSI <- as.numeric(counties$County.ANSI)


#plot(counties) #checking and this doesn't have weird gaps yet
# bring in ag val and make values a numeric variable
cattlesales <- read.csv("/Users/jamiefaselt/Resistsance_Surfaces/Data/cattle_sales_MTWY.csv")
cattlesales$Value <- gsub(",","",cattlesales$Value)
cattlesales$Value <- as.numeric(cattlesales$Value)

# join
cattlesales.spatial <- left_join(counties, cattlesales)

# double check projection
st_crs(counties) == st_crs(cattlesales.spatial) #true

#subset to relevant variables
cattle.sales.sub <- cattlesales.spatial %>% 
  dplyr::select(geometry,Value,County.ANSI,State.ANSI)

#make this a raster with temp.raster already loaded
rstr<<-fasterize::fasterize(cattle.sales.sub, r, field = 'Value')

plot(rstr) 

st_write(ag.val.sub,"ag_land_value.shp", overwrite=TRUE)
writeRaster(rstr, "ag_land_val.tif", overwrite=TRUE)

