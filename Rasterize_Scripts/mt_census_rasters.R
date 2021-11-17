# census data
# poverty
library(sf)
library(tidyverse)
library(tidycensus)
library(tmap)
library(tigris)
library(car)
library(rgdal)
library(raster)

states <- tigris::states()
mt <- states %>% filter(., NAME=="Montana", drop=TRUE)
counties <- tigris::counties()
mt.counties<-counties %>% filter(STATEFP %in%  c("30"))
r <- raster("Data/temp_rstr.tif")
mt.counties<-st_transform(mt.counties,st_crs(r))

mt.tracts <- get_acs(geography = "county", 
                     year = 2019,
                     variables = c(tpop = "B01003_001", tpopr = "B03002_001", 
                                   nhwhite = "B03002_003", nhblk = "B03002_004",
                                   nhasn = "B03002_006", hisp = "B03002_012",
                                   unemptt = "B23025_003", unemp = "B23025_005",
                                   povt = "B17001_001", pov = "B17001_002", 
                                   colt = "B15003_001", col1 = "B15003_022", 
                                   col2 = "B15003_023", col3 = "B15003_024", 
                                   col4 = "B15003_025", mobt = "B07003_001", 
                                   mob1 = "B07003_004"),
                     state = "MT",
                     survey = "acs5",
                     geometry = TRUE)

mt.tracts <- mt.tracts %>% 
  dplyr::select(-(moe)) %>%
  spread(key = variable, value = estimate) %>%
  mutate(pnhwhite = 100*(nhwhite/tpopr), pnhasn = 100*(nhasn/tpopr), 
         pnhblk = 100*(nhblk/tpopr), phisp = 100*(hisp/tpopr),
         unempr = 100*(unemp/unemptt),
         ppov = 100*(pov/povt), 
         pcol = 100*((col1+col2+col3+col4)/colt), 
         pmob = 100-100*(mob1/mobt)) %>%
  dplyr::select(c(GEOID,tpop, pnhwhite, pnhasn, pnhblk, phisp, ppov,
                  unempr, pcol, pmob))  


mt.tracts <- st_drop_geometry(mt.tracts)

mt.tracts$GEOID <- as.numeric(mt.tracts$GEOID)
mt.counties$GEOID <- as.numeric(mt.counties$GEOID)
mt.census.data <- left_join(mt.tracts, mt.counties)
mt.census.data <- st_as_sf(mt.census.data)
head(mt.census.data)

mt.pov.rstr<-fasterize::fasterize(mt.census.data, r, field = 'ppov')
plot(mt.pov.rstr)


#############################################################################
# unemployment
