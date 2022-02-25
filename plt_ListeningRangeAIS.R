# plotting listening ranges on map 
rm(list=ls())
#for map
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library (geosphere)
library(ggsn)
library(rgdal)

AKprog = 3467 # c(-4596612.39 -2250856.49) , c( 2,024,122.30 4,364,571.46)
WGS84proj = 4326

#listening range
#-------------------------------------------------------------------------------
#just shape file
shpIn = st_read("E:/RESEARCH/SanctSound/analysis/Vessel_story/Shapefiles/CI05_125Hz_10dBSNR.shp")
st_crs(shpIn)
ggplot() + 
  geom_sf(data = shpIn) + 
  ggtitle("Listening Range") + 
  coord_sf()
#all files
shpIn = readOGR(dsn = "E:/RESEARCH/SanctSound/analysis/Vessel_story/Shapefiles", layer = "CI05_125Hz_10dBSNR")
st_crs(shpIn)
proj4string(shpIn) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
st_crs(shpIn)
shpIn = st_as_sf(shpIn)


#Listening station
#-------------------------------------------------------------------------------
# CI01: latitude = 34.0438, longitude = -120.0811
sites <- st_as_sf(data.frame( latitude = 34.02, longitude = -119.32),
                  coords = c("longitude", "latitude"), crs = WGS84proj, 
                  agr = "constant")
st_crs(sites)
#10 Km AIS buffer
library(tigris)
library(tidyverse)
pois <- st_transform(sites, crs = 7801)
pt_buffer = st_buffer(pois,dist = 10000,) 
buff <- st_transform(pt_buffer, crs = 4326)

#Basemap
#-----------------------------------------------------------------------------
theme_set(theme_bw())
world = ne_countries(scale = "medium", returnclass = "sf")
st_crs(world)
class(world)
ggplot(data = world) +
  geom_sf( ) +
  geom_sf(data = sites, color = "blue", size = 2)  +
  geom_sf(data = shpIn, color = 'orange', alpha = .2) +
  geom_sf(data = pt_buffer,color = 'red',alpha=.2) +
  coord_sf(xlim = c(-121.4, -118.12), ylim = c(33.65, 34.97), expand = FALSE)
          
          
          