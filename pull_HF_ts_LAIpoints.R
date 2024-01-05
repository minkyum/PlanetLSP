library(raster)
library(rgdal)
library(gdalUtils)
library(ncdf4)
library(signal)
library(RColorBrewer)
library(doMC)

###############################
##
source('/usr3/graduate/mkmoon/GitHub/PlanetLSP/PLSP_Functions.R')

load('/projectnb/modislc/users/mkmoon/Planet/data/planet_eviStack_hf.rda')


# Time series
shpPoints <- readOGR('/projectnb/modislc/users/mkmoon/Planet/shp/18TYN_pts_1.shp')
shpPoints <- readOGR('/projectnb/modislc/projects/landsat_sentinel/shapefiles/HARV_pts_proj.shp')


datPTS1 <- extractTS(shpPoints,eviStack,imgBase,id="A1",rad=2)

plot(dates,datPTS1[1,])
