library(raster)
library(rgdal)
library(gdalUtils)
library(ncdf4)
library(signal)
library(RColorBrewer)
library(doMC)


###############################
#
sites <- c('morganmonroe2','NEON.D01.HARV.DP1.0003','harvardhemlock2','uiefmaize','portal','sevilletagrass')
lat   <- as.numeric(rep(NA,length(sites)))
lon   <- as.numeric(rep(NA,length(sites)))
vgt   <- as.numeric(rep(NA,length(sites)))
utm   <- c(16,18,18,16,12,13)

datCoor <- data.frame(sites,lon,lat,utm)

# Extract site coordinate
for(ss in 1:length(sites)){
  path <- '/projectnb/modislc/users/mkmoon/Phenocam/PhenoCam_V2_1674/data'
  sstr <- paste0(sites[ss],'*meta.txt')
  filename <- list.files(path=path,pattern=glob2rx(sstr),full.names=T)
  
  temp <- readChar(filename,file.info(filename)$size)
    
    lon <- sub('.*lon: ','',temp)
    lon <- sub('\r.*','',lon)  
  
    lat <- sub('.*lat: ','',temp)
    lat <- sub('\r.*','',lat)
    
    vgt <- sub('.*primary_veg_type: ','',temp)
    vgt <- sub('\r.*','',vgt)
    
    datCoor$lon[ss] <- as.numeric(lon)
    datCoor$lat[ss] <- as.numeric(lat)
    datCoor$vgt[ss] <- vgt
}


# Make shape-polygon
setwd('/projectnb/modislc/users/mkmoon/Planet/shp/')

geog_crs = CRS("+proj=longlat +datum=WGS84")
for(ss in 1:length(sites)){
  utm_crs = CRS(paste0("+proj=utm +zone=",datCoor$utm[ss]," +datum=WGS84 +units=m"))
  
  site <- data.frame(datCoor[ss,1:3])
  colnames(site) <- c('id','lon','lat')
  xy   <- site[,c(2,3)]
  bb   <- SpatialPointsDataFrame(coords=xy,data=site,proj4string=geog_crs)
  bb   <- spTransform(bb,utm_crs)
  
  x1 <- bb@coords[1] - 5000
  x2 <- bb@coords[1] + 5000
  y1 <- bb@coords[2] - 5000
  y2 <- bb@coords[2] + 5000
  xCoor <- c(x1,x2,x2,x1)
  yCoor <- c(y1,y1,y2,y2)
  xym <- cbind(xCoor,yCoor)
  p   <- Polygon(xym)
  ps  <- Polygons(list(p),1)
  sps <- SpatialPolygons(list(ps))
  proj4string(sps) <- utm_crs
  data <- data.frame(f=99.9)
  spdf <- SpatialPolygonsDataFrame(sps,data)
  spdf <- spTransform(spdf,geog_crs)
  
  writeOGR(spdf,".",paste0(datCoor$sites[ss]),driver="ESRI Shapefile",overwrite=T)
}


