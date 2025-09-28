#Load required libraries
library(raster)
library(rgdal)
library(gdalUtils)
library(rgeos)

library(foreach)
library(doMC)

args <- commandArgs()
print(args)

tt <- as.numeric(args[3]) 

###########################################
tile <- '16SEJ'
year <- 2018

# HLS images
imgDir <- paste0('/projectnb/modislc/projects/landsat_sentinel/v1_4/HLS30/',tile,'/images')
filesHLS <- list.files(path=imgDir,pattern=glob2rx(paste0('HLS*.',year,'*.hdf')),full.names=T)

shpPoints <- readOGR(paste0('/projectnb/modislc/projects/landsat_sentinel/MSLSP_assessment/shps/',tile,'_pts.shp'))

datesHLS <- as.Date(as.numeric(substr(filesHLS,89,91)),origin=paste0((year-1),'-12-31'))
datesHLS <- unique(datesHLS)
datesHLS <- datesHLS[order(datesHLS)]

# Fmask
imgDir <- paste0('/projectnb/modislc/projects/landsat_sentinel/v1_4/HLS30/',tile,'/fmask')
filesFmask <- list.files(path=imgDir,pattern=glob2rx(paste0('HLS*.',year,'*Fmask.tif')),full.names=T)

datesFmask <- as.Date(as.numeric(substr(filesFmask,88,90)),origin=paste0((year-1),'-12-31'))

#
baseImage <- raster(get_subdatasets(filesHLS[1])[1]) 
shpPoints$id <- as.numeric(paste0(shpPoints$id))
shpPoints <- spTransform(shpPoints, crs(baseImage))

pixID <- 1:(length(baseImage))
pixRast <-setValues(baseImage,pixID)
pixelNumbers <- extract(pixRast,shpPoints)

geog_crs = CRS("+proj=longlat +datum=WGS84")
shpLatLon <- spTransform(shpPoints,geog_crs)
Lat <- shpLatLon@coords[tt,2]
Lon <- shpLatLon@coords[tt,1]

# 
registerDoMC()

Sys.time()
datTable <- foreach(i=1:length(datesHLS),.combine=rbind) %dopar% {
  
  fileList  <- filesHLS[which(as.Date(as.numeric(substr(filesHLS,89,91)),origin=paste0((year-1),'-12-31'))==datesHLS[i])]
  fileFmask <- filesFmask[which(as.Date(as.numeric(substr(filesFmask,88,90)),origin=paste0((year-1),'-12-31'))==datesHLS[i])]
  Date      <- as.Date(as.numeric(substr(fileList[1],89,91)),origin=paste0((year-1),'-12-31'))
  
  if(length(fileList)==1){
    
    sds <- get_subdatasets(fileList)
    
    if(substr(fileList,74,76)=='L30'){
      b2 <- raster(sds[2])[pixelNumbers[tt]]
      b3 <- raster(sds[3])[pixelNumbers[tt]]
      b4 <- raster(sds[4])[pixelNumbers[tt]]
      b5 <- raster(sds[5])[pixelNumbers[tt]]
      b6 <- raster(sds[6])[pixelNumbers[tt]]
      b7 <- raster(sds[7])[pixelNumbers[tt]]
    }else{
      Fmask <- raster(fileFmask)[pixelNumbers[tt]]
      if(Fmask <2){
        b2 <- raster(sds[2])[pixelNumbers[tt]]
        b3 <- raster(sds[3])[pixelNumbers[tt]]
        b4 <- raster(sds[4])[pixelNumbers[tt]]
        b5 <- raster(sds[9])[pixelNumbers[tt]]
        b6 <- raster(sds[12])[pixelNumbers[tt]]
        b7 <- raster(sds[13])[pixelNumbers[tt]]  
      }else{
        b2 <- NA;b3 <- NA;b4 <- NA
        b5 <- NA;b6 <- NA;b7 <- NA
      }
    }
  }else{
    
    bandsTemp <- matrix(NA,length(fileList),6)
    for(j in 1:length(fileList)){
      sds <- get_subdatasets(fileList[j])
      
      if(substr(fileList[j],74,76)=='L30'){
        bandsTemp[j,1] <- raster(sds[2])[pixelNumbers[tt]]
        bandsTemp[j,2] <- raster(sds[3])[pixelNumbers[tt]]
        bandsTemp[j,3] <- raster(sds[4])[pixelNumbers[tt]]
        bandsTemp[j,4] <- raster(sds[5])[pixelNumbers[tt]]
        bandsTemp[j,5] <- raster(sds[6])[pixelNumbers[tt]]
        bandsTemp[j,6] <- raster(sds[7])[pixelNumbers[tt]]
      }else{
        bandsTemp[j,1] <- raster(sds[2])[pixelNumbers[tt]]
        bandsTemp[j,2] <- raster(sds[3])[pixelNumbers[tt]]
        bandsTemp[j,3] <- raster(sds[4])[pixelNumbers[tt]]
        bandsTemp[j,4] <- raster(sds[9])[pixelNumbers[tt]]
        bandsTemp[j,5] <- raster(sds[12])[pixelNumbers[tt]]
        bandsTemp[j,6] <- raster(sds[13])[pixelNumbers[tt]]
      }
    }
    b2 <- mean(bandsTemp[,1],na.rm=T)
    b3 <- mean(bandsTemp[,2],na.rm=T)
    b4 <- mean(bandsTemp[,3],na.rm=T)
    b5 <- mean(bandsTemp[,4],na.rm=T)
    b6 <- mean(bandsTemp[,5],na.rm=T)
    b7 <- mean(bandsTemp[,6],na.rm=T)
  }
  
  dat <- data.frame(Date,b2,b3,b4,b5,b6,b7)
}
Sys.time()

print(nrow(datTable))
datTable <- na.omit(datTable)
print(nrow(datTable))

Lat <- rep(Lat,nrow(datTable))
Lon <- rep(Lon,nrow(datTable))
Sensor <- rep('HLS',nrow(datTable))
datTable <- cbind(Sensor,Lat,Lon,datTable)

setwd('/projectnb/modislc/users/mkmoon/Planet/data/HLS_fusion/')
tt <- sprintf('%02d',tt)
save(datTable,file=paste0('HLS_fusion_HLS_',tt,'.rda'))
