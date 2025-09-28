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
shpPoints <- readOGR(paste0('/projectnb/modislc/projects/landsat_sentinel/MSLSP_assessment/shps/16SEJ_pts.shp'))

path <- '/projectnb/modislc/users/mkmoon/Planet/data/files'
filesSR <- list.files(path=path,pattern=glob2rx('*SR*.tif'),full.names=T)
filesDN <- list.files(path=path,pattern=glob2rx('*DN*.tif'),full.names=T)

yy <- substr(filesSR,53,54)
mm <- substr(filesSR,55,56)
dd <- substr(filesSR,57,58)
dates_all <- as.Date(paste(mm,'/',dd,'/',yy,sep=''),'%m/%d/%y')
dates <- unique(dates_all)

#
baseImage <- raster(filesSR[1]) 
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
datTable <- foreach(i=1:length(dates),.combine=rbind) %dopar% {
  
  fileSR <- filesSR[which(dates_all==dates[i])]
  fileDN <- filesDN[which(dates_all==dates[i])]
  Date   <- dates[i]
  
  if(length(fileSR)==1){
    
    b1  <- raster(fileSR,band=1)[pixelNumbers[tt]]/10000
    b2  <- raster(fileSR,band=2)[pixelNumbers[tt]]/10000
    b3  <- raster(fileSR,band=3)[pixelNumbers[tt]]/10000
    b4  <- raster(fileSR,band=4)[pixelNumbers[tt]]/10000
    udm <- raster(fileDN)[pixelNumbers[tt]]
    
  }else{
    
    b1  <- vector('list',length(fileSR))
    b2  <- vector('list',length(fileSR))
    b3  <- vector('list',length(fileSR))
    b4  <- vector('list',length(fileSR))
    udm <- vector('list',length(fileSR))
    for(j in 1:length(fileSR)){
      b1[[j]]  <- raster(fileSR[j],band=1)
      b2[[j]]  <- raster(fileSR[j],band=2)
      b3[[j]]  <- raster(fileSR[j],band=3)
      b4[[j]]  <- raster(fileSR[j],band=4)
      udm[[j]] <- raster(fileDN[j])
    }
    b1$fun <- mean; b1$na.rm <- T; b1 <- do.call(mosaic,b1)
    b2$fun <- mean; b2$na.rm <- T; b2 <- do.call(mosaic,b2)
    b3$fun <- mean; b3$na.rm <- T; b3 <- do.call(mosaic,b3)
    b4$fun <- mean; b4$na.rm <- T; b4 <- do.call(mosaic,b4)
    udm$fun <- max; udm$na.rm <- T; udm <- do.call(mosaic,udm)
    
    b1   <- b1[pixelNumbers[tt]]/10000
    b2   <- b2[pixelNumbers[tt]]/10000
    b3   <- b3[pixelNumbers[tt]]/10000
    b4   <- b4[pixelNumbers[tt]]/10000
    udm  <- udm[pixelNumbers[tt]]/10000
    
  }
  
  if(udm > 0 | is.na(udm)){
    b1 <- NA;b2 <- NA;b3 <- NA;b4 <- NA
  }
  
  dat <- data.frame(Date,b1,b2,b3,b4)  
}
  
print(nrow(datTable))
datTable <- na.omit(datTable)
print(nrow(datTable))

Lat    <- rep(Lat,nrow(datTable))
Lon    <- rep(Lon,nrow(datTable))
Sensor <- rep('Planet',nrow(datTable))
datTable <- cbind(Sensor,Lat,Lon,datTable)

setwd('/projectnb/modislc/users/mkmoon/Planet/data/HLS_fusion/')
tt <- sprintf('%02d',tt)
save(datTable,file=paste0('HLS_fusion_Planet_',tt,'.rda'))


# #################
# files <- list.files(path='/projectnb/modislc/users/mkmoon/Planet/data/HLS_fusion/',
#                     pattern=glob2rx('HLS*Planet*.rda'))
# for(i in 1:10){
#   load(files[i])
#   if(i==1){
#     theTable <- datTable
#   }else{
#     dat <- datTable
#     theTable <- rbind(theTable,dat)
#   }
# }
# evi2 <- (2.5*(theTable$b4-theTable$b3)/(theTable$b4+2.4*theTable$b3+1))
# ndvi <- ((theTable$b4-theTable$b3)/(theTable$b4+theTable$b3))
# nirv <- theTable$b4*((theTable$b4-theTable$b3)/(theTable$b4+theTable$b3))
# 
# theTable <- cbind(theTable,ndvi,evi2,nirv)
# write.csv(theTable,file='datafusion_Planet.csv')
# 
# 
# files <- list.files(path='/projectnb/modislc/users/mkmoon/Planet/data/HLS_fusion/',
#                     pattern=glob2rx('HLS*HLS*.rda'))
# for(i in 1:10){
#   load(files[i])
#   if(i==1){
#     theTable <- datTable
#   }else{
#     dat <- datTable
#     theTable <- rbind(theTable,dat)
#   }
# }
# evi2 <- (2.5*(theTable$b5-theTable$b4)/(theTable$b5+2.4*theTable$b4+1))
# ndvi <- ((theTable$b5-theTable$b4)/(theTable$b5+theTable$b4))
# nirv <- theTable$b5*((theTable$b5-theTable$b4)/(theTable$b5+theTable$b4))
# plot(nirv,evi2)
# 
# theTable <- cbind(theTable,ndvi,evi2,nirv)
# write.csv(theTable,file='datafusion_HLS.csv')

