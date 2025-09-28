#Load required libraries
library(raster)
library(rgdal)
library(gdalUtils)
library(rgeos)

args <- commandArgs()
print(args)

tt <- as.numeric(args[3]) 
# tt <- 7


###########################################
sites <- c('bozeman','kansas','NEON.D10.CPER.DP1.00033','NEON.D09.WOOD.DP1.00033','NEON.D06.KONZ.DP1.00033','rosemountnprs','vaira')
tiles <- c('12TWR','15SUD','13TEF','14TMT','14SQJ','15TVK','10SFH')
lats  <- c(45.7831,39.0561,40.8155,47.1282,39.1008,44.6781,38.4122)
lons  <- c(-110.7778,-95.1907,-104.7456,-99.2413,-96.5631,-93.0723,-120.9506)


# List of HLS images
imgDir <- paste0('/projectnb/modislc/users/mkmoon/Planet/glasslands/mslsp/input/HLS30/',tiles[tt],'/images')
filesHLS <- list.files(path=imgDir,pattern=glob2rx('HLS*.*.hdf'),full.names=T)

baseImage <- raster(get_subdatasets(filesHLS[1])[1]) 

# Get dates
datesHLS <- as.Date(as.numeric(substr(filesHLS,101,103)),origin=paste0((as.numeric(substr(filesHLS,97,100))-1),'-12-31'))
datesHLS <- unique(datesHLS)
datesHLS <- datesHLS[order(datesHLS)]


# Point Shape file
geog_crs = CRS("+proj=longlat +datum=WGS84")
site <- data.frame(1,lons[tt],lats[tt])
colnames(site) <- c('id','xcrd','ycrd')
xy   <- site[,c(2,3)]
bb   <- SpatialPointsDataFrame(coords=xy,data=site,proj4string=geog_crs)
utm_crs   <- baseImage@crs
shpPoint <- spTransform(bb,utm_crs)

# Fmask
imgDir <- paste0('/projectnb/modislc/users/mkmoon/Planet/glasslands/mslsp/input/HLS30/',tiles[tt],'/fmask')
filesFmask <- list.files(path=imgDir,pattern=glob2rx('HLS*.*Fmask.tif'),full.names=T)


#
pixID <- 1:(length(baseImage))
pixRast <-setValues(baseImage,pixID)
pixelNumber <- extract(pixRast,shpPoint)


# 
print(length(datesHLS))

datTable <- matrix(NA,length(datesHLS),6)
for(i in 1:length(datesHLS)){
# datTable <- foreach(i=1:length(datesHLS),.combine=rbind) %dopar% {
  
  fileList  <- filesHLS[which(as.Date(as.numeric(substr(filesHLS,101,103)),origin=paste0((as.numeric(substr(filesHLS,97,100))-1),'-12-31'))==datesHLS[i])]
  fileFmask <- filesFmask[which(as.Date(as.numeric(substr(filesFmask,100,102)),origin=paste0((as.numeric(substr(filesFmask,96,99))-1),'-12-31'))==datesHLS[i])]
  Date      <- as.Date(as.numeric(substr(fileList[1],101,103)),origin=paste0((as.numeric(substr(fileList,97,100))-1),'-12-31'))
  
  if(length(fileList)==1){
    
    sds <- get_subdatasets(fileList)
    
    if(substr(fileList,86,88)=='L30'){
      b2 <- raster(sds[2])[pixelNumber]
      b3 <- raster(sds[3])[pixelNumber]
      b4 <- raster(sds[4])[pixelNumber]
      b5 <- raster(sds[5])[pixelNumber]
      b6 <- raster(sds[6])[pixelNumber]
      b7 <- raster(sds[7])[pixelNumber]
    }else{
      log <- try({    
        Fmask <- raster(fileFmask)[pixelNumber]
        if(Fmask <2){
          b2 <- raster(sds[2])[pixelNumber]
          b3 <- raster(sds[3])[pixelNumber]
          b4 <- raster(sds[4])[pixelNumber]
          b5 <- raster(sds[9])[pixelNumber]
          b6 <- raster(sds[12])[pixelNumber]
          b7 <- raster(sds[13])[pixelNumber]  
        }else{
          b2 <- NA;b3 <- NA;b4 <- NA
          b5 <- NA;b6 <- NA;b7 <- NA
        }
      },silent=TRUE)
      if(inherits(log, "try-error")){
        b2 <- NA;b3 <- NA;b4 <- NA
        b5 <- NA;b6 <- NA;b7 <- NA
      } 
    }
  }else{
    
    bandsTemp <- matrix(NA,length(fileList),6)
    for(j in 1:length(fileList)){
      sds <- get_subdatasets(fileList[j])
      
      if(substr(fileList[j],86,88)=='L30'){
        bandsTemp[j,1] <- raster(sds[2])[pixelNumber]
        bandsTemp[j,2] <- raster(sds[3])[pixelNumber]
        bandsTemp[j,3] <- raster(sds[4])[pixelNumber]
        bandsTemp[j,4] <- raster(sds[5])[pixelNumber]
        bandsTemp[j,5] <- raster(sds[6])[pixelNumber]
        bandsTemp[j,6] <- raster(sds[7])[pixelNumber]
      }else{
        bandsTemp[j,1] <- raster(sds[2])[pixelNumber]
        bandsTemp[j,2] <- raster(sds[3])[pixelNumber]
        bandsTemp[j,3] <- raster(sds[4])[pixelNumber]
        bandsTemp[j,4] <- raster(sds[9])[pixelNumber]
        bandsTemp[j,5] <- raster(sds[12])[pixelNumber]
        bandsTemp[j,6] <- raster(sds[13])[pixelNumber]
      }
    }
    b2 <- mean(bandsTemp[,1],na.rm=T)
    b3 <- mean(bandsTemp[,2],na.rm=T)
    b4 <- mean(bandsTemp[,3],na.rm=T)
    b5 <- mean(bandsTemp[,4],na.rm=T)
    b6 <- mean(bandsTemp[,5],na.rm=T)
    b7 <- mean(bandsTemp[,6],na.rm=T)
  }
  
  datTable[i,] <- c(b2,b3,b4,b5,b6,b7)
  print(i)
  # dat <- data.frame(Date,b2,b3,b4,b5,b6,b7)
}

datTABLE <- data.frame(datesHLS,datTable)
colnames(datTABLE) <- c('date','b2-blue','b3-green','b4-red','b5-nir','b6-swir1','b7-swir2')

print(nrow(datTABLE))
datTABLE <- na.omit(datTABLE)
print(nrow(datTABLE))

# Save 
setwd('/projectnb/modislc/users/mkmoon/Planet/glasslands/data/HLS_bands/')
write.csv(datTABLE,file=paste0('HLS_bands_',sites[tt],'.csv'))


# ###########################################
# dat <- read.csv('/projectnb/modislc/users/mkmoon/Planet/glasslands/data/HLS_bands/HLS_bands_vaira.csv')
# 
# evi2 <- 2.5*(dat$b5.nir-dat$b4.red)/(dat$b5.nir+2.4*dat$b4.red+1)
# dates <- as.Date(dat$date)
# plot(dates,evi2)
