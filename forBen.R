library(raster)
library(rgdal)
library(gdalUtils)
library(ncdf4)
library(signal)
library(RColorBrewer)
library(doMC)
library(scales)
library(rjson)
library(imager)   #needed for efficient distance to snow calculate
library(matrixStats)   
library(WGCNA)
library(zoo)
library(RcppRoll)

args <- commandArgs()
print(args)

cc <- as.numeric(args[3])

# ###############################
# # Make shape-polygon
# setwd('/projectnb/modislc/users/mkmoon/Planet/shp/sites/')
# 
# geog_crs = CRS("+proj=longlat +datum=WGS84")
# utm_crs = CRS("+proj=utm +zone=13 +datum=WGS84 +units=m")
# site <- data.frame(-106.9898,38.9592)
# colnames(site) <- c('lon','lat')
# xy   <- site[,c(1,2)]
# bb   <- SpatialPointsDataFrame(coords=xy,data=site,proj4string=geog_crs)
# bb   <- spTransform(bb,utm_crs)
# 
# x1 <- bb@coords[1] - 1500
# x2 <- bb@coords[1] + 1500
# y1 <- bb@coords[2] - 1500
# y2 <- bb@coords[2] + 1500
# xCoor <- c(x1,x2,x2,x1)
# yCoor <- c(y1,y1,y2,y2)
# xym <- cbind(xCoor,yCoor)
# p   <- Polygon(xym)
# ps  <- Polygons(list(p),1)
# sps <- SpatialPolygons(list(ps))
# proj4string(sps) <- utm_crs
# data <- data.frame(f=99.9)
# spdf <- SpatialPolygonsDataFrame(sps,data)
# spdf <- spTransform(spdf,geog_crs)
# 
# # spdf <- spTransform(spdf,crs(shpHLS))
# # print(intersect(shpHLS,spdf))
# 
# writeOGR(spdf,".","colorado",driver="ESRI Shapefile",overwrite=T)




# HLS tile: 13SCD
###############################
source('/usr3/graduate/mkmoon/GitHub/PlanetLSP/PLSP_Functions.R')
source('/usr3/graduate/mkmoon/GitHub/MSLSP/MSLSP_Functions.r')
params <- fromJSON(file='/usr3/graduate/mkmoon/GitHub/MSLSP/MSLSP_Parameters.json')

registerDoMC()

#######
imgDir <- '/projectnb/modislc/users/mkmoon/Planet/data/Planet_SR/CR/files'

datStack <- saveEVI2stack(imgDir)

setwd('/projectnb/modislc/users/mkmoon/Planet/data/')
save(datStack,file='planet_eviStack_cr.rda')


###############################
# Time series
shpPoints <- readOGR('/projectnb/modislc/users/mkmoon/Planet/shp/13SCD_pts_1.shp')

datPTS1 <- extractTS(shpPoints,datStack,id=1)
datPTS2 <- extractTS(shpPoints,datStack,id=2)
datPTS3 <- extractTS(shpPoints,datStack,id=3)
datPTS4 <- extractTS(shpPoints,datStack,id=4)
datPTS5 <- extractTS(shpPoints,datStack,id=5)
datPTS6 <- extractTS(shpPoints,datStack,id=6)

plot(datStack$dates,datPTS6)


par(mfrow=c(1,1),oma=c(2,1,0,1),mar=c(0.2,5,1,5),mgp=c(2.5,1,0))
plot(datStack$dates,datPTS1,
     ylim=c(-0.1,1),axe=F,ann=F,pch=1,cex=1.1)
box(lty=1)
axis(2,seq(0,1,0.2),cex.axis=1.3)
mtext('EVI2',2,2.4,cex=1.3)

points(dates,apply(datPTS2,2,median,na.rm=T),pch=2,ylim=c(0,1),col='blue')
points(dates,apply(datPTS3,2,median,na.rm=T),pch=3,ylim=c(0,1))
points(dates,apply(datPTS4,2,median,na.rm=T),pch=4,ylim=c(0,1))
points(dates,apply(datPTS5,2,median,na.rm=T),pch=5,ylim=c(0,1))
axis(1,at=as.Date(c(99,366,(365*2+1),(365*3+1)),origin='2016-12-31'),
     c('Apr 2017','Jan 2018','Jan 2019','Jan 2020'),
     cex.axis=1.3)


###############################
load('/projectnb/modislc/users/mkmoon/Planet/data/planet_eviStack_cr.rda')

dates    <- datStack$dates
imgBase  <- datStack$imgBase
bluStack <- datStack$b1
greStack <- datStack$b2
redStack <- datStack$b3
nirStack <- datStack$b4
snwStack <- datStack$snowPix

imgBase   <- imgBase
phenYrs <- '2019' 

# chunk <- length(imgBase)%/%20
# if(cc==20){
#   chunks <- c((chunk*(cc-1)+1):length(eviStack[[1]]))  
# }else{
#   chunks <- c((chunk*(cc-1)+1):(chunk*cc))
# }
# print(length(chunks))

pheno_mat <- foreach(j=1:length(imgBase),.combine='rbind') %dopar% {

  blue  <- bluStack[j,]
  green <- greStack[j,]
  red   <- redStack[j,]
  nir   <- nirStack[j,]
  snow <- snwStack[j,]
  
  DoPhenologyPlanet(blue,green,red,nir,snow,dates,phenYrs,params)
}

# setwd('/projectnb/modislc/users/mkmoon/Planet/phe/2019/CR/')
# cc <- sprintf('%02d',cc)
# save(pheno_mat,file=paste0('phePlanet_CR_',cc,'.rda'))


################################
# load('/projectnb/modislc/users/mkmoon/Planet/data/planet_eviStack_cr.rda')
# 
# path <- paste0('/projectnb/modislc/users/mkmoon/Planet/phe/2019/CR')
# filePhe <- list.files(path,pattern=glob2rx('phe*.rda'),full.names=T)
# 
# for(cc in 1:length(filePhe)){
#   load(filePhe[cc])
#   if(cc==1){
#     pheMat <- pheno_mat
#   }else{
#     pheMat <- rbind(pheMat,pheno_mat)
#   }
# }

rastPP <- vector('list',11)
for(rr in 1:11){
  rastPP[[rr]] <- setValues(imgBase,pheno_mat[,rr])
}

# par(mfrow=c(1,3))
# plot(rastPP[[2]]);plot(rastPP[[6]]);plot(rastPP[[8]])
# hist(rastPP[[2]]);hist(rastPP[[6]]);hist(rastPP[[8]])
# setwd(paste0('/projectnb/modislc/users/mkmoon/Planet/phe/',Year,'/',vgt[ss]))
# writeRaster(rastPP[[2]],filename=paste0('phe_Planet_',Year,'_sos_',vgt[ss],'.tif'),format="GTiff",overwrite=T)
# writeRaster(rastPP[[6]],filename=paste0('phe_Planet_',Year,'_eos_',vgt[ss],'.tif'),format="GTiff",overwrite=T)
# writeRaster(rastPP[[8]],filename=paste0('phe_Planet_',Year,'_amp_',vgt[ss],'.tif'),format="GTiff",overwrite=T)

var <- c('OGI','50PCGI','OGMx','Peak','OGD','50PCGD','OGMn',
         'EVImax','EVIamp','EVIarea','NumCycles')

setwd('/projectnb/modislc/users/mkmoon/Planet/phe/2019/CR')
for(ii in 1:11){
  writeRaster(rastPP[[ii]],filename=paste0('phe_Planet_2019_CR_',ii,'_',var[ii],'.tif'),format="GTiff",overwrite=T)
}
