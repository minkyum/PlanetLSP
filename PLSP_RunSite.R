library(raster)
library(rgdal)
library(gdalUtils)
library(ncdf4)
library(signal)
library(RColorBrewer)
library(doMC)

args <- commandArgs()
print(args)

cc <- as.numeric(args[3]) 


###############################
##
saveEVI2stack <- function(imgDir){
  
  path <- imgDir
  
  # Dates
  fileSR <- list.files(path=path,pattern=glob2rx('*SR*.tif'))
  fileDN <- list.files(path=path,pattern=glob2rx('*DN*.tif'))
  
  yy <- substr(fileSR,3,4)
  mm <- substr(fileSR,5,6)
  dd <- substr(fileSR,7,8)
  dates_all <- as.Date(paste(mm,'/',dd,'/',yy,sep=''),'%m/%d/%y')
  dates <- unique(dates_all)
  
  # Full file list
  fileSR <- list.files(path=path,pattern=glob2rx('*SR*.tif'),full.names=T)
  fileDN <- list.files(path=path,pattern=glob2rx('*DN*.tif'),full.names=T)
  
  imgFull <- NULL
  for(i in 1:length(dates)){
    ids <- which(dates_all==dates[i])  
    if(length(ids)==1){
      imgFull <- i
    }
  }
  imgBase <- raster(fileSR[which(dates_all==dates[imgFull])])  
  
  eviStack <- foreach(i=1:length(dates),.combine='c') %dopar% {
    
    ids <- which(dates_all==dates[i])
    
    if(length(ids)>1){
      temp <- vector('list',length(ids))
      for(j in 1:length(ids)){
        red <- raster(fileSR[ids[j]],band=3)/10000
        nir <- raster(fileSR[ids[j]],band=4)/10000
        udm <- raster(fileDN[ids[j]])
        vis <- 2.5*(nir-red)/(nir+2.4*red+1)
        vis[udm>2|is.na(udm)] <- NA
        temp[[j]] <- vis
      }
      
      for(rr in 1:length(ids)){
        log <- try(compareRaster(temp[[rr]],imgBase,extent=F,rowcol=F),
                   silent=T)
        if(inherits(log,'try-error')){
          temp[[rr]] <- projectRaster(temp[[rr]],imgBase)    
        }
      }
      temp$fun <- mean
      temp$na.rm <- T
      rast <- do.call(mosaic,temp)  
      
    }else{
      red <- raster(fileSR[ids],band=3)/10000
      nir <- raster(fileSR[ids],band=4)/10000
      udm <- raster(fileDN[ids])
      vis <- 2.5*(nir-red)/(nir+2.4*red+1)
      vis[udm>2|is.na(udm)] <- NA
      rast <- vis
    }
  }
  
  return(list(eviStack=eviStack,dates=dates,imgBase=imgBase))
}


###############################
registerDoMC()

imgDir <- '/projectnb/modislc/users/mkmoon/Planet/data/HF/files'


eviStack <- saveEVI2stack(imgDir)

dates <- eviStack$dates
imgBase <- eviStack$imgBase
eviStack <- eviStack$eviStack


###############################
chunk <- length(eviStack[[1]])%/%10

if(cc==11){
  chunks <- c((chunk*(cc-1)+1):length(eviStack[[1]]))  
}else{
  chunks <- c((chunk*(cc-1)+1):(chunk*cc))
}

  
pheme <- foreach(j=chunks,.combine='c') %dopar% {
    
  eviPoint <- matrix(NA,length(eviStack),1)
  for(i in 1:length(eviStack)){
    eviPoint[i,1] <- eviStack[[i]][j]
  }
  
  if(sum(!is.na(eviPoint))>50 & max(diff(dates[!is.na(eviPoint)]))<30){
    bgevi <- quantile(eviPoint[which(as.numeric(substr(as.character(dates),6,7))<3|
                                       as.numeric(substr(as.character(dates),6,7))>10)],0.9,na.rm=T)
    eviPoint[eviPoint<bgevi] <- bgevi
      
    vitime <- matrix(NA,365,1)
    vitime[c(1,365)] <- bgevi 
    for(i in 1:length(dates)){
      vitime[as.numeric(strftime(dates[i], format = "%j")),1] <- eviPoint[i]  
    }
    xd <- 1:365
    aa <- approx(xd,vitime,n=365)[[2]]
    yd <- sgolayfilt(aa)
    dat <- data.frame(xd,yd)
    
    spl <- smooth.spline(yd,spar=0.4)
    yyd <- predict(spl)[[2]]
    
    
    pheme <- which(yyd>(max(yyd)+min(yyd))/2)[1] 
  }else{
    pheme <- NA
  }
}

setwd('/projectnb/modislc/users/mkmoon/Planet/phe/')
cc <- sprintf('%02d',cc)
save(pheme,file=paste('phePlanet_HF_',cc,'.rda',sep=''))


################################
path <- '/projectnb/modislc/users/mkmoon/Planet/phe'
fileSP <- list.files(path,pattern=glob2rx('*HF*.rda'),full.names=T)

for(cc in 1:length(fileSP)){
  load(fileSP[cc])
  if(cc==1){
    pheMat <- pheme
  }else{
    pheMat <- c(pheMat,pheme)
  }
}


path <- imgDir

# Dates
fileSR <- list.files(path=path,pattern=glob2rx('*SR*.tif'))
fileDN <- list.files(path=path,pattern=glob2rx('*DN*.tif'))

yy <- substr(fileSR,3,4)
mm <- substr(fileSR,5,6)
dd <- substr(fileSR,7,8)
dates_all <- as.Date(paste(mm,'/',dd,'/',yy,sep=''),'%m/%d/%y')
dates <- unique(dates_all)

plot(dates)

# Full file list
fileSR <- list.files(path=path,pattern=glob2rx('*SR*.tif'),full.names=T)
fileDN <- list.files(path=path,pattern=glob2rx('*DN*.tif'),full.names=T)

imgFull <- NULL
for(i in 1:length(dates)){
  ids <- which(dates_all==dates[i])
  if(length(ids)==1){
    imgFull <- i
  }
}
imgBase <- raster(fileSR[which(dates_all==dates[imgFull])])

phePn <- setValues(imgBase,pheMat)

plot(phePn)

setwd('/projectnb/modislc/users/mkmoon/Planet/phe/')
writeRaster(phePn,filename='phe_Planet_2019_HF.tif',format="GTiff",overwrite=T)


################################
tile <- '18TYN'
fileHLS <- paste0('/projectnb/modislc/projects/landsat_sentinel/MSLSP_HLS30/',tile,'/phenoMetrics/MSLSP_',tile,'_2019.nc')

nc <- nc_open(fileHLS)
var <- names(nc[['var']])
pheHLS <- raster(fileHLS,varname=var[4])
pheHLS <- crop(pheHLS,phePn)
plot(pheHLS)
hist(pheHLS)

setwd('/projectnb/modislc/users/mkmoon/Planet/phe/')
writeRaster(pheHLS,filename="phe_HLS_2019_HF.tif",format="GTiff",overwrite=T)

#
phePn30 <- projectRaster(phePn,pheHLS)

htPn  <- hist(phePn,breaks=seq(0,400,1),plot=F)
htHLS <- hist(pheHLS,breaks=seq(0,400,1),plot=F)
htPn$counts <- htPn$counts/max(htPn$counts)
htHLS$counts <- htHLS$counts/max(htHLS$counts)

par(mfrow=c(1,2))
plot(values(pheHLS),values(phePn30),xlim=c(100,190),ylim=c(100,190))
abline(0,1)
summary(lm(values(phePn30)~values(pheHLS)))
abline(lm(values(phePn30)~values(pheHLS)))

plot(htPn,xlim=c(120,180),col=rgb(1,0,0,0.6))
plot(htHLS,col=rgb(0,0,1,0.6),add=T)

# 
# # Flux tower data
# geog_crs = CRS("+proj=longlat +datum=WGS84")
# site <- data.frame(1,-72.171478,42.537755)
# colnames(site) <- c('id','lon','lat')
# xy   <- site[,c(2,3)]
# bb   <- SpatialPointsDataFrame(coords=xy,data=site,proj4string=geog_crs)
# bb <- spTransform(bb,crs(phePn))
# 
# setwd('/projectnb/modislc/users/mkmoon/Planet/shp/')
# writeOGR(bb,'.','18TYN_pts',driver="ESRI Shapefile",overwrite=T)
# 
# plot(phePn)
# plot(bb,add=T)
# 
pixNum <- setValues(phePn,1:length(phePn))
bb <- spTransform(bb,crs(phePn))
pixid <- extract(pixNum,bb,buffer=45)

eviMat <- matrix(NA,length(pixid[[1]]),length(dates))
for(i in 1:length(dates)){
  eviMat[,i] <- eviStack[[i]][pixid[[1]]]
}

# Plot
for(i in 1:length(dates)){
  if(i==1){
    plot(dates,eviMat[i,],ylim=c(0,0.8))
  }else{
    points(dates,eviMat[i,])
  }
}
eviMatMedi <- apply(eviMat,2,mean)
eviMatSD <- apply(eviMat,2,sd)

eviHLS <- matrix(NA,9,length(theTable[[1]]$dates))
for(i in 1:9){
  eviHLS[i,] <- theTable[[i]]$original_VI
}
eviHLSMedi <- apply(eviHLS,2,mean)
eviHLSSD <- apply(eviHLS,2,sd)


# Phenocam
gcc <- read.csv('/projectnb/modislc/users/mkmoon/Phenocam/PhenoCam_V2_1674/data/harvard_DB_1000_3day.csv',
                skip=24)
gcc90   <- gcc[gcc$year==2018,c('date','gcc_90')]

# flux
gpp <- read.csv('/projectnb/modislc/users/mkmoon/Planet/tower/hf004-02-filled.csv')
gpp <- gpp[gpp$year==2019,]
gpp <- gpp$gee
gpp <- matrix(gpp,24,365)
gpp <- -apply(gpp,2,mean)
gpp <- gpp*10^-6*(60*60*24)*(44)

###########
setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
png(filename='timeseries_HF.png',width=11.5,height=6,units='in',res=300)

par(oma=c(2,2,1,2),mar=c(4,8,1,4),mgp=c(2.5,1,0))
plot(dates,eviMatMedi,pch=19,col='red',
     xlim=c(17540+365,17900+365),ylim=c(0.05,0.85),
     xlab='',ylab='',cex.lab=1.5,
     cex.axis=1.5)
points(theTable[[i]]$dates,eviHLSMedi,pch=19,col='blue')

# load('/projectnb/modislc/users/mkmoon/autumn/sample_data/mms_gppFilled.rda')
# gpp_day <- matrix(gpp,48,length(gpp)/48)
# gpp_day <- apply(gpp_day,2,mean)*0.0864*44/2

par(new = TRUE)
plot(as.Date(11:357,origin='2018-12-31'),gpp[11:357],
     axes=F,bty="n",xlab="",ylab="",col='black',pch=19,
     ylim=c(-18,75),type='l',lwd=1.2)
axis(side=4,at=seq(-50,100,20),cex.axis=1.5)
mtext(expression(paste('GPP (g CO'[2],' ',m^-2,' ',day^-1,')',sep='')),
      side=4,line=3,cex=1.5)

par(new = TRUE)
plot(dates,eviMatMedi,pch=19,col='red',
     xlim=c(17540+365,17900+365),ylim=c(0.05,0.85),
     xlab='2019',ylab='EVI2',cex.lab=1.5,
     cex.axis=1.5,cex=1.5)
points(theTable[[i]]$dates,eviHLSMedi,pch=19,col='blue',cex=1.5)
arrows(dates,eviMatMedi-eviMatSD,dates,eviMatMedi+eviMatSD,
       code=1,length=0,col='red')
arrows(theTable[[i]]$dates,eviHLSMedi-eviHLSSD,theTable[[i]]$dates,eviHLSMedi+eviHLSSD,
       code=1,length=0,col='blue')

par(new = TRUE)
plot(as.Date(1:365,origin='2017-12-31'),gcc90$gcc_90,
     ylim=c(0.34,0.507),axes=F,bty="n",xlab="",ylab="",col='forestgreen',pch=19)
axis(side=2,at=seq(0,1,0.05),cex.axis=1.5,line=4)
mtext('GCC',side=2,line=6.5,cex=1.5)

legend('topleft',c('HLS','Planet','GCC','GPP'),
       pch=c(19,19,19,NA),
       lty=c(NA,NA,NA,1),
       lwd=c(NA,NA,NA,1),
       col=c('blue','red','forestgreen','black'),
       cex=1.5,bty='n',pt.cex=c(1.7,1.7,1.3,NA))

# abline(v=as.Date(mean(extract(pheHLS,bb,buffer=45)[[1]]),origin='2017-12-31'),lty=5)
# abline(v=as.Date(mean(extract(phePn,bb,buffer=45)[[1]]),origin='2017-12-31'),lty=5)
dev.off()
