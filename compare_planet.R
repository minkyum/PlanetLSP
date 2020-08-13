library(raster)
library(gdalUtils)
library(ncdf4)
library(signal)
library(RColorBrewer)

args <- commandArgs()
print(args)

tt <- as.numeric(substr(args[3],1,1)) 
cc <- as.numeric(substr(args[3],2,4)) 

###############################
path <- '/projectnb/modislc/users/mkmoon/Planet/data/files'
fileSR <- list.files(path=path,pattern=glob2rx('*SR*.tif'),full.names=T)
fileDN <- list.files(path=path,pattern=glob2rx('*DN*.tif'),full.names=T)

yy <- substr(fileSR,53,54)
mm <- substr(fileSR,55,56)
dd <- substr(fileSR,57,58)
dates_all <- as.Date(paste(mm,'/',dd,'/',yy,sep=''),'%m/%d/%y')
dates <- unique(dates_all)
plot(dates)

eviStack <- vector('list',length(dates))
for(i in 1:length(dates)){
  ids <- which(dates_all==dates[i])
  if(length(ids)>1){
    temp <- vector('list',length(ids))
    for(j in 1:length(ids)){
      red <- raster(fileSR[ids[j]],band=3)/10000
      nir <- raster(fileSR[ids[j]],band=4)/10000
      udm <- raster(fileDN[ids[j]])
      vis <- 2.5*(nir-red)/(nir+2.4*red+1)
      vis[udm==2] <- NA
      temp[[j]] <- vis
    }
    temp$fun <- mean
    temp$na.rm <- T
    rast <- do.call(mosaic,temp)
  }else{
    red <- raster(fileSR[ids],band=3)/10000
    nir <- raster(fileSR[ids],band=4)/10000
    udm <- raster(fileSR[ids])
    vis <- 2.5*(nir-red)/(nir+2.4*red+1)
    vis[udm==2] <- NA
    rast <- 2.5*(nir-red)/(nir+2.4*red+1)
  }
  eviStack[[i]] <- rast
  print(i)
}

# setwd('/projectnb/modislc/users/mkmoon/Planet/phe/')
# save(eviStack,file=paste('phePlanet_2018.rda',sep=''))

pheMat <- matrix(NA,length(eviStack[[1]]),1)

if(cc==15){
  chunks <- c((100000*(cc-1)+1):length(eviStack[[1]]))  
}else{
  chunks <- c((100000*(cc-1)+1):(100000*cc))
}

for(j in chunks){
  eviPoint <- matrix(NA,length(eviStack),1)
  for(i in 1:length(eviStack)){
    eviPoint[i,1] <- eviStack[[i]][j]
  }
  
  if(sum(!is.na(eviPoint))>50 & max(diff(dates[!is.na(eviPoint)]))<30){
    bgevi <- quantile(eviPoint[c(1:12,77:82)],0.9,na.rm=T)
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
    try({
      if(tt==1){
        ydhat <- nls(yd ~ m1+(m2)/(1 + exp((m3-xd)/m4))-(m2)/(1 + exp((m5-xd)/m6)),
                     data=dat,
                     start = list(m1=bgevi,
                                  m2=0.5,
                                  m3=which(yd>(max(yd)+bgevi)/2)[1],
                                  m4=8,
                                  m5=285,
                                  m6=7))
        yyd <- predict(ydhat)
      }else{
        spl <- smooth.spline(yd,spar=0.4)
        yyd <- predict(spl)[[2]]
      }
     
      pheMat[j,1] <- which(yyd>(max(yyd)+min(yyd))/2)[1]  
    },silent=T)
  }
  if(j%%1000==0) print(j)
}

setwd('/projectnb/modislc/users/mkmoon/Planet/phe/')
cc <- sprintf('%02d',cc)
save(pheMat,file=paste('phePlanet_',tt,'_',cc,'.rda',sep=''))

################################
load('/projectnb/modislc/users/mkmoon/Planet/phe/phePlanet_2018.rda')

path <- '/projectnb/modislc/users/mkmoon/Planet/phe'
fileSP <- list.files(path,pattern=glob2rx('*_2_*.rda'),full.names=T)

for(cc in 1:15){
  load(fileSP[cc])

  if(cc==1){
    pheMAT <- pheMat
  }

  if(cc==15){
    chunks <- c((100000*(cc-1)+1):length(eviStack[[1]]))
  }else{
    chunks <- c((100000*(cc-1)+1):(100000*cc))
  }

  pheMAT[chunks] <- pheMat[chunks]
}
phePn <- setValues(eviStack[[1]],pheMAT)
plot(phePn)
hist(phePn,breaks=seq(0,400,1),xlim=c(80,170))

# setwd('/projectnb/modislc/users/mkmoon/Planet/phe/')
# writeRaster(phePn, filename="pheSP.tif", format="GTiff", overwrite=TRUE)

################################
nc <- nc_open('/projectnb/modislc/projects/landsat_sentinel/MSLSP_HLS30/16SEJ/phenoMetrics/MSLSP_16SEJ_2018.nc')
var <- names(nc[['var']])
pheHLS <- raster('/projectnb/modislc/projects/landsat_sentinel/MSLSP_HLS30/16SEJ/phenoMetrics/MSLSP_16SEJ_2018.nc',
                 varname=var[4])
pheHLS <- crop(pheHLS,phePn)
plot(pheHLS)
hist(pheHLS)

# setwd('/projectnb/modislc/users/mkmoon/Planet/phe/')
# writeRaster(pheHLS, filename="pheHLS.tif", format="GTiff", overwrite=TRUE)

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


# Flux tower data
geog_crs = CRS("+proj=longlat +datum=WGS84")
site <- data.frame(1,-86.4131,39.3232)
colnames(site) <- c('id','lon','lat')
xy   <- site[,c(2,3)]
bb   <- SpatialPointsDataFrame(coords=xy,data=site,proj4string=geog_crs)
bb <- spTransform(bb,crs(phePn))

plot(phePn)
plot(bb,add=T)

pixNum <- setValues(phePn,1:length(phePn))
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

eviHLS <- matrix(NA,9,length(theTable[[i]]$dates))
for(i in 1:9){
  eviHLS[i,] <- theTable[[i]]$original_VI
}
eviHLSMedi <- apply(eviHLS,2,mean)
eviHLSSD <- apply(eviHLS,2,sd)


# Phenocam
gcc <- read.csv('/projectnb/modislc/users/mkmoon/Phenocam/PhenoCam_V2_1674/data/morganmonroe_DB_1000_3day.csv',
                skip=24)
gcc90   <- gcc[gcc$year==2018,c('date','gcc_90')]

###########
setwd('/projectnb/modislc/users/mkmoon/Planet/phe/')
png(filename='timeseries_1.png',width=11.5,height=6,units='in',res=300)
par(oma=c(2,2,1,2),mar=c(4,8,1,4),mgp=c(2.5,1,0))
plot(dates,eviMatMedi,pch=19,col='red',
     xlim=c(17540,17900),ylim=c(0,0.8),
     xlab='',ylab='',cex.lab=1.5,
     cex.axis=1.5)
points(theTable[[i]]$dates,eviHLSMedi,pch=19,col='blue')

# load('/projectnb/modislc/users/mkmoon/autumn/sample_data/mms_gppFilled.rda')
# gpp_day <- matrix(gpp,48,length(gpp)/48)
# gpp_day <- apply(gpp_day,2,mean)*0.0864*44/2

par(new = TRUE)
plot(as.Date(25:360,origin='2017-12-31'),gpp_day[(1096-364+24):(1096-5)]*0.0864*44/2,
    axes=F,bty="n",xlab="",ylab="",col='black',pch=19,
    ylim=c(-10,50),type='l',lwd=1.2)
axis(side=4,at=seq(-50,100,10),cex.axis=1.5)
mtext(expression(paste('GPP (g CO'[2],' ',m^-2,' ',day^-1,')',sep='')),
      side=4,line=3,cex=1.5)

par(new = TRUE)
plot(dates,eviMatMedi,pch=19,col='red',
     xlim=c(17540,17900),ylim=c(0,0.8),
     xlab='2018',ylab='EVI2',cex.lab=1.5,
     cex.axis=1.5,cex=1.5)
points(theTable[[i]]$dates,eviHLSMedi,pch=19,col='blue',cex=1.5)
arrows(dates,eviMatMedi-eviMatSD,dates,eviMatMedi+eviMatSD,
       code=1,length=0,col='red')
arrows(theTable[[i]]$dates,eviHLSMedi-eviHLSSD,theTable[[i]]$dates,eviHLSMedi+eviHLSSD,
       code=1,length=0,col='blue')

par(new = TRUE)
plot(as.Date(1:365,origin='2017-12-31'),gcc90$gcc_90,
     ylim=c(0.32,0.39),axes=F,bty="n",xlab="",ylab="",col='forestgreen',pch=19)
axis(side=2,at=seq(0.25,0.4,0.02),cex.axis=1.5,line=4)
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
