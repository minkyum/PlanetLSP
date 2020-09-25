library(raster)
library(rgdal)
library(gdalUtils)
library(ncdf4)
library(signal)
library(RColorBrewer)
library(doMC)
library(scales)


###############################
#
sites <- c('NEON.D02.SCBI.DP1.00033',
           'NEON.D01.HARV.DP1.00033_',
           'uiefswitchgrass2',
           'burnssagebrush',
           'cperuvb',
           'howland1')
lat   <- rep(NA,length(sites))
lon   <- rep(NA,length(sites))
vgt1   <- rep(NA,length(sites))
vgt2   <- rep(NA,length(sites))
utm   <- c(17,18,16,11,13,19)

datCoor <- data.frame(sites,lon,lat,vgt1,vgt2,utm)

# Extract site coordinate
for(ss in 1:length(sites)){
  path <- '/projectnb/modislc/users/mkmoon/Planet/data/PhenoCam'
  sstr <- paste0(sites[ss],'*meta.txt')
  filename <- list.files(path=path,pattern=glob2rx(sstr),full.names=T,recursive=T)
  
  temp <- readChar(filename[1],file.info(filename[1])$size)
    
    lon <- sub('.*lon: ','',temp)
    lon <- sub('\n.*','',lon)  
  
    lat <- sub('.*lat: ','',temp)
    lat <- sub('\n.*','',lat)
    
    vgt1 <- sub('.*primary_veg_type: ','',temp)
    vgt1 <- sub('\n.*','',vgt1)
    
    vgt2 <- sub('.*secondary_veg_type: ','',temp)
    vgt2 <- sub('\n.*','',vgt2)
    
    datCoor$lon[ss] <- as.numeric(lon)
    datCoor$lat[ss] <- as.numeric(lat)
    datCoor$vgt1[ss] <- vgt1
    datCoor$vgt2[ss] <- vgt2
}


# # Make shape-polygon
# setwd('/projectnb/modislc/users/mkmoon/Planet/shp/sites/')
# shpHLS <- shapefile('/projectnb/modislc/projects/landsat_sentinel/shapefiles/sentinel2_tiles_world/sentinel2_tiles_world/sentinel2_tiles_north_america_Albers.shp')
# 
# geog_crs = CRS("+proj=longlat +datum=WGS84")
# for(ss in 1:length(sites)){
#   utm_crs = CRS(paste0("+proj=utm +zone=",datCoor$utm[ss]," +datum=WGS84 +units=m"))
#   
#   site <- data.frame(datCoor[ss,1:3])
#   colnames(site) <- c('id','lon','lat')
#   xy   <- site[,c(2,3)]
#   bb   <- SpatialPointsDataFrame(coords=xy,data=site,proj4string=geog_crs)
#   bb   <- spTransform(bb,utm_crs)
#   
#   x1 <- bb@coords[1] - 1500
#   x2 <- bb@coords[1] + 1500
#   y1 <- bb@coords[2] - 1500
#   y2 <- bb@coords[2] + 1500
#   xCoor <- c(x1,x2,x2,x1)
#   yCoor <- c(y1,y1,y2,y2)
#   xym <- cbind(xCoor,yCoor)
#   p   <- Polygon(xym)
#   ps  <- Polygons(list(p),1)
#   sps <- SpatialPolygons(list(ps))
#   proj4string(sps) <- utm_crs
#   data <- data.frame(f=99.9)
#   spdf <- SpatialPolygonsDataFrame(sps,data)
#   spdf <- spTransform(spdf,geog_crs)
# 
#   # spdf <- spTransform(spdf,crs(shpHLS))
#   # print(intersect(shpHLS,spdf))
#   
#   writeOGR(spdf,".",paste0(datCoor$sites[ss]),driver="ESRI Shapefile",overwrite=T)
# }

## GCC
# Extract 3 day transition data file list
sites <- c('NEON.D02.SCBI.DP1.00033',
           
           'NEON.D01.HARV.DP1.00033_DB',
           'NEON.D01.HARV.DP1.00033_EN',
           
           'uiefswitchgrass2',
           'uiefprairie2',
           'uiefmiscanthus2',
           'uiefmaize2',
           
           'burnssagebrush','cperuvb',
           
           'howland1')


datGcc <- matrix(NA,(365*3),length(sites)+1)
datGcc[,1] <- as.Date(1:(365*3),origin='2016-12-31')

for(ss in 1:length(sites)){
  sstr <- paste0(sites[ss],'*3day.csv')
  filename <- list.files(path=path,pattern=glob2rx(sstr),full.names=T,recursive=T)
  dat <- read.csv(filename,skip=22)
  
  for(i in 1:(365*3)){
    temp <- which(datGcc[i,1]==as.numeric(as.Date(dat$date)))
    
    if(length(temp)==1){
      datGcc[i,(ss+1)] <- dat$gcc_90[temp]
    }
  }
  print(ss)  
}

# par(mfrow=c(5,1),oma=c(2,1,0,1),mar=c(0.2,5,1,5),mgp=c(2.5,1,0))
# for(i in 1:length(sites)){
#   if(i==1|i==2|i==4|i==8|i==9){
#     plot(as.Date(1:(365*3),origin='2016-12-31'),datGcc[,(i+1)],
#          # ylim=c((quantile(datGcc[,(i+1)],0.2,na.rm=T)-0.02),
#          #        (min(datGcc[,(i+1)],na.rm=T)+0.2)),
# 
#          axe=F,ann=F
#     )
#     axis(2,seq(0,1,0.1),cex.axis=1.5)
#     mtext('GCC',2,2.7,cex=1.3)
#     box(lty=1)
#   }else{
#     points(as.Date(1:(365*3),origin='2016-12-31'),datGcc[,(i+1)])
#   }
# }

############################
setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')

#######
# AG
png(filename='uief_2019_ts_hls.png',width=10,height=6.8,unit='in',res=300)

par(mfrow=c(4,1),oma=c(2,1,0,1),mar=c(0.2,5,1,5),mgp=c(2.5,1,0))
# for(i in 1:nrow(datPTS1)){
  plot(dates[209:312],apply(datPTS1[,209:312],2,median,na.rm=T),
       col=alpha('red',0.7),ylim=c(0,1),axe=F,ann=F,
       pch=19,cex=1.5) # Shapefile point 1
  box(lty=1)
  axis(2,seq(0,1,0.5),cex.axis=1.8)
  mtext('EVI2',2,2.7,cex=1.3)
  text(18250,0.85,'1',pos=2,cex=3)
  abline(v=18057,lty=5,lwd=1.5)
  
  points(theTable[[1]]$dates,theTable[[1]]$original_VI,
         col=alpha('blue',0.7),pch=19,cex=1.5)
  
  legend('topleft',c('Planet','HLS','PhenoCam'),pch=19,cex=2,
         col=c('red','blue','forestgreen'),bty='n',
         pt.cex=c(1.7,1.7,1.4))
  
  par(new = TRUE)
  plot(as.Date(3:359,origin='2018-12-31'),datGcc[(731+2):(1095-6),8], 
      axes=F,bty="n",xlab="",ylab="",
      ylim=c(0.32,0.47),pch=19,cex=1.2,col=alpha('forestgreen',0.7))
  axis(4,seq(0,1,0.05),cex.axis=1.8)
  mtext(expression(italic(G[CC])),4,3,cex=1.3)
  
# }
 
# par(mfrow=c(4,1),oma=c(2,2,1,2),mar=c(4,5,1,4),mgp=c(2.5,1,0))
# for(i in 1:nrow(datPTS2)){
  plot(dates[209:312],apply(datPTS2[,209:312],2,median,na.rm=T),
       col=alpha('red',0.7),ylim=c(0,1),axe=F,ann=F,
       pch=19,cex=1.5) # Shapefile point 2
  box(lty=1)
  axis(2,seq(0,1,0.5),cex.axis=1.8)
  mtext('EVI2',2,2.7,cex=1.3)
  text(18250,0.85,'2',pos=2,cex=3)
  abline(v=18057,lty=5,lwd=1.5)
  
  points(theTable[[4]]$dates,theTable[[4]]$original_VI,
         col=alpha('blue',0.7),pch=19,cex=1.5)
  
  par(new = TRUE)
  plot(as.Date(3:359,origin='2018-12-31'),datGcc[(731+2):(1095-6),7], 
       axes=F,bty="n",xlab="",ylab="",
       ylim=c(0.30,0.50),pch=19,cex=1.2,col=alpha('forestgreen',0.7))
  axis(4,seq(0,1,0.05),cex.axis=1.8)
  mtext(expression(italic(G[CC])),4,3,cex=1.3)
# }

# par(mfrow=c(4,1),oma=c(2,2,1,2),mar=c(4,5,1,4),mgp=c(2.5,1,0))
# for(i in 1:nrow(datPTS3)){
  plot(dates[209:312],apply(datPTS3[,209:312],2,median,na.rm=T),
       col=alpha('red',0.7),ylim=c(0,1),axe=F,ann=F,
       pch=19,cex=1.5) # Shapefile point 3
  box(lty=1)
  axis(2,seq(0,1,0.5),cex.axis=1.8)
  mtext('EVI2',2,2.7,cex=1.3)
  text(18250,0.85,'3',pos=2,cex=3)
  abline(v=18057,lty=5,lwd=1.5)
  
  points(theTable[[2]]$dates,theTable[[2]]$original_VI,
         col=alpha('blue',0.7),pch=19,cex=1.5)
  
  par(new = TRUE)
  plot(as.Date(3:359,origin='2018-12-31'),datGcc[(731+2):(1095-6),6], 
       axes=F,bty="n",xlab="",ylab="",
       ylim=c(0.29,0.45),pch=19,cex=1.2,col=alpha('forestgreen',0.7))
  axis(4,seq(0,1,0.05),cex.axis=1.8)
  mtext(expression(italic(G[CC])),4,3,cex=1.3)
# }

# par(mfrow=c(4,1),oma=c(2,2,1,2),mar=c(4,5,1,4),mgp=c(2.5,1,0))
# for(i in 1:nrow(datPTS4)){
  plot(dates[209:312],apply(datPTS4[,209:312],2,median,na.rm=T),
       col=alpha('red',0.7),ylim=c(0,1),axe=F,ann=F,
       pch=19,cex=1.5) # Shapefile point 4
  box(lty=1)
  axis(2,seq(0,1,0.5),cex.axis=1.8)
  axis(1,at=as.Date(c(15,74,135,196,258,319,(365+15)),origin='2018-12-31'),
       c('Jan','Mar','May','Jul','Sep','Nov','Jan'),
       cex.axis=1.8)
  mtext('EVI2',2,2.7,cex=1.3)
  text(18250,0.85,'4',pos=2,cex=3)
  abline(v=18057,lty=5,lwd=1.5)
  
  points(theTable[[3]]$dates,theTable[[3]]$original_VI,
         col=alpha('blue',0.7),pch=19,cex=1.5)
  
  par(new = TRUE)
  plot(as.Date(3:359,origin='2018-12-31'),datGcc[(731+2):(1095-6),5], 
       axes=F,bty="n",xlab="",ylab="",
       ylim=c(0.31,0.42),pch=19,cex=1.2,col=alpha('forestgreen',0.7))
  axis(4,seq(0,1,0.05),cex.axis=1.8)
  mtext(expression(italic(G[CC])),4,3,cex=1.3)

# }

dev.off()

#######
# MF

# par(mfrow=c(4,1),oma=c(2,1,0,1),mar=c(0.2,5,1,5),mgp=c(2.5,1,0))
# for(i in 1:nrow(datPTS2)){
#   plot(dates[1:155],datPTS2[i,1:155],ylim=c(0,1)) # Shapefile point 1
#   
#   par(new = TRUE)
#   plot(as.Date(99:(365*3),origin='2016-12-31'),datGcc[99:(365*3),3], 
#        axes=F,bty="n",xlab="",ylab="",
#        ylim=c(0.29,0.50),pch=19,cex=1.3,col=alpha('forestgreen',0.5))
# }
# 
# par(mfrow=c(4,1),oma=c(2,1,0,1),mar=c(0.2,5,1,5),mgp=c(2.5,1,0))
# for(i in 1:8){
#   plot(dates[1:155],datPTS3[i,1:155],ylim=c(0,1)) # Shapefile point 1
#   
#   par(new = TRUE)
#   plot(as.Date(99:(365*3),origin='2016-12-31'),datGcc[99:(365*3),4], 
#        axes=F,bty="n",xlab="",ylab="",
#        ylim=c(0.33,0.47),pch=19,cex=1.3,col=alpha('forestgreen',0.5))
# }

setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')

png(filename='hf_2019_ts_hls.png',width=10,height=6,unit='in',res=300)

par(mfrow=c(2,1),oma=c(2,1,0,1),mar=c(0.2,5,1,5),mgp=c(2.5,1,0))
  plot(dates[1:155],apply(datPTS1[,1:155],2,median,na.rm=T),
       col=alpha('red',0.7),ylim=c(0,1),axe=F,ann=F,
       pch=19,cex=1.1) # Shapefile point 2
  box(lty=1)
  axis(2,seq(0,1,0.5),cex.axis=1.3)
  mtext('EVI2',2,2.4,cex=1.3)
  text(18250,0.85,'DB',pos=2,cex=2)
  abline(v=18034,lty=5,lwd=1.5)
  
  points(theTable[[2]]$dates,theTable[[2]]$original_VI,
         col=alpha('blue',0.7),pch=19,cex=1.1)
  
  par(new = TRUE)
  plot(as.Date(99:(365*3),origin='2016-12-31'),datGcc[99:(365*3),3],
       axes=F,bty="n",xlab="",ylab="",
       ylim=c(0.29,0.50),pch=19,cex=0.9,col=alpha('forestgreen',0.5))
  axis(4,seq(0,1,0.1),cex.axis=1.3)
  mtext(expression(italic(G[CC])),4,2.5,cex=1.3)

  
  plot(dates[1:155],apply(datPTS2[,1:155],2,median,na.rm=T),
       col=alpha('red',0.7),ylim=c(0,1),axe=F,ann=F,
       pch=19,cex=1.1) # Shapefile point 2
  box(lty=1)
  axis(2,seq(0,1,0.5),cex.axis=1.3)
  mtext('EVI2',2,2.4,cex=1.3)
  text(18250,0.85,'EN',pos=2,cex=2)
  axis(1,at=as.Date(c(182,365+182,(365*2+182)),origin='2016-12-31'),
       c(2017,2018,2019),
       cex.axis=1.3)
  abline(v=18034,lty=5,lwd=1.5)
  
  points(theTable[[1]]$dates,theTable[[1]]$original_VI,
         col=alpha('blue',0.7),pch=19,cex=1.1)
 
  par(new = TRUE)
  plot(as.Date(99:(365*3),origin='2016-12-31'),datGcc[99:(365*3),4],
       axes=F,bty="n",xlab="",ylab="",
       ylim=c(0.33,0.47),pch=19,cex=0.9,col=alpha('forestgreen',0.5))
  axis(4,seq(0,1,0.05),cex.axis=1.3)
  mtext(expression(italic(G[CC])),4,2.5,cex=1.3)
  
  legend('topleft',c('Planet','HLS','PhenoCam'),pch=19,cex=1.2,
         col=c('red','blue','forestgreen'),bty='n',
         pt.cex=c(1.3,1.3,1.0))
  

dev.off()



############################
png(filename='hf_2019_ts_hls_planet.png',width=10,height=8,unit='in',res=300)

par(mfrow=c(3,1),oma=c(2,1,0,1),mar=c(0.2,5,1,5),mgp=c(2.5,1,0))
plot(dates[1:155],apply(datPTS1[,1:155],2,median,na.rm=T),
     col=alpha('red',0),ylim=c(0,1),axe=F,ann=F,
     pch=19,cex=1.5) # Shapefile point 2
box(lty=1)
axis(2,seq(0,1,0.5),cex.axis=2)
mtext('EVI2',2,2.7,cex=1.5)
text(18250,0.85,'DB',pos=2,cex=3)
legend('topleft',c('Planet','HLS'),pch=19,cex=2,
       col=c('red','blue'),bty='n',
       pt.cex=1.8)
for(i in 1:nrow(datPTS1)){
  points(dates[1:155],datPTS1[i,1:155],
         col=alpha('red',0.15),pch=19,cex=1.5)
}
points(theTable[[2]]$dates,theTable[[2]]$original_VI,
       col=alpha('blue',0.5),pch=19,cex=1.5)


plot(dates[1:155],apply(datPTS2[,1:155],2,median,na.rm=T),
     col=alpha('red',0),ylim=c(0,1),axe=F,ann=F,
     pch=19,cex=1.5) # Shapefile point 2
box(lty=1)
axis(2,seq(0,1,0.5),cex.axis=2)
mtext('EVI2',2,2.7,cex=1.5)
text(18250,0.85,'EN',pos=2,cex=3)
legend('topleft',c('Planet','HLS'),pch=19,cex=2,
       col=c('red','blue'),bty='n',
       pt.cex=1.8)
for(i in 1:nrow(datPTS2)){
  points(dates[1:155],datPTS2[i,1:155],
         col=alpha('red',0.15),pch=19,cex=1.5)
}
points(theTable[[1]]$dates,theTable[[1]]$original_VI,
       col=alpha('blue',0.5),pch=19,cex=1.5)


plot(dates[1:155],apply(datPTS2[,1:155],2,median,na.rm=T),
     col=alpha('red',0),ylim=c(0,1),axe=F,ann=F,
     pch=19,cex=1.1) # Shapefile point 2
box(lty=1)
axis(2,seq(0,1,0.5),cex.axis=2)
axis(1,at=as.Date(c(182,365+182,(365*2+182)),origin='2016-12-31'),
     c(2017,2018,2019),
     cex.axis=2)
mtext('EVI2',2,2.7,cex=1.5)
legend('topleft',c('HLS-DB','HLS-EN'),pch=c(1,4),cex=2,
       bty='n',pt.cex=2)
points(theTable[[2]]$dates,theTable[[2]]$original_VI,
       pch=1,cex=2)
points(theTable[[1]]$dates,theTable[[1]]$original_VI,
       pch=4,cex=2)



dev.off()




#######
setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')

# GR & SH
png(filename='cperuvb_2019_ts_hls.png',width=10,height=6,unit='in',res=300)

par(mfrow=c(2,1),oma=c(2,1,0,1),mar=c(0.2,5,1,5),mgp=c(2.5,1,0))

plot(dates[1:365],apply(datPTS4[,1:365],2,median,na.rm=T),
     col=alpha('red',0.7),ylim=c(0,0.4),axe=F,ann=F,
     pch=19,cex=1.1) # Shapefile point 1
box(lty=1)
axis(2,seq(0,1,0.2),cex.axis=1.3)
mtext('EVI2',2,2.4,cex=1.3)

legend('topleft',c('Planet','HLS','PhenoCam'),pch=19,cex=1.2,
       col=c('red','blue','forestgreen'),bty='n',
       pt.cex=c(1.3,1.3,1.0))

points(theTable[[4]]$dates,theTable[[4]]$original_VI,
       col=alpha('blue',0.7),pch=19,cex=1.1)

par(new = TRUE)
plot(as.Date(23:(365*3-13),origin='2016-12-31'),datGcc[23:(365*3-13),10],
     axes=F,bty="n",xlab="",ylab="",
     ylim=c(0.31,0.41),pch=19,cex=0.9,col=alpha('forestgreen',0.5))
axis(4,seq(0,1,0.03),cex.axis=1.3)
mtext(expression(italic(G[CC])),4,2.4,cex=1.3)

dev.off()
#####
setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')

# GR & SH
png(filename='burnssagebrush_2019_ts_hls.png',width=10,height=6,unit='in',res=300)

par(mfrow=c(2,1),oma=c(2,1,0,1),mar=c(0.2,5,1,5),mgp=c(2.5,1,0))

plot(0,0,axe=F,ann=F,cex=0)
plot(dates[1:341],apply(datPTS4[,1:341],2,median,na.rm=T),
     col=alpha('red',0.7),ylim=c(0,0.3),axe=F,ann=F,
     pch=19,cex=1.1) # Shapefile point 1
box(lty=1)
axis(2,seq(0,1,0.1),cex.axis=1.3)
mtext('EVI2',2,2.4,cex=1.3)

legend('topleft',c('Planet','HLS','PhenoCam'),pch=19,cex=1.2,
       col=c('red','blue','forestgreen'),bty='n',
       pt.cex=c(1.3,1.3,1.0))

points(theTable[[4]]$dates,theTable[[4]]$original_VI,
       col=alpha('blue',0.7),pch=19,cex=1.1)

par(new = TRUE)
plot(as.Date(82:(365*3-8),origin='2016-12-31'),datGcc[82:(365*3-8),9],
     axes=F,bty="n",xlab="",ylab="",
     ylim=c(0.332,0.384),pch=19,cex=0.9,col=alpha('forestgreen',0.5))
axis(4,seq(0,1,0.02),cex.axis=1.3)
mtext(expression(italic(G[CC])),4,2.4,cex=1.3)
axis(1,at=as.Date(c(182,365+182,(365*2+182)),origin='2016-12-31'),
     c(2017,2018,2019),
     cex.axis=1.5)

dev.off()
