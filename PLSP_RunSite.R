library(raster)
library(rgdal)
library(gdalUtils)
library(ncdf4)
library(signal)
library(RColorBrewer)
library(doMC)

args <- commandArgs()
print(args)

ss <- as.numeric(substr(args[3],1,1))
cc <- as.numeric(substr(args[3],2,3))


###############################
##
source('/usr3/graduate/mkmoon/GitHub/PlanetLSP/PLSP_Functions.R')


vgt <- c('DB','MF','EN','AG','GR','SH','CR')


# ###############################
# lDates    <- vector('list',length(vgt))
# limgBase  <- vector('list',length(vgt))
# leviStack <- vector('list',length(vgt))
# lredStack <- vector('list',length(vgt))
# lnirStack <- vector('list',length(vgt))

registerDoMC()

# for(i in 1:length(vgt)){
  imgDir <- paste0('/projectnb/modislc/users/mkmoon/Planet/data/Planet_SR/',vgt[i],'/files')

  eviStack <- saveEVI2stack(imgDir)

  dates <- eviStack$dates
  imgBase <- eviStack$imgBase
  eviStack <- eviStack$eviStack
# 
#   lDates[[i]]    <- dates
#   limgBase[[i]]  <- imgBase
#   leviStack[[i]] <- eviStack
# 
#   print(i)
# }
# 
# setwd('/projectnb/modislc/users/mkmoon/Planet/data/')
# save(lDates,
#      limgBase,
#      leviStack,
#      file='planet_eviStack.rda')
load('/projectnb/modislc/users/mkmoon/Planet/data/planet_eviStack.rda')


# for(i in 1:length(vgt)){
#   imgDir <- paste0('/projectnb/modislc/users/mkmoon/Planet/data/Planet_SR/',vgt[i],'/files')
# 
#   nirStack <- saveNIRstack(imgDir)
#   
#   dates <- nirStack$dates
#   imgBase <- nirStack$imgBase
#   nirStack <- nirStack$nirStack
# 
#   lDates[[i]]    <- dates
#   limgBase[[i]]  <- imgBase
#   lnirStack[[i]] <- nirStack
# 
#   print(i)
# }
# 
# setwd('/projectnb/modislc/users/mkmoon/Planet/data/')
# save(lDates,
#      limgBase,
#      lnirStack,
#      file='planet_nirStack.rda')
# 
# 
# for(i in 1:length(vgt)){
#   imgDir <- paste0('/projectnb/modislc/users/mkmoon/Planet/data/Planet_SR/',vgt[i],'/files')
#   
#   redStack <- saveREDstack(imgDir)
#   
#   dates <- redStack$dates
#   imgBase <- redStack$imgBase
#   redStack <- redStack$redStack
#   
#   lDates[[i]]    <- dates
#   limgBase[[i]]  <- imgBase
#   lredStack[[i]] <- redStack
#   
#   print(i)
# }
# save(lDates,
#      limgBase,
#      lredStack,
#      file='planet_redStack.rda')
# 
# 

# ###############################
# # Time series
# shpPoints <- readOGR('/projectnb/modislc/users/mkmoon/Planet/shp/18TYN_pts_1.shp')
# 
# datPTS1 <- extractTS(shpPoints,leviStack[[2]],limgBase[[2]],id=1,rad=5)
# datPTS2 <- extractTS(shpPoints,leviStack[[2]],limgBase[[2]],id=2,rad=5)
# datPTS3 <- extractTS(shpPoints,leviStack[[2]],limgBase[[2]],id=3,rad=5)
# datPTS4 <- extractTS(shpPoints,leviStack[[2]],limgBase[[2]],id=4,rad=5)
# datPTS5 <- extractTS(shpPoints,leviStack[[2]],limgBase[[2]],id=5,rad=5)
# datPTS6 <- extractTS(shpPoints,leviStack[[2]],limgBase[[2]],id=6,rad=5)
# 
# setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
# png(filename='hf_2019_enPoints.png',width=10,height=5,unit='in',res=300)
# 
# par(mfrow=c(1,1),oma=c(2,1,0,1),mar=c(0.2,5,1,5),mgp=c(2.5,1,0))
# plot(lDates[[2]][1:155],apply(datPTS1[,1:155],2,median,na.rm=T),
#      ylim=c(0,0.8),col='red',axe=F,ann=F,pch=1,cex=1.1)
# box(lty=1)
# axis(2,seq(0,1,0.2),cex.axis=1.3)
# mtext('EVI2',2,2.4,cex=1.3)
# 
# points(lDates[[2]][1:155],apply(datPTS2[,1:155],2,median,na.rm=T),pch=2,ylim=c(0,1),col='blue')
# points(lDates[[2]][1:155],apply(datPTS4[,1:155],2,median,na.rm=T),pch=3,ylim=c(0,1))
# points(lDates[[2]][1:155],apply(datPTS5[,1:155],2,median,na.rm=T),pch=4,ylim=c(0,1))
# points(lDates[[2]][1:155],apply(datPTS6[,1:155],2,median,na.rm=T),pch=5,ylim=c(0,1))
# axis(1,at=as.Date(c(99,366,(365*2+1),(365*3+1)),origin='2016-12-31'),
#      c('Apr 2017','Jan 2018','Jan 2019','Jan 2020'),
#      cex.axis=1.3)
# 
# dev.off()
# 
# # NIR and red
# load('/projectnb/modislc/users/mkmoon/Planet/data/planet_nirStack.rda')
# load('/projectnb/modislc/users/mkmoon/Planet/data/planet_redStack.rda')
# 
# # Time series
# shpPoints <- readOGR('/projectnb/modislc/users/mkmoon/Planet/shp/18TYN_pts_1.shp')
# 
# datPTS1n <- extractTS(shpPoints,lnirStack[[2]],limgBase[[2]],id=1,rad=5)
# datPTS2n <- extractTS(shpPoints,lnirStack[[2]],limgBase[[2]],id=2,rad=5)
# datPTS3n <- extractTS(shpPoints,lnirStack[[2]],limgBase[[2]],id=3,rad=5)
# datPTS4n <- extractTS(shpPoints,lnirStack[[2]],limgBase[[2]],id=4,rad=5)
# datPTS5n <- extractTS(shpPoints,lnirStack[[2]],limgBase[[2]],id=5,rad=5)
# datPTS6n <- extractTS(shpPoints,lnirStack[[2]],limgBase[[2]],id=6,rad=5)
# 
# datPTS1r <- extractTS(shpPoints,lredStack[[2]],limgBase[[2]],id=1,rad=5)
# datPTS2r <- extractTS(shpPoints,lredStack[[2]],limgBase[[2]],id=2,rad=5)
# datPTS3r <- extractTS(shpPoints,lredStack[[2]],limgBase[[2]],id=3,rad=5)
# datPTS4r <- extractTS(shpPoints,lredStack[[2]],limgBase[[2]],id=4,rad=5)
# datPTS5r <- extractTS(shpPoints,lredStack[[2]],limgBase[[2]],id=5,rad=5)
# datPTS6r <- extractTS(shpPoints,lredStack[[2]],limgBase[[2]],id=6,rad=5)
# 
# setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
# png(filename='hf_2019_enPoints_bands.png',width=13,height=7,unit='in',res=300)
# 
# par(mfrow=c(2,1),oma=c(2,1,0,1),mar=c(0.2,5,1,2),mgp=c(2.5,1,0))
# plot(lDates[[2]][1:155],apply(datPTS1n[,1:155],2,median,na.rm=T),
#      ylim=c(1000,5300),col='red',axe=F,ann=F,pch=1,cex=1.1)
# box(lty=1)
# axis(2,seq(0,10000,1000),cex.axis=1.3)
# mtext('NIR (x 0.0001)',2,2.4,cex=1.3)
# 
# points(lDates[[2]][1:155],apply(datPTS2n[,1:155],2,median,na.rm=T),pch=2,ylim=c(0,1),col='blue')
# points(lDates[[2]][1:155],apply(datPTS4n[,1:155],2,median,na.rm=T),pch=3,ylim=c(0,1))
# points(lDates[[2]][1:155],apply(datPTS5n[,1:155],2,median,na.rm=T),pch=4,ylim=c(0,1))
# points(lDates[[2]][1:155],apply(datPTS6n[,1:155],2,median,na.rm=T),pch=5,ylim=c(0,1))
# 
# plot(lDates[[2]][1:155],apply(datPTS1r[,1:155],2,median,na.rm=T),
#      ylim=c(0,2000),col='red',axe=F,ann=F,pch=1,cex=1.1)
# box(lty=1)
# axis(2,seq(0,10000,500),cex.axis=1.3)
# mtext('Red (x 0.0001)',2,2.4,cex=1.3)
# 
# points(lDates[[2]][1:155],apply(datPTS2r[,1:155],2,median,na.rm=T),pch=2,ylim=c(0,1),col='blue')
# points(lDates[[2]][1:155],apply(datPTS4r[,1:155],2,median,na.rm=T),pch=3,ylim=c(0,1))
# points(lDates[[2]][1:155],apply(datPTS5r[,1:155],2,median,na.rm=T),pch=4,ylim=c(0,1))
# points(lDates[[2]][1:155],apply(datPTS6r[,1:155],2,median,na.rm=T),pch=5,ylim=c(0,1))
# axis(1,at=as.Date(c(99,366,(365*2+1),(365*3+1)),origin='2016-12-31'),
#      c('Apr 2017','Jan 2018','Jan 2019','Jan 2020'),
#      cex.axis=1.3)
# 
# dev.off()


# # AG site 2 additional points
# shpPoints <- readOGR('/projectnb/modislc/users/mkmoon/Planet/shp/16TCK_pts_1.shp')
# datPTS1 <- extractTS(shpPoints,leviStack[[4]],limgBase[[4]],id=2,rad=5)
# 
# shpPoints <- readOGR('/projectnb/modislc/users/mkmoon/Planet/shp/16TCK_pts_2.shp')
# datPTS2 <- extractTS(shpPoints,leviStack[[4]],limgBase[[4]],id=1,rad=5)
# datPTS3 <- extractTS(shpPoints,leviStack[[4]],limgBase[[4]],id=2,rad=5)
# datPTS4 <- extractTS(shpPoints,leviStack[[4]],limgBase[[4]],id=4,rad=5)
# datPTS5 <- extractTS(shpPoints,leviStack[[4]],limgBase[[4]],id=5,rad=5)
# 
# setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
# png(filename='ag_2019_exPoints.png',width=9.5,height=5,unit='in',res=300)
# 
# par(mfrow=c(1,1),oma=c(2,1,0,1),mar=c(0.2,5,1,5),mgp=c(2.5,1,0))
# 
# plot(lDates[[4]][209:312],apply(datPTS1[,209:312],2,median,na.rm=T),
#      col=alpha('red',0.7),ylim=c(0,1),axe=F,ann=F,
#      pch=1,cex=1.1) # Shapefile point 2
# box(lty=1)
# axis(2,seq(0,1,0.5),cex.axis=1.3)
# mtext('EVI2',2,2.7,cex=1.3)
# 
# points(lDates[[4]][209:312],apply(datPTS2[,209:312],2,median,na.rm=T),pch=2)
# points(lDates[[4]][209:312],apply(datPTS3[,209:312],2,median,na.rm=T),pch=3)
# points(lDates[[4]][209:312],apply(datPTS4[,209:312],2,median,na.rm=T),pch=4)
# points(lDates[[4]][209:312],apply(datPTS5[,209:312],2,median,na.rm=T),pch=4)
# 
# par(new = TRUE)
# plot(as.Date(3:359,origin='2018-12-31'),datGcc[(731+2):(1095-6),7], 
#      axes=F,bty="n",xlab="",ylab="",
#      ylim=c(0.30,0.50),pch=19,cex=1.2,col=alpha('forestgreen',0.7))
# axis(4,seq(0,1,0.05),cex.axis=1.3)
# mtext(expression(italic(G[CC])),4,3,cex=1.3)
# axis(1,at=as.Date(c(15,74,135,196,258,319,(365+15)),origin='2018-12-31'),
#      c('Jan','Mar','May','Jul','Sep','Nov','Jan'),
#      cex.axis=1.3)
# 
# dev.off()
# 
# 
# # for(i in 1:nrow(datPTS1)){
# #   if(i==1){
# #     plot(lDates[[1]],datPTS1[i,],col=i)
# #   }else if(i > 1 & i < 21){
# #     points(lDates[[1]],datPTS1[i,],col=i)
# #   }else{
# #     points(lDates[[1]],datPTS1[i,],col=i)
# #   }
# # }
# 
# 
# # ###############################
# # # EVI amplitude
# # eviAmp <- function(eviStack,imgBase){
# #   dat <- matrix(NA,length(imgBase),1)
# #   
# #   temp1 <- NULL
# #   
# #   amp <- foreach(j=1:length(imgBase),.combine=rbind) %dopar% {
# #     
# #     temp <- NULL
# #     for(i in 1:length(eviStack)){
# #       temp[i] <- eviStack[[i]][j]
# #     }    
# #     
# #     quantile(temp,0.9,na.rm=T) - quantile(temp,0.1,na.rm=T)
# #   }
# #   
# #   rast <- setValues(imgBase,amp)
# #   
# #   return(rast)
# # }
# # 
# # setwd('/projectnb/modislc/users/mkmoon/Planet/data/Images/')
# # writeRaster(rast,filename='eviamp_2019_hf.tif',format="GTiff",overwrite=T)
# # 
# # ###############################
eviStack  <- leviStack[[ss]]
datesFull <- lDates[[ss]]
imgBase   <- limgBase[[ss]]

chunk <- length(imgBase)%/%20

if(cc==20){
  chunks <- c((chunk*(cc-1)+1):length(eviStack[[1]]))  
}else{
  chunks <- c((chunk*(cc-1)+1):(chunk*cc))
}

print(length(chunks))
  
pheme <- foreach(j=chunks,.combine='rbind') %dopar% {
    
  eviPoint <- matrix(NA,length(eviStack),1)
  for(i in 1:length(eviStack)){
    eviPoint[i,1] <- eviStack[[i]][j]
  }
  eviFull   <- eviPoint
  eviPointT <- eviFull[substr(datesFull,1,4)=='2017']
  dates     <- datesFull[substr(datesFull,1,4)=='2017']
  
  eviPoint <- eviPointT[!is.na(eviPointT)]
  dates    <- dates[!is.na(eviPointT)]
  gws      <- dates[as.numeric(substr(dates,6,7))>3&as.numeric(substr(dates,6,7))<10]
  
  if(sum(!is.na(eviPoint))>30 & max(diff(gws))<30){
    bgevi <- quantile(eviFull[which(as.numeric(substr(as.character(datesFull),6,7))<3|
                                       as.numeric(substr(as.character(datesFull),6,7))>10)],0.9,na.rm=T)
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
    
    # # de-spike
    # deyyd <- yyd[as.numeric(strftime(dates, format = "%j"))]
    # despi <- which((deyyd-eviPoint)>0.1)
    # 
    # # re-fit
    # vitime[as.numeric(strftime(dates[despi], format = "%j"))] <- NA
    # 
    # aa <- approx(xd,vitime,n=365)[[2]]
    # yd <- sgolayfilt(aa)
    # dat <- data.frame(xd,yd)
    # spl <- smooth.spline(yd,spar=0.4)
    # yyd <- predict(spl)[[2]]
    
    #
    valPeak <- max(yyd)
    eviArea <- sum(yyd)
    
    dpeak <- which(yyd==max(yyd))
    yyd1  <- yyd[1:dpeak]
    yyd2  <- yyd[dpeak:365]
    
    phes15 <- which(yyd1>(min(yyd1)+((max(yyd1)-min(yyd1))*0.15)))[1] 
    phes50 <- which(yyd1>(min(yyd1)+((max(yyd1)-min(yyd1))*0.50)))[1] 
    phes90 <- which(yyd1>(min(yyd1)+((max(yyd1)-min(yyd1))*0.90)))[1] 
    
    phee15 <- 366- which(rev(yyd2)>(min(yyd2)+((max(yyd2)-min(yyd2))*0.15)))[1] 
    phee50 <- 366- which(rev(yyd2)>(min(yyd2)+((max(yyd2)-min(yyd2))*0.50)))[1] 
    phee90 <- 366- which(rev(yyd2)>(min(yyd2)+((max(yyd2)-min(yyd2))*0.90)))[1]  
    
    eviamp <- max(yyd)-min(yyd)
    
    pheme <- c(phes15,phes50,phes90,dpeak,phee90,phee50,phee15,eviamp,valPeak,eviArea)
    
  }else{
    pheme <- rep(NA,10)
  }
}

setwd(paste0('/projectnb/modislc/users/mkmoon/Planet/phe/2017/',vgt[ss]))
cc <- sprintf('%02d',cc)
save(pheme,file=paste0('phePlanet_',vgt[ss],'_',cc,'_1.rda'))


################################
rastPP <- vector('list',10)
rastHH <- vector('list',10)
valPP <- vector('list',7)
valHH <- vector('list',7)
ValPP <- vector('list',7)
ValHH <- vector('list',7)
statPhe <- matrix(NA,6,7*3)
StatPhe <- matrix(NA,6,7*3)
datQtil <- vector('list',6)

datQtils <- vector('list',6)
datQtild <- vector('list',6)

datEVImax <- vector('list',6)
datEVIare <- vector('list',6)

Year <- 2019
for(ss in 1:6){
  vgt <- c('DB','MF','EN','AG','GR','SH')
  path <- paste0('/projectnb/modislc/users/mkmoon/Planet/phe/',Year,'/',vgt[ss])
  filePhe <- list.files(path,pattern=glob2rx('phe*_1.rda'),full.names=T)

  for(cc in 1:length(filePhe)){
    load(filePhe[cc])
    if(cc==1){
      pheMat <- pheme
    }else{
      pheMat <- rbind(pheMat,pheme)
    }
  }

  for(rr in 1:10){
    rastPP[[rr]] <- setValues(limgBase[[ss]],pheMat[,rr])
  }

  # # par(mfrow=c(1,3))
  # # plot(rastPP[[2]]);plot(rastPP[[6]]);plot(rastPP[[8]])
  # # hist(rastPP[[2]]);hist(rastPP[[6]]);hist(rastPP[[8]])
  # 
  # # setwd(paste0('/projectnb/modislc/users/mkmoon/Planet/phe/',Year,'/',vgt[ss]))
  # # writeRaster(rastPP[[2]],filename=paste0('phe_Planet_',Year,'_sos_',vgt[ss],'.tif'),format="GTiff",overwrite=T)
  # # writeRaster(rastPP[[6]],filename=paste0('phe_Planet_',Year,'_eos_',vgt[ss],'.tif'),format="GTiff",overwrite=T)
  # # writeRaster(rastPP[[8]],filename=paste0('phe_Planet_',Year,'_amp_',vgt[ss],'.tif'),format="GTiff",overwrite=T)
  # 
  # setwd(paste0('/projectnb/modislc/users/mkmoon/Planet/phe/',Year,'/',vgt[ss]))
  # for(ii in 1:7){
  #   writeRaster(rastPP[[ii]],filename=paste0('phe_Planet_',Year,'_',vgt[ss],'_',ii,'.tif'),format="GTiff",overwrite=T)  
  # }
  

  ################################
  tiles <- c('17SQD','18TYN','19TEL','16TCK','13TEF','10TGP')
  tile <- tiles[ss]
  fileHLS <- paste0('/projectnb/modislc/projects/landsat_sentinel/MSLSP_HLS30/',tile,'/phenoMetrics/MSLSP_',tile,'_',Year,'.nc')

  nc <- nc_open(fileHLS)
  var <- names(nc[['var']])

  for(rr in 1:10){
    if(rr==9){
      pheHLS <- raster(fileHLS,varname=var[10])
      rastHH[[rr]] <- projectRaster(pheHLS,rastPP[[1]],method='ngb')
    }else if(rr==10){
      pheHLS <- raster(fileHLS,varname=var[12])
      rastHH[[rr]] <- projectRaster(pheHLS,rastPP[[1]],method='ngb')
    }else{
      pheHLS <- raster(fileHLS,varname=var[(rr+2)])
      rastHH[[rr]] <- projectRaster(pheHLS,rastPP[[1]],method='ngb')
    }
  }

  # # EVImax & EVIarea
  # set.seed(456123)
  # sam <- sample(1:length(rastPP[[9]]),100000)
  # x11  <- values(rastPP[[9]])[sam]*10000
  # y11  <- values(rastHH[[9]])[sam]
  # x21  <- values(rastPP[[10]])[sam]*100
  # y21  <- values(rastHH[[10]])[sam]
  #
  # datEVImax[[ss]] <- cbind(x11,y11)
  # datEVIare[[ss]] <- cbind(x21,y21)

  # Quantile
  datHHPP <- matrix(NA,70000,12)
  bined   <- c(0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95)
  k <- 1
  for(rr in 1:7){
    matHH <- as.matrix(rastHH[[rr]])
    matPP <- as.matrix(rastPP[[rr]])
    for(i in 1:100){
      for(j in 1:100){
        temphh <- matHH[(10*(i-1)+1):(10*i),(10*(j-1)+1):(10*j)]
        temppp <- matPP[(10*(i-1)+1):(10*i),(10*(j-1)+1):(10*j)]

        datHHPP[k,1] <- median(temphh,na.rm=T)
        for(jj in 2:12){
          datHHPP[k,jj] <- quantile(temppp,bined[jj-1],na.rm=T)
        }
        k <- k + 1
      }
    }
  }
  datQtil[[ss]] <- datHHPP
  #


  # set.seed(456456)
  # sam <- sample(1:length(rastPP[[1]]),100000)
  # valpp <- vector('list',7)
  # valhh <- vector('list',7)
  # for(i in 1:7){
  #   valpp[[i]] <- values(rastPP[[i]])[sam]
  #   valhh[[i]] <- values(rastHH[[i]])[sam]
  # 
  #   x1 <- values(rastPP[[i]])[sam]
  #   y1 <- values(rastHH[[i]])[sam]
  # 
  #   statPhe[ss,(3*(i-1)+1)] <- round(cor(x1,y1,use='na.or.complete'),3)
  #   statPhe[ss,(3*(i-1)+2)] <- round(sqrt(mean((x1-y1)^2,na.rm=T)),2)
  #   statPhe[ss,(3*(i-1)+3)] <- round(mean(x1-y1,na.rm=T),2)
  # }
  # valPP[[ss]] <- unlist(valpp)
  # valHH[[ss]] <- unlist(valhh)

  # #
  # phePn30 <- projectRaster(rastSOS,pheHLS)
  # htPn  <- hist(rastSOS,breaks=seq(0,400,1),plot=F)
  # htHLS <- hist(pheHLS,breaks=seq(0,400,1),plot=F)
  # htPn$counts <- htPn$counts/max(htPn$counts)
  # htHLS$counts <- htHLS$counts/max(htHLS$counts)
  #
  # par(mfrow=c(1,2))
  # plot(values(pheHLS),values(phePn30),xlim=c(60,190),ylim=c(60,190))
  # abline(0,1)
  # summary(lm(values(phePn30)~values(pheHLS)))
  # abline(lm(values(phePn30)~values(pheHLS)))
  #
  # plot(htPn,xlim=c(60,180),col=rgb(1,0,0,0.6))
  # plot(htHLS,col=rgb(0,0,1,0.6),add=T)

  # vgt <- c('db','mf','en','ag','gr','sh')
  #
  # shpPoints <- shapefile(paste0('/projectnb/modislc/users/mkmoon/Planet/shp/rp_',vgt[ss],'_1000.shp'))
  #
  # shpPoints <- spTransform(shpPoints,crs(rastSOS))
  # datPP <- extract(rastSOS,shpPoints,buffer=45)
  # shpPoints <- spTransform(shpPoints,crs(pheHLS))
  # datHH <- extract(pheHLS,shpPoints,buffer=45,fun=median)
  #
  # datPPqunt <- matrix(NA,length(datHH),11)
  # for(i in 1:length(datHH)){
  #   temp <- datPP[[i]]
  #
  #   datPPqunt[i,1] <- quantile(temp,0.05,na.rm=T)
  #   datPPqunt[i,2] <- quantile(temp,0.1,na.rm=T)
  #   datPPqunt[i,3] <- quantile(temp,0.2,na.rm=T)
  #   datPPqunt[i,4] <- quantile(temp,0.3,na.rm=T)
  #   datPPqunt[i,5] <- quantile(temp,0.4,na.rm=T)
  #   datPPqunt[i,6] <- quantile(temp,0.5,na.rm=T)
  #   datPPqunt[i,7] <- quantile(temp,0.6,na.rm=T)
  #   datPPqunt[i,8] <- quantile(temp,0.7,na.rm=T)
  #   datPPqunt[i,9] <- quantile(temp,0.8,na.rm=T)
  #   datPPqunt[i,10] <- quantile(temp,0.9,na.rm=T)
  #   datPPqunt[i,11] <- quantile(temp,0.95,na.rm=T)
  # }
  #
  # if(ss==1){
  #   statPhedb <- matrix(NA,11,5)
  #   for(i in 1:11){
  #     statPhedb[i,1] <- cor(datPPqunt[,i],datHH,use='na.or.complete')
  #     statPhedb[i,2] <- sqrt(mean((datPPqunt[,i]-datHH)^2,na.rm=T))
  #     statPhedb[i,3] <- mean(datPPqunt[,i]-datHH,na.rm=T)
  #   }
  # }else if(ss==2){
  #   statPhemf <- matrix(NA,11,5)
  #   for(i in 1:11){
  #     statPhemf[i,1] <- cor(datPPqunt[,i],datHH,use='na.or.complete')
  #     statPhemf[i,2] <- sqrt(mean((datPPqunt[,i]-datHH)^2,na.rm=T))
  #     statPhemf[i,3] <- mean(datPPqunt[,i]-datHH,na.rm=T)
  #   }
  # }else if(ss==3){
  #   statPheen <- matrix(NA,11,5)
  #   for(i in 1:11){
  #     statPheen[i,1] <- cor(datPPqunt[,i],datHH,use='na.or.complete')
  #     statPheen[i,2] <- sqrt(mean((datPPqunt[,i]-datHH)^2,na.rm=T))
  #     statPheen[i,3] <- mean(datPPqunt[,i]-datHH,na.rm=T)
  #   }
  # }else if(ss==4){
  #   statPheag <- matrix(NA,11,5)
  #   for(i in 1:11){
  #     statPheag[i,1] <- cor(datPPqunt[,i],datHH,use='na.or.complete')
  #     statPheag[i,2] <- sqrt(mean((datPPqunt[,i]-datHH)^2,na.rm=T))
  #     statPheag[i,3] <- mean(datPPqunt[,i]-datHH,na.rm=T)
  #   }
  # }else if(ss==5){
  #   statPhegr <- matrix(NA,11,5)
  #   for(i in 1:11){
  #     statPhegr[i,1] <- cor(datPPqunt[,i],datHH,use='na.or.complete')
  #     statPhegr[i,2] <- sqrt(mean((datPPqunt[,i]-datHH)^2,na.rm=T))
  #     statPhegr[i,3] <- mean(datPPqunt[,i]-datHH,na.rm=T)
  #   }
  # }else{
  #   statPhesh <- matrix(NA,11,5)
  #   for(i in 1:11){
  #     statPhesh[i,1] <- cor(datPPqunt[,i],datHH,use='na.or.complete')
  #     statPhesh[i,2] <- sqrt(mean((datPPqunt[,i]-datHH)^2,na.rm=T))
  #     statPhesh[i,3] <- mean(datPPqunt[,i]-datHH,na.rm=T)
  #   }
  # }


  # #######################
  # # Average based on HLS pixel
  # datHlsPix <- vector('list',7)
  # for(vv in 1:7){
  #   datHLSpix <- matrix(NA,(33*33),2)
  #   matHH <- as.matrix(rastHH[[vv]])
  #   matPP <- as.matrix(rastPP[[vv]])
  #   k <- 1
  #   for(i in 1:33){
  #     for(j in 1:33){
  #       datHLSpix[k,1] <- mean(matHH[((30*(i-1)+1):(30*i)),((30*(j-1)+1):(30*j))],na.rm=T)
  #       datHLSpix[k,2] <- mean(matPP[((30*(i-1)+1):(30*i)),((30*(j-1)+1):(30*j))],na.rm=T)
  #       k <- k+1
  #     }
  #   }
  #   datHlsPix[[vv]] <- datHLSpix
  #   # print(vv)
  # }
  # for(i in 1:7){
  #   if(i==1){
  #     x0 <- datHlsPix[[i]][,2]
  #     y0 <- datHlsPix[[i]][,1]
  #   }else{
  #     x0 <- c(x0,datHlsPix[[i]][,2])
  #     y0 <- c(y0,datHlsPix[[i]][,1])
  #   }
  #   x1 <- datHlsPix[[i]][,2]
  #   y1 <- datHlsPix[[i]][,1]
  #   StatPhe[ss,(3*(i-1)+1)] <- round(cor(x1,y1,use='na.or.complete'),3)
  #   StatPhe[ss,(3*(i-1)+2)] <- round(sqrt(mean((x1-y1)^2,na.rm=T)),2)
  #   StatPhe[ss,(3*(i-1)+3)] <- round(mean(x1-y1,na.rm=T),2)
  # }
  # ValPP[[ss]] <- x0
  # ValHH[[ss]] <- y0

 print(ss)
}

# setwd('/projectnb/modislc/users/mkmoon/Planet/data/')
# write.csv(statPhe,file='phe_1to1.csv')
# write.csv(StatPhe,file='phe_1to1_at30.csv')

# plot
setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
png(filename='1to1_phe_sig.png',width=12,height=8,unit='in',res=300)

par(mfrow=c(2,3),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0))
spec <- rev(brewer.pal(11,'Spectral'))
mycolRamp = colorRampPalette(c('White',spec))
for(i in 1:6){
  sam <- sample(1:700000,100000)
  x1 <- valPP[[i]][sam]
  y1 <- valHH[[i]][sam]

  limx <- c(0,400)
  limy <- c(0,400)

  smoothScatter(x1,y1,xlim=limx,ylim=limy,
                colramp=mycolRamp,nbin=600,nrpoints=0,
                transformation=function(x)x^.6,axe=F,
                xlab='Planet phenometrics',ylab='HLS phenometrics',
                cex.lab=1.7)
  axis(1,at=seq(0,400,100),cex.axis=1.7)
  axis(2,at=seq(0,400,100),cex.axis=1.7)
  abline(0,1,lty=5)

  reg <- formatC(round(cor(x1,y1,use='na.or.complete'),3), digits=3,format="fg", flag="#")
  rmse <- formatC(round(sqrt(mean((x1-y1)^2,na.rm=T)),1),  digits=3,format="fg", flag="#")
  bias <- formatC(round(mean(x1-y1,na.rm=T),1),  digits=3,format="fg")

  text(220,100,expression(paste(italic(r),' =',sep='')),cex=1.8,pos=4)
  text(260,100,reg,cex=1.8,pos=4)
  text(220,70,paste('RMSE = ',rmse,sep=''),cex=1.8,pos=4)
  text(220,40,paste('Bias = ',bias,sep=''),cex=1.8,pos=4)
  text(220,10,'n = 100000',cex=1.8,pos=4)
}

dev.off()

# plot at 30 m HLS
setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
png(filename='1to1_phe_at30_ds_sig.png',width=12,height=8,unit='in',res=300)

par(mfrow=c(2,3),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0))
spec <- rev(brewer.pal(11,'Spectral'))
mycolRamp = colorRampPalette(c('White',spec))
for(i in 1:6){
  x1 <- ValPP[[i]]
  y1 <- ValHH[[i]]

  limx <- c(0,400)
  limy <- c(0,400)

  smoothScatter(x1,y1,xlim=limx,ylim=limy,
                colramp=mycolRamp,nbin=600,nrpoints=0,
                transformation=function(x)x^.75,axe=F,
                xlab='Planet phenometrics',ylab='HLS phenometrics',
                cex.lab=1.7)
  # plot(x1,y1,xlim=limx,ylim=limy,axe=F,
  #      xlab='Planet phenometrics',ylab='HLS phenometrics',
  #      cex.lab=1.7,pch=19,cex=0.5,col=rgb(0,0,0,0.3))
  axis(1,at=seq(0,400,100),cex.axis=1.7)
  axis(2,at=seq(0,400,100),cex.axis=1.7)
  abline(0,1,lty=5)
  box()

  reg <- formatC(round(cor(x1,y1,use='na.or.complete'),3), digits=3,format="fg", flag="#")
  rmse <- formatC(round(sqrt(mean((x1-y1)^2,na.rm=T)),1),  digits=3,format="fg", flag="#")
  bias <- formatC(round(mean(x1-y1,na.rm=T),1),  digits=3,format="fg")

  text(220,100,expression(paste(italic(r),' =',sep='')),cex=1.8,pos=4)
  text(260,100,reg,cex=1.8,pos=4)
  text(220,70,paste('RMSE = ',rmse,sep=''),cex=1.8,pos=4)
  text(220,40,paste('Bias = ',bias,sep=''),cex=1.8,pos=4)
  text(220,10,paste('n = ',sum(!is.na(x1)&!is.na(y1)),sep=''),cex=1.8,pos=4)
}

dev.off()



# Plots for EVImax & EVIarea
setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
png(filename='1to1_evimax.png',width=12,height=8,unit='in',res=300)

par(mfrow=c(2,3),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0))
spec <- rev(brewer.pal(11,'Spectral'))
mycolRamp = colorRampPalette(c('White',spec))
for(i in 1:6){
  x1 <- datEVImax[[i]][,1]
  y1 <- datEVImax[[i]][,2]

  limx <- c(0,10000)
  limy <- c(0,10000)

  smoothScatter(x1,y1,xlim=limx,ylim=limy,
                colramp=mycolRamp,nbin=600,nrpoints=0,
                transformation=function(x)x^.6,axe=F,
                xlab='Planet Peak EVI2 (x 0.0001)',ylab='HLS Peak EVI2 (x 0.0001)',
                cex.lab=1.7)
  axis(1,at=seq(0,10000,2500),cex.axis=1.7)
  axis(2,at=seq(0,10000,2500),cex.axis=1.7)
  abline(0,1,lty=5)

  reg <- round(cor(x1,y1,use='na.or.complete'),3)
  rmse <- round(sqrt(mean((x1-y1)^2,na.rm=T)))
  bias <- round(mean(x1-y1,na.rm=T))

  text(5000,2500,expression(paste(italic(r),' =',sep='')),cex=1.8,pos=4)
  text(6000,2500,reg,cex=1.8,pos=4)
  text(5000,1800,paste('RMSE = ',rmse,sep=''),cex=1.8,pos=4)
  text(5000,1100,paste('Bias = ',bias,sep=''),cex=1.8,pos=4)
  text(5000,400,'n = 100000',cex=1.8,pos=4)
}

dev.off()


png(filename='1to1_eviarea.png',width=12,height=8,unit='in',res=300)

par(mfrow=c(2,3),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0))
spec <- rev(brewer.pal(11,'Spectral'))
mycolRamp = colorRampPalette(c('White',spec))
for(i in 1:6){
  x1 <- datEVIare[[i]][,1]
  y1 <- datEVIare[[i]][,2]

  limx <- c(0,22000)
  limy <- c(0,22000)

  smoothScatter(x1,y1,xlim=limx,ylim=limy,
                colramp=mycolRamp,nbin=600,nrpoints=0,
                transformation=function(x)x^.6,axe=F,
                xlab='Planet EVI2 area (x 0.0001)',ylab='HLS EVI2 area (x 0.0001)',
                cex.lab=1.7)
  axis(1,at=seq(0,30000,10000),cex.axis=1.7)
  axis(2,at=seq(0,30000,10000),cex.axis=1.7)
  abline(0,1,lty=5)

  reg <- round(cor(x1,y1,use='na.or.complete'),3)
  rmse <- round(sqrt(mean((x1-y1)^2,na.rm=T)))
  bias <- round(mean(x1-y1,na.rm=T))

  text(13000,5000,expression(paste(italic(r),' =',sep='')),cex=1.8,pos=4)
  text(16000,5000,reg,cex=1.8,pos=4)
  text(13000,3500,paste('RMSE = ',rmse,sep=''),cex=1.8,pos=4)
  text(13000,2000,paste('Bias = ',bias,sep=''),cex=1.8,pos=4)
  text(13000,500,'n = 100000',cex=1.8,pos=4)
}

dev.off()


######
# Quantile for all metrics
statPhedb <- matrix(NA,11,5)
statPhemf <- matrix(NA,11,5)
statPheen <- matrix(NA,11,5)
statPheag <- matrix(NA,11,5)
statPhegr <- matrix(NA,11,5)
statPhesh <- matrix(NA,11,5)

temp <- datQtil[[1]]
for(i in 1:11){
  statPhedb[i,1] <- cor(temp[,(i+1)],temp[,1],use='na.or.complete')
  statPhedb[i,2] <- sqrt(mean((temp[,(i+1)]-temp[,1])^2,na.rm=T))
  statPhedb[i,3] <- mean(temp[,(i+1)]-temp[,1],na.rm=T)
}
temp <- datQtil[[2]]
for(i in 1:11){
  statPhemf[i,1] <- cor(temp[,(i+1)],temp[,1],use='na.or.complete')
  statPhemf[i,2] <- sqrt(mean((temp[,(i+1)]-temp[,1])^2,na.rm=T))
  statPhemf[i,3] <- mean(temp[,(i+1)]-temp[,1],na.rm=T)
}
temp <- datQtil[[3]]
for(i in 1:11){
  statPheen[i,1] <- cor(temp[,(i+1)],temp[,1],use='na.or.complete')
  statPheen[i,2] <- sqrt(mean((temp[,(i+1)]-temp[,1])^2,na.rm=T))
  statPheen[i,3] <- mean(temp[,(i+1)]-temp[,1],na.rm=T)
}
temp <- datQtil[[4]]
for(i in 1:11){
  statPheag[i,1] <- cor(temp[,(i+1)],temp[,1],use='na.or.complete')
  statPheag[i,2] <- sqrt(mean((temp[,(i+1)]-temp[,1])^2,na.rm=T))
  statPheag[i,3] <- mean(temp[,(i+1)]-temp[,1],na.rm=T)
}
temp <- datQtil[[5]]
for(i in 1:11){
  statPhegr[i,1] <- cor(temp[,(i+1)],temp[,1],use='na.or.complete')
  statPhegr[i,2] <- sqrt(mean((temp[,(i+1)]-temp[,1])^2,na.rm=T))
  statPhegr[i,3] <- mean(temp[,(i+1)]-temp[,1],na.rm=T)
}
temp <- datQtil[[6]]
for(i in 1:11){
  statPhesh[i,1] <- cor(temp[,(i+1)],temp[,1],use='na.or.complete')
  statPhesh[i,2] <- sqrt(mean((temp[,(i+1)]-temp[,1])^2,na.rm=T))
  statPhesh[i,3] <- mean(temp[,(i+1)]-temp[,1],na.rm=T)
}

# Quantile for 50% greenup
statPhedbs <- matrix(NA,11,5)
statPhemfs <- matrix(NA,11,5)
statPheens <- matrix(NA,11,5)
statPheags <- matrix(NA,11,5)
statPhegrs <- matrix(NA,11,5)
statPheshs <- matrix(NA,11,5)

temp <- datQtil[[1]]
for(i in 1:11){
  statPhedbs[i,1] <- cor(temp[10001:20000,(i+1)],temp[10001:20000,1],use='na.or.complete')
  statPhedbs[i,2] <- sqrt(mean((temp[10001:20000,(i+1)]-temp[10001:20000,1])^2,na.rm=T))
  statPhedbs[i,3] <- mean(temp[10001:20000,(i+1)]-temp[10001:20000,1],na.rm=T)
}
temp <- datQtil[[2]]
for(i in 1:11){
  statPhemfs[i,1] <- cor(temp[10001:20000,(i+1)],temp[10001:20000,1],use='na.or.complete')
  statPhemfs[i,2] <- sqrt(mean((temp[10001:20000,(i+1)]-temp[10001:20000,1])^2,na.rm=T))
  statPhemfs[i,3] <- mean(temp[10001:20000,(i+1)]-temp[10001:20000,1],na.rm=T)
}
temp <- datQtil[[3]]
for(i in 1:11){
  statPheens[i,1] <- cor(temp[10001:20000,(i+1)],temp[10001:20000,1],use='na.or.complete')
  statPheens[i,2] <- sqrt(mean((temp[10001:20000,(i+1)]-temp[10001:20000,1])^2,na.rm=T))
  statPheens[i,3] <- mean(temp[10001:20000,(i+1)]-temp[10001:20000,1],na.rm=T)
}
temp <- datQtil[[4]]
for(i in 1:11){
  statPheags[i,1] <- cor(temp[10001:20000,(i+1)],temp[10001:20000,1],use='na.or.complete')
  statPheags[i,2] <- sqrt(mean((temp[10001:20000,(i+1)]-temp[10001:20000,1])^2,na.rm=T))
  statPheags[i,3] <- mean(temp[10001:20000,(i+1)]-temp[10001:20000,1],na.rm=T)
}
temp <- datQtil[[5]]
for(i in 1:11){
  statPhegrs[i,1] <- cor(temp[10001:20000,(i+1)],temp[10001:20000,1],use='na.or.complete')
  statPhegrs[i,2] <- sqrt(mean((temp[10001:20000,(i+1)]-temp[10001:20000,1])^2,na.rm=T))
  statPhegrs[i,3] <- mean(temp[10001:20000,(i+1)]-temp[10001:20000,1],na.rm=T)
}
temp <- datQtil[[6]]
for(i in 1:11){
  statPheshs[i,1] <- cor(temp[10001:20000,(i+1)],temp[10001:20000,1],use='na.or.complete')
  statPheshs[i,2] <- sqrt(mean((temp[10001:20000,(i+1)]-temp[10001:20000,1])^2,na.rm=T))
  statPheshs[i,3] <- mean(temp[10001:20000,(i+1)]-temp[10001:20000,1],na.rm=T)
}

# Quantile for 50% greendown
statPhedbd <- matrix(NA,11,5)
statPhemfd <- matrix(NA,11,5)
statPheend <- matrix(NA,11,5)
statPheagd <- matrix(NA,11,5)
statPhegrd <- matrix(NA,11,5)
statPheshd <- matrix(NA,11,5)

temp <- datQtil[[1]]
for(i in 1:11){
  statPhedbd[i,1] <- cor(temp[50001:60000,(i+1)],temp[50001:60000,1],use='na.or.complete')
  statPhedbd[i,2] <- sqrt(mean((temp[50001:60000,(i+1)]-temp[50001:60000,1])^2,na.rm=T))
  statPhedbd[i,3] <- mean(temp[50001:60000,(i+1)]-temp[50001:60000,1],na.rm=T)
}
temp <- datQtil[[2]]
for(i in 1:11){
  statPhemfd[i,1] <- cor(temp[50001:60000,(i+1)],temp[50001:60000,1],use='na.or.complete')
  statPhemfd[i,2] <- sqrt(mean((temp[50001:60000,(i+1)]-temp[50001:60000,1])^2,na.rm=T))
  statPhemfd[i,3] <- mean(temp[50001:60000,(i+1)]-temp[50001:60000,1],na.rm=T)
}
temp <- datQtil[[3]]
for(i in 1:11){
  statPheend[i,1] <- cor(temp[50001:60000,(i+1)],temp[50001:60000,1],use='na.or.complete')
  statPheend[i,2] <- sqrt(mean((temp[50001:60000,(i+1)]-temp[50001:60000,1])^2,na.rm=T))
  statPheend[i,3] <- mean(temp[50001:60000,(i+1)]-temp[50001:60000,1],na.rm=T)
}
temp <- datQtil[[4]]
for(i in 1:11){
  statPheagd[i,1] <- cor(temp[50001:60000,(i+1)],temp[50001:60000,1],use='na.or.complete')
  statPheagd[i,2] <- sqrt(mean((temp[50001:60000,(i+1)]-temp[50001:60000,1])^2,na.rm=T))
  statPheagd[i,3] <- mean(temp[50001:60000,(i+1)]-temp[50001:60000,1],na.rm=T)
}
temp <- datQtil[[5]]
for(i in 1:11){
  statPhegrd[i,1] <- cor(temp[50001:60000,(i+1)],temp[50001:60000,1],use='na.or.complete')
  statPhegrd[i,2] <- sqrt(mean((temp[50001:60000,(i+1)]-temp[50001:60000,1])^2,na.rm=T))
  statPhegrd[i,3] <- mean(temp[50001:60000,(i+1)]-temp[50001:60000,1],na.rm=T)
}
temp <- datQtil[[6]]
for(i in 1:11){
  statPheshd[i,1] <- cor(temp[50001:60000,(i+1)],temp[50001:60000,1],use='na.or.complete')
  statPheshd[i,2] <- sqrt(mean((temp[50001:60000,(i+1)]-temp[50001:60000,1])^2,na.rm=T))
  statPheshd[i,3] <- mean(temp[50001:60000,(i+1)]-temp[50001:60000,1],na.rm=T)
}

################
# plot - all
setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
png(filename='stat_1to1_phe_2.png',width=13,height=7.5,unit='in',res=300)

mycol <- brewer.pal(7,'Set1')
mycol <- c(mycol[1:5],mycol[7])
par(fig=c(0,0.318,0,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0))
plot(c(5,seq(10,90,10),95),statPhedb[,1],pch=21,cex=1.8,bg=mycol[1],
     xlim=c(0,100),ylim=c(0.84,1),type='o',lwd=2,axe=F,ann=F)
box(lty=1)
axis(1,c(5,25,50,75,95),cex.axis=1.3)
axis(2,seq(0.80,1,0.05),cex.axis=1.3)
mtext('Quantile (%)',1,2.5,cex=1.5)
mtext('Correlation',2,2.5,cex=1.5)

legend('bottomleft',c('DB','MF','EN','AG','GR','SH'),
       pch=c(21:25,21),cex=1.5,bty='n',lwd=2,pt.bg=mycol,pt.cex=1.8)

points(c(5,seq(10,90,10),95),statPhemf[,1],type='o',pch=22,cex=1.8,lwd=2,bg=mycol[2])
points(c(5,seq(10,90,10),95),statPheen[,1],type='o',pch=23,cex=1.8,lwd=2,bg=mycol[3])
points(c(5,seq(10,90,10),95),statPheag[,1],type='o',pch=24,cex=1.8,lwd=2,bg=mycol[4])
points(c(5,seq(10,90,10),95),statPhegr[,1],type='o',pch=25,cex=1.8,lwd=2,bg=mycol[5])
points(c(5,seq(10,90,10),95),statPhesh[,1],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[6])

par(fig=c(0.342,0.679,0,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0),new=T)
plot(c(5,seq(10,90,10),95),statPhedb[,2],pch=21,cex=1.8,bg=mycol[1],
     xlim=c(0,100),ylim=c(8,50),type='o',lwd=2,axe=F,ann=F)
box(lty=1)
axis(1,c(5,25,50,75,95),cex.axis=1.3)
axis(2,seq(0,50,10),cex.axis=1.3)
mtext('Quantile (%)',1,2.5,cex=1.5)
mtext('RMSE',2,2.5,cex=1.5)

points(c(5,seq(10,90,10),95),statPhemf[,2],type='o',pch=22,cex=1.8,lwd=2,bg=mycol[2])
points(c(5,seq(10,90,10),95),statPheen[,2],type='o',pch=23,cex=1.8,lwd=2,bg=mycol[3])
points(c(5,seq(10,90,10),95),statPheag[,2],type='o',pch=24,cex=1.8,lwd=2,bg=mycol[4])
points(c(5,seq(10,90,10),95),statPhegr[,2],type='o',pch=25,cex=1.8,lwd=2,bg=mycol[5])
points(c(5,seq(10,90,10),95),statPhesh[,2],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[6])

par(fig=c(0.681,1,0,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0),new=T)
plot(c(5,seq(10,90,10),95),statPhedb[,3],pch=21,cex=1.8,bg=mycol[1],
     xlim=c(0,100),ylim=c(-23,23),type='o',lwd=2,axe=F,ann=F)
box(lty=1)
axis(1,c(5,25,50,75,95),cex.axis=1.3)
axis(2,seq(-30,30,5),cex.axis=1.3)
mtext('Quantile (%)',1,2.5,cex=1.5)
mtext('Bias',2,2.5,cex=1.5)

points(c(5,seq(10,90,10),95),statPhemf[,3],type='o',pch=22,cex=1.8,lwd=2,bg=mycol[2])
points(c(5,seq(10,90,10),95),statPheen[,3],type='o',pch=23,cex=1.8,lwd=2,bg=mycol[3])
points(c(5,seq(10,90,10),95),statPheag[,3],type='o',pch=24,cex=1.8,lwd=2,bg=mycol[4])
points(c(5,seq(10,90,10),95),statPhegr[,3],type='o',pch=25,cex=1.8,lwd=2,bg=mycol[5])
points(c(5,seq(10,90,10),95),statPhesh[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[6])
abline(h=0,lty=5)

dev.off()

# plot - 50% greenup
setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
png(filename='stat_1to1_phe_greenup.png',width=13,height=8.5,unit='in',res=300)

mycol <- brewer.pal(7,'Set1')
mycol <- c(mycol[1:5],mycol[7])
par(fig=c(0,0.318,0,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0))
plot(c(5,seq(10,90,10),95),statPhedbs[,1],pch=21,cex=1.8,bg=mycol[1],
     xlim=c(0,100),ylim=c(0,1),type='o',lwd=2,axe=F,ann=F)
box(lty=1)
axis(1,c(5,25,50,75,95),cex.axis=1.3)
axis(2,seq(0.0,1,0.2),cex.axis=1.3)
mtext('Quantile (%)',1,2.5,cex=1.5)
mtext('Correlation',2,2.5,cex=1.5)

points(c(5,seq(10,90,10),95),statPhemfs[,1],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[2])
points(c(5,seq(10,90,10),95),statPheens[,1],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[3])
points(c(5,seq(10,90,10),95),statPheags[,1],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[4])
points(c(5,seq(10,90,10),95),statPhegrs[,1],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[5])
points(c(5,seq(10,90,10),95),statPheshs[,1],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[6])

par(fig=c(0.342,0.679,0,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0),new=T)
plot(c(5,seq(10,90,10),95),statPhedbs[,2],pch=21,cex=1.8,bg=mycol[1],
     xlim=c(0,100),ylim=c(0,60),type='o',lwd=2,axe=F,ann=F)
box(lty=1)
axis(1,c(5,25,50,75,95),cex.axis=1.3)
axis(2,seq(0,50,10),cex.axis=1.3)
mtext('Quantile (%)',1,2.5,cex=1.5)
mtext('RMSE',2,2.5,cex=1.5)

legend('topright',c('DB','MF','EN','AG','GR','SH'),
       pch=21,cex=1.5,bty='n',lwd=2,pt.bg=mycol,pt.cex=1.8)


points(c(5,seq(10,90,10),95),statPhemfs[,2],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[2])
points(c(5,seq(10,90,10),95),statPheens[,2],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[3])
points(c(5,seq(10,90,10),95),statPheags[,2],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[4])
points(c(5,seq(10,90,10),95),statPhegrs[,2],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[5])
points(c(5,seq(10,90,10),95),statPheshs[,2],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[6])

par(fig=c(0.681,1,0,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0),new=T)
plot(c(5,seq(10,90,10),95),statPhedbs[,3],pch=21,cex=1.8,bg=mycol[1],
     xlim=c(0,100),ylim=c(-30,30),type='o',lwd=2,axe=F,ann=F)
box(lty=1)
axis(1,c(5,25,50,75,95),cex.axis=1.3)
axis(2,seq(-30,30,10),cex.axis=1.3)
mtext('Quantile (%)',1,2.5,cex=1.5)
mtext('Bias',2,2.5,cex=1.5)

points(c(5,seq(10,90,10),95),statPhemfs[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[2])
points(c(5,seq(10,90,10),95),statPheens[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[3])
points(c(5,seq(10,90,10),95),statPheags[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[4])
points(c(5,seq(10,90,10),95),statPhegrs[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[5])
points(c(5,seq(10,90,10),95),statPheshs[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[6])
abline(h=0,lty=5)

dev.off()

# plot - greendown
setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
png(filename='stat_1to1_phe_greendown.png',width=13,height=8.5,unit='in',res=300)

mycol <- brewer.pal(7,'Set1')
mycol <- c(mycol[1:5],mycol[7])
par(fig=c(0,0.318,0,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0))
plot(c(5,seq(10,90,10),95),statPhedbd[,1],pch=21,cex=1.8,bg=mycol[1],
     xlim=c(0,100),ylim=c(0,1),type='o',lwd=2,axe=F,ann=F)
box(lty=1)
axis(1,c(5,25,50,75,95),cex.axis=1.3)
axis(2,seq(0.0,1,0.2),cex.axis=1.3)
mtext('Quantile (%)',1,2.5,cex=1.5)
mtext('Correlation',2,2.5,cex=1.5)

points(c(5,seq(10,90,10),95),statPhemfd[,1],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[2])
points(c(5,seq(10,90,10),95),statPheend[,1],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[3])
points(c(5,seq(10,90,10),95),statPheagd[,1],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[4])
points(c(5,seq(10,90,10),95),statPhegrd[,1],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[5])
points(c(5,seq(10,90,10),95),statPheshd[,1],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[6])

par(fig=c(0.342,0.679,0,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0),new=T)
plot(c(5,seq(10,90,10),95),statPhedbd[,2],pch=21,cex=1.8,bg=mycol[1],
     xlim=c(0,100),ylim=c(0,60),type='o',lwd=2,axe=F,ann=F)
box(lty=1)
axis(1,c(5,25,50,75,95),cex.axis=1.3)
axis(2,seq(0,50,10),cex.axis=1.3)
mtext('Quantile (%)',1,2.5,cex=1.5)
mtext('RMSE',2,2.5,cex=1.5)

points(c(5,seq(10,90,10),95),statPhemfd[,2],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[2])
points(c(5,seq(10,90,10),95),statPheend[,2],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[3])
points(c(5,seq(10,90,10),95),statPheagd[,2],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[4])
points(c(5,seq(10,90,10),95),statPhegrd[,2],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[5])
points(c(5,seq(10,90,10),95),statPheshd[,2],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[6])

par(fig=c(0.681,1,0,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0),new=T)
plot(c(5,seq(10,90,10),95),statPhedbd[,3],pch=21,cex=1.8,bg=mycol[1],
     xlim=c(0,100),ylim=c(-30,30),type='o',lwd=2,axe=F,ann=F)
box(lty=1)
axis(1,c(5,25,50,75,95),cex.axis=1.3)
axis(2,seq(-30,30,10),cex.axis=1.3)
mtext('Quantile (%)',1,2.5,cex=1.5)
mtext('Bias',2,2.5,cex=1.5)

legend('bottomright',c('DB','MF','EN','AG','GR','SH'),
       pch=21,cex=1.5,bty='n',lwd=2,pt.bg=mycol,pt.cex=1.8)

points(c(5,seq(10,90,10),95),statPhemfd[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[2])
points(c(5,seq(10,90,10),95),statPheend[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[3])
points(c(5,seq(10,90,10),95),statPheagd[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[4])
points(c(5,seq(10,90,10),95),statPhegrd[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[5])
points(c(5,seq(10,90,10),95),statPheshd[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[6])
abline(h=0,lty=5)

dev.off()


# plot - only bias
setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
png(filename='stat_1to1_phe_bias.png',width=9,height=7,unit='in',res=300)

mycol <- brewer.pal(7,'Set1')
mycol <- c(mycol[1:5],mycol[7])
par(fig=c(0,0.5,0,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0),new=T)
plot(c(5,seq(10,90,10),95),statPhedbs[,3],pch=21,cex=1.8,bg=mycol[1],
     xlim=c(0,100),ylim=c(-27,27),type='o',lwd=2,axe=F,ann=F)
box(lty=1)
axis(1,c(5,25,50,75,95),cex.axis=1.3)
axis(2,seq(-30,30,10),cex.axis=1.3)
mtext('Quantile (%)',1,2.5,cex=1.5)
mtext('Bias',2,2.5,cex=1.5)

legend('bottomright',c('DB','MF','EN','AG','GR','SH'),
       pch=21,cex=1.5,bty='n',lwd=2,pt.bg=mycol,pt.cex=1.8)

points(c(5,seq(10,90,10),95),statPhemfs[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[2])
points(c(5,seq(10,90,10),95),statPheens[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[3])
points(c(5,seq(10,90,10),95),statPheags[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[4])
points(c(5,seq(10,90,10),95),statPhegrs[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[5])
points(c(5,seq(10,90,10),95),statPheshs[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[6])
abline(h=0,lty=5)

par(fig=c(0.5,1,0,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0),new=T)
plot(c(5,seq(10,90,10),95),statPhedbd[,3],pch=21,cex=1.8,bg=mycol[1],
     xlim=c(0,100),ylim=c(-27,27),type='o',lwd=2,axe=F,ann=F)
box(lty=1)
axis(1,c(5,25,50,75,95),cex.axis=1.3)
axis(2,seq(-30,30,10),cex.axis=1.3)
mtext('Quantile (%)',1,2.5,cex=1.5)
mtext('Bias',2,2.5,cex=1.5)

points(c(5,seq(10,90,10),95),statPhemfd[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[2])
points(c(5,seq(10,90,10),95),statPheend[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[3])
points(c(5,seq(10,90,10),95),statPheagd[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[4])
points(c(5,seq(10,90,10),95),statPhegrd[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[5])
points(c(5,seq(10,90,10),95),statPheshd[,3],type='o',pch=21,cex=1.8,lwd=2,bg=mycol[6])
abline(h=0,lty=5)

dev.off()


################
# PhenoCam
sites <- c('NEON.D02.SCBI.DP1.00033',

           'NEON.D01.HARV.DP1.00033_DB',
           'NEON.D01.HARV.DP1.00033_EN',

           'howland1',

           'uiefmaize2',
           'uiefmiscanthus2',
           'uiefprairie2',
           'uiefswitchgrass2',

           'cperuvb',
           'burnssagebrush')

dateGcc <- matrix(NA,length(sites)*3,2)
path <- '/projectnb/modislc/users/mkmoon/Planet/data/PhenoCam'
for(ss in 1:length(sites)){
  sstr <- paste0(sites[ss],'*3day_transition_dates.csv')
  filename <- list.files(path=path,pattern=glob2rx(sstr),full.names=T,recursive=T)
  dat <- read.csv(filename,skip=16)

  dat1 <- dat[dat$direction=='rising'&dat$gcc_value=='gcc_90',8]
  dat2 <- dat[dat$direction=='falling'&dat$gcc_value=='gcc_90',8]

  # print(dat1[length(dat1)])
  # print(dat2[1])
  try({
    dateGcc[ss,1]    <- as.numeric(strftime(as.Date(dat1[which(substr(dat1,1,4)==2019)]), format = "%j"))
  },silent=T)
  try({
    dateGcc[ss+10,1] <- as.numeric(strftime(as.Date(dat1[which(substr(dat1,1,4)==2018)]), format = "%j"))
  },silent=T)
  try({
    dateGcc[ss+20,1] <- as.numeric(strftime(as.Date(dat1[which(substr(dat1,1,4)==2017)]), format = "%j"))
  },silent=T)
  try({
    dateGcc[ss,2]    <- as.numeric(strftime(as.Date(dat2[which(substr(dat2,1,4)==2019)]), format = "%j"))
  },silent=T)
  try({
    dateGcc[ss+10,2] <- as.numeric(strftime(as.Date(dat2[which(substr(dat2,1,4)==2018)]), format = "%j"))
  },silent=T)
  try({
    dateGcc[ss+20,2] <- as.numeric(strftime(as.Date(dat2[which(substr(dat2,1,4)==2017)]), format = "%j"))  
  },silent=T)
}

datePP <- matrix(NA,length(sites)*3,2)
dateHH <- matrix(NA,length(sites)*3,2)
rastPP <- vector('list',8)
rastHH <- vector('list',8)
for(yy in 2019:2017){
  for(ss in 1:6){
    vgt <- c('DB','MF','EN','AG','GR','SH')
    path <- paste0('/projectnb/modislc/users/mkmoon/Planet/phe/',yy,'/',vgt[ss])
    filePhe <- list.files(path,pattern=glob2rx('phe*_1.rda'),full.names=T)
    
    for(cc in 1:length(filePhe)){
      load(filePhe[cc])
      if(cc==1){
        pheMat <- pheme
      }else{
        pheMat <- rbind(pheMat,pheme)
      }
    }
    
    for(rr in 1:8){
      rastPP[[rr]] <- setValues(limgBase[[ss]],pheMat[,rr])
    }
    
    #
    tiles <- c('17SQD','18TYN','19TEL','16TCK','13TEF','10TGP')
    tile <- tiles[ss]
    fileHLS <- paste0('/projectnb/modislc/projects/landsat_sentinel/MSLSP_HLS30/',tile,'/phenoMetrics/MSLSP_',tile,'_',yy,'.nc')
    nc <- nc_open(fileHLS)
    var <- names(nc[['var']])
    for(rr in c(2,6)){
      pheHLS <- raster(fileHLS,varname=var[(rr+2)])
      rastHH[[rr]] <- projectRaster(pheHLS,rastPP[[1]],method='ngb')
    }
    
    shpPoints <- readOGR(paste('/projectnb/modislc/users/mkmoon/Planet/shp/',tile,'_pts_1.shp',sep=''))
    
    if(yy==2019){
      if(ss==1){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[1,1] <- extract(rastPP[[2]],shp)[3]
        datePP[1,2] <- extract(rastPP[[6]],shp)[3]
        
        shp <- spTransform(shpPoints,crs(rastHH[[2]]))
        dateHH[1,1] <- extract(rastHH[[2]],shp)[3]
        dateHH[1,2] <- extract(rastHH[[6]],shp)[3]
      }else if(ss==2){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[2,1] <- extract(rastPP[[2]],shp)[1]
        datePP[2,2] <- extract(rastPP[[6]],shp)[1]
        datePP[3,1] <- extract(rastPP[[2]],shp)[2]
        datePP[3,2] <- extract(rastPP[[6]],shp)[2]
        
        shp <- spTransform(shpPoints,crs(rastHH[[2]]))
        dateHH[2,1] <- extract(rastHH[[2]],shp)[1]
        dateHH[2,2] <- extract(rastHH[[6]],shp)[1]
        dateHH[3,1] <- extract(rastHH[[2]],shp)[2]
        dateHH[3,2] <- extract(rastHH[[6]],shp)[2]
      }else if(ss==3){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[4,1] <- extract(rastPP[[2]],shp)[3]
        datePP[4,2] <- extract(rastPP[[6]],shp)[3]
        
        shp <- spTransform(shpPoints,crs(rastHH[[2]]))
        dateHH[4,1] <- extract(rastHH[[2]],shp)[3]
        dateHH[4,2] <- extract(rastHH[[6]],shp)[3]
      }else if(ss==4){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[5,1] <- extract(rastPP[[2]],shp)[1]
        datePP[5,2] <- extract(rastPP[[6]],shp)[1]
        datePP[6,1] <- extract(rastPP[[2]],shp)[2]
        datePP[6,2] <- extract(rastPP[[6]],shp)[2]
        datePP[7,1] <- extract(rastPP[[2]],shp)[3]
        datePP[7,2] <- extract(rastPP[[6]],shp)[3]
        datePP[8,1] <- extract(rastPP[[2]],shp)[4]
        datePP[8,2] <- extract(rastPP[[6]],shp)[4]
        
        shp <- spTransform(shpPoints,crs(rastHH[[2]]))
        dateHH[5,1] <- extract(rastHH[[2]],shp)[1]
        dateHH[5,2] <- extract(rastHH[[6]],shp)[1]
        dateHH[6,1] <- extract(rastHH[[2]],shp)[2]
        dateHH[6,2] <- extract(rastHH[[6]],shp)[2]
        dateHH[7,1] <- extract(rastHH[[2]],shp)[3]
        dateHH[7,2] <- extract(rastHH[[6]],shp)[3]
        dateHH[8,1] <- extract(rastHH[[2]],shp)[4]
        dateHH[8,2] <- extract(rastHH[[6]],shp)[4]
      }else if(ss==5){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[9,1] <- extract(rastPP[[2]],shp)[3]
        datePP[9,2] <- extract(rastPP[[6]],shp)[3]
        
        shp <- spTransform(shpPoints,crs(rastHH[[2]]))
        dateHH[9,1] <- extract(rastHH[[2]],shp)[3]
        dateHH[9,2] <- extract(rastHH[[6]],shp)[3]
      }else{
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[10,1] <- extract(rastPP[[2]],shp)[4]
        datePP[10,2] <- extract(rastPP[[6]],shp)[4]
        
        shp <- spTransform(shpPoints,crs(rastHH[[2]]))
        dateHH[10,1] <- extract(rastHH[[2]],shp)[4]
        dateHH[10,2] <- extract(rastHH[[6]],shp)[4]
      }  
    }else if(yy==2018){
      if(ss==1){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[1+10,1] <- extract(rastPP[[2]],shp)[3]
        datePP[1+10,2] <- extract(rastPP[[6]],shp)[3]
        
        shp <- spTransform(shpPoints,crs(rastHH[[2]]))
        dateHH[1+10,1] <- extract(rastHH[[2]],shp)[3]
        dateHH[1+10,2] <- extract(rastHH[[6]],shp)[3]
      }else if(ss==2){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[2+10,1] <- extract(rastPP[[2]],shp)[1]
        datePP[2+10,2] <- extract(rastPP[[6]],shp)[1]
        datePP[3+10,1] <- extract(rastPP[[2]],shp)[2]
        datePP[3+10,2] <- extract(rastPP[[6]],shp)[2]
        
        shp <- spTransform(shpPoints,crs(rastHH[[2]]))
        dateHH[2+10,1] <- extract(rastHH[[2]],shp)[1]
        dateHH[2+10,2] <- extract(rastHH[[6]],shp)[1]
        dateHH[3+10,1] <- extract(rastHH[[2]],shp)[2]
        dateHH[3+10,2] <- extract(rastHH[[6]],shp)[2]
      }else if(ss==3){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[4+10,1] <- extract(rastPP[[2]],shp)[3]
        datePP[4+10,2] <- extract(rastPP[[6]],shp)[3]
        
        shp <- spTransform(shpPoints,crs(rastHH[[2]]))
        dateHH[4+10,1] <- extract(rastHH[[2]],shp)[3]
        dateHH[4+10,2] <- extract(rastHH[[6]],shp)[3]
      }else if(ss==4){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[5+10,1] <- extract(rastPP[[2]],shp)[1]
        datePP[5+10,2] <- extract(rastPP[[6]],shp)[1]
        datePP[6+10,1] <- extract(rastPP[[2]],shp)[2]
        datePP[6+10,2] <- extract(rastPP[[6]],shp)[2]
        datePP[7+10,1] <- extract(rastPP[[2]],shp)[3]
        datePP[7+10,2] <- extract(rastPP[[6]],shp)[3]
        datePP[8+10,1] <- extract(rastPP[[2]],shp)[4]
        datePP[8+10,2] <- extract(rastPP[[6]],shp)[4]
        
        shp <- spTransform(shpPoints,crs(rastHH[[2]]))
        dateHH[5+10,1] <- extract(rastHH[[2]],shp)[1]
        dateHH[5+10,2] <- extract(rastHH[[6]],shp)[1]
        dateHH[6+10,1] <- extract(rastHH[[2]],shp)[2]
        dateHH[6+10,2] <- extract(rastHH[[6]],shp)[2]
        dateHH[7+10,1] <- extract(rastHH[[2]],shp)[3]
        dateHH[7+10,2] <- extract(rastHH[[6]],shp)[3]
        dateHH[8+10,1] <- extract(rastHH[[2]],shp)[4]
        dateHH[8+10,2] <- extract(rastHH[[6]],shp)[4]
      }else if(ss==5){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[9+10,1] <- extract(rastPP[[2]],shp)[3]
        datePP[9+10,2] <- extract(rastPP[[6]],shp)[3]
        
        shp <- spTransform(shpPoints,crs(rastHH[[2]]))
        dateHH[9+10,1] <- extract(rastHH[[2]],shp)[3]
        dateHH[9+10,2] <- extract(rastHH[[6]],shp)[3]
      }else{
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[10+10,1] <- extract(rastPP[[2]],shp)[4]
        datePP[10+10,2] <- extract(rastPP[[6]],shp)[4]
        
        shp <- spTransform(shpPoints,crs(rastHH[[2]]))
        dateHH[10+10,1] <- extract(rastHH[[2]],shp)[4]
        dateHH[10+10,2] <- extract(rastHH[[6]],shp)[4]
      }
    }else{
      if(ss==1){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[1+20,1] <- extract(rastPP[[2]],shp)[3]
        datePP[1+20,2] <- extract(rastPP[[6]],shp)[3]
        
        shp <- spTransform(shpPoints,crs(rastHH[[2]]))
        dateHH[1+20,1] <- extract(rastHH[[2]],shp)[3]
        dateHH[1+20,2] <- extract(rastHH[[6]],shp)[3]
      }else if(ss==2){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[2+20,1] <- extract(rastPP[[2]],shp)[1]
        datePP[2+20,2] <- extract(rastPP[[6]],shp)[1]
        datePP[3+20,1] <- extract(rastPP[[2]],shp)[2]
        datePP[3+20,2] <- extract(rastPP[[6]],shp)[2]
        
        shp <- spTransform(shpPoints,crs(rastHH[[2]]))
        dateHH[2+20,1] <- extract(rastHH[[2]],shp)[1]
        dateHH[2+20,2] <- extract(rastHH[[6]],shp)[1]
        dateHH[3+20,1] <- extract(rastHH[[2]],shp)[2]
        dateHH[3+20,2] <- extract(rastHH[[6]],shp)[2]
      }else if(ss==3){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[4+20,1] <- extract(rastPP[[2]],shp)[3]
        datePP[4+20,2] <- extract(rastPP[[6]],shp)[3]
        
        shp <- spTransform(shpPoints,crs(rastHH[[2]]))
        dateHH[4+20,1] <- extract(rastHH[[2]],shp)[3]
        dateHH[4+20,2] <- extract(rastHH[[6]],shp)[3]
      }else if(ss==4){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[5+20,1] <- extract(rastPP[[2]],shp)[1]
        datePP[5+20,2] <- extract(rastPP[[6]],shp)[1]
        datePP[6+20,1] <- extract(rastPP[[2]],shp)[2]
        datePP[6+20,2] <- extract(rastPP[[6]],shp)[2]
        datePP[7+20,1] <- extract(rastPP[[2]],shp)[3]
        datePP[7+20,2] <- extract(rastPP[[6]],shp)[3]
        datePP[8+20,1] <- extract(rastPP[[2]],shp)[4]
        datePP[8+20,2] <- extract(rastPP[[6]],shp)[4]
        
        shp <- spTransform(shpPoints,crs(rastHH[[2]]))
        dateHH[5+20,1] <- extract(rastHH[[2]],shp)[1]
        dateHH[5+20,2] <- extract(rastHH[[6]],shp)[1]
        dateHH[6+20,1] <- extract(rastHH[[2]],shp)[2]
        dateHH[6+20,2] <- extract(rastHH[[6]],shp)[2]
        dateHH[7+20,1] <- extract(rastHH[[2]],shp)[3]
        dateHH[7+20,2] <- extract(rastHH[[6]],shp)[3]
        dateHH[8+20,1] <- extract(rastHH[[2]],shp)[4]
        dateHH[8+20,2] <- extract(rastHH[[6]],shp)[4]
      }else if(ss==5){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[9+20,1] <- extract(rastPP[[2]],shp)[3]
        datePP[9+20,2] <- extract(rastPP[[6]],shp)[3]
        
        shp <- spTransform(shpPoints,crs(rastHH[[2]]))
        dateHH[9+20,1] <- extract(rastHH[[2]],shp)[3]
        dateHH[9+20,2] <- extract(rastHH[[6]],shp)[3]
      }else{
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[10+20,1] <- extract(rastPP[[2]],shp)[4]
        datePP[10+20,2] <- extract(rastPP[[6]],shp)[4]
        
        shp <- spTransform(shpPoints,crs(rastHH[[2]]))
        dateHH[10+20,1] <- extract(rastHH[[2]],shp)[4]
        dateHH[10+20,2] <- extract(rastHH[[6]],shp)[4]
      }
    }
    
    print(ss)
  }  
}

setwd('/projectnb/modislc/users/mkmoon/Planet/data/')
# write.csv(datePP,file='pcs_pp.csv')
# write.csv(dateHH,file='pcs_hh.csv')
# write.csv(dateGcc,file='pcs_pc.csv')
datTT <- read.csv('pcs_total_1.csv')     
datTT[15:18,] <- NA
datTT[25:28,] <- NA
datePP <- datTT[,1:2]
dateHH <- datTT[,3:4]
dateGcc <- datTT[,5:6]

#
setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
png(filename='phe_1to1_phenocam_3yr_1.png',width=12,height=4.2,unit='in',res=300)

vgt <- c('DB','MF','EN','AG','GR','SH')
mycol <- brewer.pal(7,'Set1')
mycol <- c(mycol[1:5],mycol[7])
par(mfrow=c(1,3),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0))
plot(dateGcc[1,1],datePP[1,1],xlim=c(65,365),ylim=c(65,365),bg=mycol[1],pch=21,
     cex=2.2,axe=F,ann=F)
box(lty=1)
axis(1,seq(0,400,50),cex.axis=1.5)
axis(2,seq(0,400,50),cex.axis=1.5)
mtext('PhenoCam Dates (DOY)',1,2.6,cex=1.2)
mtext('Planet Dates (DOY)',2,2.6,cex=1.2)
abline(0,1,lty=5)
legend(100,365,vgt,pch=21,bty='n',cex=1.7,pt.cex=2.2,pt.bg=mycol)

for(i in 2:30){
  if(i==11|i==21){
    j <- 1
    ccol <- mycol[j]
  }else if(i==2|i==3|i==12|i==13|i==22|i==23){
    j <- 2
    ccol <- mycol[j]
  }else if(i==4|i==14|i==24){
    j <- 3
    ccol <- mycol[j]
  }else if(i>4&i<9|i>14&i<19|i>24&i<29){
    j <- 4
    ccol <- mycol[j]
  }else if(i==9|i==19|i==29){
    j <- 5
    ccol <- mycol[j]
  }else{
    j <- 6
    ccol <- mycol[j]
  }
  points(dateGcc[i,1],datePP[i,1],pch=21,cex=2.2,bg=ccol)
  print(j)
}
for(i in 1:30){
  if(i==1|i==11|i==21){
    j <- 1
    ccol <- mycol[j]
  }else if(i==2|i==3|i==12|i==13|i==22|i==23){
    j <- 2
    ccol <- mycol[j]
  }else if(i==4|i==14|i==24){
    j <- 3
    ccol <- mycol[j]
  }else if(i>4&i<9|i>14&i<19|i>24&i<29){
    j <- 4
    ccol <- mycol[j]
  }else if(i==9|i==19|i==29){
    j <- 5
    ccol <- mycol[j]
  }else{
    j <- 6
    ccol <- mycol[j]
  }
  points(dateGcc[i,2],datePP[i,2],pch=22,cex=2.2,bg=ccol)
}

x1 <- c(dateGcc[,1],dateGcc[,2])
y1 <- c(datePP[,1],datePP[,2])

reg <- formatC(round(cor(x1,y1,use='na.or.complete'),3), digits=3,format="fg", flag="#")
rmse <- formatC(round(sqrt(mean((x1-y1)^2,na.rm=T)),1),  digits=3,format="fg", flag="#")
bias <- formatC(round(mean(x1-y1,na.rm=T),1),  digits=3,format="fg")
# nn <- sum(!is.na(x1))

text(220,120,expression(paste(italic(r),' =',sep='')),cex=1.7,pos=4)
text(250,120,reg,cex=1.7,pos=4)
text(220,100,paste('RMSE = ',rmse,sep=''),cex=1.7,pos=4)
text(220,80,paste('Bias = ',bias,sep=''),cex=1.7,pos=4)

plot(dateGcc[1,1],dateHH[1,1],xlim=c(65,365),ylim=c(65,365),bg=mycol[1],pch=21,
     cex=2.2,axe=F,ann=F)
box(lty=1)
axis(1,seq(0,400,50),cex.axis=1.5)
axis(2,seq(0,400,50),cex.axis=1.5)
mtext('PhenoCam Dates (DOY)',1,2.6,cex=1.2)
mtext('HLS Dates (DOY)',2,2.6,cex=1.2)
abline(0,1,lty=5)

for(i in 2:30){
  if(i==11|i==21){
    j <- 1
    ccol <- mycol[j]
  }else if(i==2|i==3|i==12|i==13|i==22|i==23){
    j <- 2
    ccol <- mycol[j]
  }else if(i==4|i==14|i==24){
    j <- 3
    ccol <- mycol[j]
  }else if(i>4&i<9|i>14&i<19|i>24&i<29){
    j <- 4
    ccol <- mycol[j]
  }else if(i==9|i==19|i==29){
    j <- 5
    ccol <- mycol[j]
  }else{
    j <- 6
    ccol <- mycol[j]
  }
  points(dateGcc[i,1],dateHH[i,1],bg=ccol,pch=21,cex=2.2)
}
for(i in 1:30){
  if(i==1|i==11|i==21){
    j <- 1
    ccol <- mycol[j]
  }else if(i==2|i==3|i==12|i==13|i==22|i==23){
    j <- 2
    ccol <- mycol[j]
  }else if(i==4|i==14|i==24){
    j <- 3
    ccol <- mycol[j]
  }else if(i>4&i<9|i>14&i<19|i>24&i<29){
    j <- 4
    ccol <- mycol[j]
  }else if(i==9|i==19|i==29){
    j <- 5
    ccol <- mycol[j]
  }else{
    j <- 6
    ccol <- mycol[j]
  }
  points(dateGcc[i,2],dateHH[i,2],bg=ccol,pch=22,cex=2.2)
}

x1 <- c(dateGcc[,1],dateGcc[,2])
y1 <- c(dateHH[,1],dateHH[,2])

reg <- formatC(round(cor(x1,y1,use='na.or.complete'),3), digits=3,format="fg", flag="#")
rmse <- formatC(round(sqrt(mean((x1-y1)^2,na.rm=T)),1),  digits=3,format="fg", flag="#")
bias <- formatC(round(mean(x1-y1,na.rm=T),1),  digits=3,format="fg")
# nn <- sum(!is.na(x1))

text(220,120,expression(paste(italic(r),' =',sep='')),cex=1.7,pos=4)
text(250,120,reg,cex=1.7,pos=4)
text(220,100,paste('RMSE = ',rmse,sep=''),cex=1.7,pos=4)
text(220,80,paste('Bias = ',bias,sep=''),cex=1.7,pos=4)


plot(datePP[1,1],dateHH[1,1],xlim=c(65,365),ylim=c(65,365),bg=mycol[1],pch=21,
     cex=2.2,axe=F,ann=F)
box(lty=1)
axis(1,seq(0,400,50),cex.axis=1.5)
axis(2,seq(0,400,50),cex.axis=1.5)
mtext('Planet Dates (DOY)',1,2.6,cex=1.2)
mtext('HLS Dates (DOY)',2,2.6,cex=1.2)
abline(0,1,lty=5)

for(i in 2:30){
  if(i==11|i==21){
    j <- 1
    ccol <- mycol[j]
  }else if(i==2|i==3|i==12|i==13|i==22|i==23){
    j <- 2
    ccol <- mycol[j]
  }else if(i==4|i==14|i==24){
    j <- 3
    ccol <- mycol[j]
  }else if(i>4&i<9|i>14&i<19|i>24&i<29){
    j <- 4
    ccol <- mycol[j]
  }else if(i==9|i==19|i==29){
    j <- 5
    ccol <- mycol[j]
  }else{
    j <- 6
    ccol <- mycol[j]
  }
  points(datePP[i,1],dateHH[i,1],bg=ccol,pch=21,cex=2.2)
}
for(i in 1:30){
  if(i==1|i==11|i==21){
    j <- 1
    ccol <- mycol[j]
  }else if(i==2|i==3|i==12|i==13|i==22|i==23){
    j <- 2
    ccol <- mycol[j]
  }else if(i==4|i==14|i==24){
    j <- 3
    ccol <- mycol[j]
  }else if(i>4&i<9|i>14&i<19|i>24&i<29){
    j <- 4
    ccol <- mycol[j]
  }else if(i==9|i==19|i==29){
    j <- 5
    ccol <- mycol[j]
  }else{
    j <- 6
    ccol <- mycol[j]
  }
  points(datePP[i,2],dateHH[i,2],bg=ccol,pch=22,cex=2.2)
}

x1 <- c(datePP[,1],datePP[,2])
y1 <- c(dateHH[,1],dateHH[,2])

reg <- formatC(round(cor(x1,y1,use='na.or.complete'),3), digits=3,format="fg", flag="#")
rmse <- formatC(round(sqrt(mean((x1-y1)^2,na.rm=T)),1),  digits=3,format="fg", flag="#")
bias <- formatC(round(mean(x1-y1,na.rm=T),1),  digits=3,format="fg")
# nn <- sum(!is.na(x1))

text(220,120,expression(paste(italic(r),' =',sep='')),cex=1.7,pos=4)
text(250,120,reg,cex=1.7,pos=4)
text(220,100,paste('RMSE = ',rmse,sep=''),cex=1.7,pos=4)
text(220,80,paste('Bias = ',bias,sep=''),cex=1.7,pos=4)


dev.off()


# # # Flux tower data
# # geog_crs = CRS("+proj=longlat +datum=WGS84")
# # site <- data.frame(1,-72.171478,42.537755)
# # colnames(site) <- c('id','lon','lat')
# # xy   <- site[,c(2,3)]
# # bb   <- SpatialPointsDataFrame(coords=xy,data=site,proj4string=geog_crs)
# # bb <- spTransform(bb,crs(phePn))
# #
# # setwd('/projectnb/modislc/users/mkmoon/Planet/shp/')
# # writeOGR(bb,'.','18TYN_pts',driver="ESRI Shapefile",overwrite=T)
# #
# # plot(phePn)
# # plot(bb,add=T)
# #
# pixNum <- setValues(phePn,1:length(phePn))
# bb <- spTransform(bb,crs(phePn))
# pixid <- extract(pixNum,bb,buffer=45)
#
# eviMat <- matrix(NA,length(pixid[[1]]),length(dates))
# for(i in 1:length(dates)){
#   eviMat[,i] <- eviStack[[i]][pixid[[1]]]
# }
#
# # Plot
# for(i in 1:length(dates)){
#   if(i==1){
#     plot(dates,eviMat[i,],ylim=c(0,0.8))
#   }else{
#     points(dates,eviMat[i,])
#   }
# }
# eviMatMedi <- apply(eviMat,2,mean)
# eviMatSD <- apply(eviMat,2,sd)
#
# eviHLS <- matrix(NA,9,length(theTable[[1]]$dates))
# for(i in 1:9){
#   eviHLS[i,] <- theTable[[i]]$original_VI
# }
# eviHLSMedi <- apply(eviHLS,2,mean)
# eviHLSSD <- apply(eviHLS,2,sd)
#
#
# # Phenocam
# gcc <- read.csv('/projectnb/modislc/users/mkmoon/Phenocam/PhenoCam_V2_1674/data/harvard_DB_1000_3day.csv',
#                 skip=24)
# gcc90   <- gcc[gcc$year==2018,c('date','gcc_90')]
#
# # flux
# gpp <- read.csv('/projectnb/modislc/users/mkmoon/Planet/tower/hf004-02-filled.csv')
# gpp <- gpp[gpp$year==2019,]
# gpp <- gpp$gee
# gpp <- matrix(gpp,24,365)
# gpp <- -apply(gpp,2,mean)
# gpp <- gpp*10^-6*(60*60*24)*(44)
#
# ###########
# setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
# png(filename='timeseries_HF.png',width=11.5,height=6,units='in',res=300)
#
# par(oma=c(2,2,1,2),mar=c(4,8,1,4),mgp=c(2.5,1,0))
# plot(dates,eviMatMedi,pch=19,col='red',
#      xlim=c(17540+365,17900+365),ylim=c(0.05,0.85),
#      xlab='',ylab='',cex.lab=1.5,
#      cex.axis=1.5)
# points(theTable[[i]]$dates,eviHLSMedi,pch=19,col='blue')
#
# # load('/projectnb/modislc/users/mkmoon/autumn/sample_data/mms_gppFilled.rda')
# # gpp_day <- matrix(gpp,48,length(gpp)/48)
# # gpp_day <- apply(gpp_day,2,mean)*0.0864*44/2
#
# par(new = TRUE)
# plot(as.Date(11:357,origin='2018-12-31'),gpp[11:357],
#      axes=F,bty="n",xlab="",ylab="",col='black',pch=19,
#      ylim=c(-18,75),type='l',lwd=1.2)
# axis(side=4,at=seq(-50,100,20),cex.axis=1.5)
# mtext(expression(paste('GPP (g CO'[2],' ',m^-2,' ',day^-1,')',sep='')),
#       side=4,line=3,cex=1.5)
#
# par(new = TRUE)
# plot(dates,eviMatMedi,pch=19,col='red',
#      xlim=c(17540+365,17900+365),ylim=c(0.05,0.85),
#      xlab='2019',ylab='EVI2',cex.lab=1.5,
#      cex.axis=1.5,cex=1.5)
# points(theTable[[i]]$dates,eviHLSMedi,pch=19,col='blue',cex=1.5)
# arrows(dates,eviMatMedi-eviMatSD,dates,eviMatMedi+eviMatSD,
#        code=1,length=0,col='red')
# arrows(theTable[[i]]$dates,eviHLSMedi-eviHLSSD,theTable[[i]]$dates,eviHLSMedi+eviHLSSD,
#        code=1,length=0,col='blue')
#
# par(new = TRUE)
# plot(as.Date(1:365,origin='2017-12-31'),gcc90$gcc_90,
#      ylim=c(0.34,0.507),axes=F,bty="n",xlab="",ylab="",col='forestgreen',pch=19)
# axis(side=2,at=seq(0,1,0.05),cex.axis=1.5,line=4)
# mtext('GCC',side=2,line=6.5,cex=1.5)
#
# legend('topleft',c('HLS','Planet','GCC','GPP'),
#        pch=c(19,19,19,NA),
#        lty=c(NA,NA,NA,1),
#        lwd=c(NA,NA,NA,1),
#        col=c('blue','red','forestgreen','black'),
#        cex=1.5,bty='n',pt.cex=c(1.7,1.7,1.3,NA))
#
# # abline(v=as.Date(mean(extract(pheHLS,bb,buffer=45)[[1]]),origin='2017-12-31'),lty=5)
# # abline(v=as.Date(mean(extract(phePn,bb,buffer=45)[[1]]),origin='2017-12-31'),lty=5)
# dev.off()
