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


vgt <- c('DB','MF','EN','AG','GR','SH')


# ###############################
# lDates    <- vector('list',length(vgt))
# limgBase  <- vector('list',length(vgt))
# leviStack <- vector('list',length(vgt))
# lredStack <- vector('list',length(vgt))
# lnirStack <- vector('list',length(vgt))

registerDoMC()

# for(i in 1:length(vgt)){
#   imgDir <- paste0('/projectnb/modislc/users/mkmoon/Planet/data/Planet_SR/',vgt[i],'/files')
# 
#   eviStack <- saveEVI2stack(imgDir)
# 
#   dates <- eviStack$dates
#   imgBase <- eviStack$imgBase
#   eviStack <- eviStack$eviStack
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
# 
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
  eviPointT <- eviFull[substr(datesFull,1,4)=='2019']
  dates     <- datesFull[substr(datesFull,1,4)=='2019']
  
  eviPoint <- eviPointT[!is.na(eviPointT)]
  dates    <- dates[!is.na(eviPointT)]
  gws      <- dates[as.numeric(substr(dates,6,7))>3&as.numeric(substr(dates,6,7))<10]
  
  if(sum(!is.na(eviPoint))>50 & max(diff(gws))<30){
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

setwd(paste0('/projectnb/modislc/users/mkmoon/Planet/phe/',vgt[ss]))
cc <- sprintf('%02d',cc)
save(pheme,file=paste0('phePlanet_',vgt[ss],'_',cc,'_1.rda'))


################################
rastPP <- vector('list',10)
rastHH <- vector('list',10)
valPP <- vector('list',7)
valHH <- vector('list',7)
statPhe <- matrix(NA,6,7*3)
datQtil <- vector('list',6)

datEVImax <- vector('list',6)
datEVIare <- vector('list',6)

for(ss in 1:6){
  vgt <- c('DB','MF','EN','AG','GR','SH')
  path <- paste0('/projectnb/modislc/users/mkmoon/Planet/phe/',vgt[ss])
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

  # par(mfrow=c(1,3))
  # plot(rastSOS);plot(rastEOS);plot(rastEMP)
  # hist(rastSOS);hist(rastEOS);hist(rastEMP)
  #
  # setwd(paste0('/projectnb/modislc/users/mkmoon/Planet/phe/',vgt[ss]))
  # writeRaster(rastSOS,filename=paste0('phe_Planet_2019_sos_',vgt[ss],'.tif'),format="GTiff",overwrite=T)
  # writeRaster(rastEOS,filename=paste0('phe_Planet_2019_eos_',vgt[ss],'.tif'),format="GTiff",overwrite=T)
  # writeRaster(rastEMP,filename=paste0('phe_Planet_2019_amp_',vgt[ss],'.tif'),format="GTiff",overwrite=T)

  ################################
  tiles <- c('17SQD','18TYN','19TEL','16TCK','13TEF','10TGP')
  tile <- tiles[ss]
  fileHLS <- paste0('/projectnb/modislc/projects/landsat_sentinel/MSLSP_HLS30/',tile,'/phenoMetrics/MSLSP_',tile,'_2019.nc')

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
  
  # EVImax & EVIarea
  set.seed(456123)
  sam <- sample(1:length(rastPP[[9]]),100000)
  x11  <- values(rastPP[[9]])[sam]*10000
  y11  <- values(rastHH[[9]])[sam]
  x21  <- values(rastPP[[10]])[sam]*100
  y21  <- values(rastHH[[10]])[sam]
  
  datEVImax[[ss]] <- cbind(x11,y11)
  datEVIare[[ss]] <- cbind(x21,y21)

  # # Quantile
  # datHHPP <- matrix(NA,70000,12)
  # bined   <- c(0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95)
  # k <- 1
  # for(rr in 1:7){
  #   matHH <- as.matrix(rastHH[[rr]])
  #   matPP <- as.matrix(rastPP[[rr]])
  #   for(i in 1:100){
  #     for(j in 1:100){
  #       temphh <- matHH[(10*(i-1)+1):(10*i),(10*(j-1)+1):(10*j)]
  #       temppp <- matPP[(10*(i-1)+1):(10*i),(10*(j-1)+1):(10*j)]
  # 
  #       datHHPP[k,1] <- median(temphh,na.rm=T)
  #       for(jj in 2:12){
  #         datHHPP[k,jj] <- quantile(temppp,bined[jj-1],na.rm=T)
  #       }
  #       k <- k + 1
  #     }
  #   }
  # }
  # datQtil[[ss]] <- datHHPP
  # #


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
 print(ss)
}

# setwd('/projectnb/modislc/users/mkmoon/Planet/data/')
# write.csv(statPhe,file='phe_1to1.csv')

# plot
setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
png(filename='1to1_phe.png',width=12,height=8,unit='in',res=300)

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

  reg <- round(cor(x1,y1,use='na.or.complete'),3)
  rmse <- round(sqrt(mean((x1-y1)^2,na.rm=T)),2)
  bias <- round(mean(x1-y1,na.rm=T),2)

  text(220,100,expression(paste(italic(r),' =',sep='')),cex=1.8,pos=4)
  text(260,100,reg,cex=1.8,pos=4)
  text(220,70,paste('RMSE = ',rmse,sep=''),cex=1.8,pos=4)
  text(220,40,paste('Bias = ',bias,sep=''),cex=1.8,pos=4)
  text(220,10,'n = 100000',cex=1.8,pos=4)
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


# plot
setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
png(filename='stat_1to1_phe.png',width=13,height=8.5,unit='in',res=300)

par(fig=c(0,0.318,0,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0))
plot(c(5,seq(10,90,10),95),statPhedb[,1],pch=1,cex=1.3,
     xlim=c(0,100),ylim=c(0.84,1),type='o',lwd=2,axe=F,ann=F)
box(lty=1)
axis(1,c(5,25,50,75,95),cex.axis=1.3)
axis(2,seq(0.80,1,0.05),cex.axis=1.3)
mtext('Quantile (%)',1,2.5,cex=1.5)
mtext('Correlation',2,2.5,cex=1.5)

legend('bottomleft',c('DB','MF','EN','AG','GR','SH'),pch=1:6,cex=1.3,bty='n',lwd=2)

points(c(5,seq(10,90,10),95),statPhemf[,1],type='o',pch=2,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),statPheen[,1],type='o',pch=3,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),statPheag[,1],type='o',pch=4,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),statPhegr[,1],type='o',pch=5,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),statPhesh[,1],type='o',pch=6,cex=1.3,lwd=2)

par(fig=c(0.342,0.679,0,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0),new=T)
plot(c(5,seq(10,90,10),95),statPhedb[,2],pch=1,cex=1.3,
     xlim=c(0,100),ylim=c(8,50),type='o',lwd=2,axe=F,ann=F)
box(lty=1)
axis(1,c(5,25,50,75,95),cex.axis=1.3)
axis(2,seq(0,50,10),cex.axis=1.3)
mtext('Quantile (%)',1,2.5,cex=1.5)
mtext('RMSE',2,2.5,cex=1.5)

points(c(5,seq(10,90,10),95),statPhemf[,2],type='o',pch=2,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),statPheen[,2],type='o',pch=3,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),statPheag[,2],type='o',pch=4,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),statPhegr[,2],type='o',pch=5,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),statPhesh[,2],type='o',pch=6,cex=1.3,lwd=2)

par(fig=c(0.681,1,0,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0),new=T)
plot(c(5,seq(10,90,10),95),statPhedb[,3],pch=1,cex=1.3,
     xlim=c(0,100),ylim=c(-23,23),type='o',lwd=2,axe=F,ann=F)
box(lty=1)
axis(1,c(5,25,50,75,95),cex.axis=1.3)
axis(2,seq(-30,30,5),cex.axis=1.3)
mtext('Quantile (%)',1,2.5,cex=1.5)
mtext('Bias',2,2.5,cex=1.5)

points(c(5,seq(10,90,10),95),statPhemf[,3],type='o',pch=2,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),statPheen[,3],type='o',pch=3,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),statPheag[,3],type='o',pch=4,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),statPhegr[,3],type='o',pch=5,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),statPhesh[,3],type='o',pch=6,cex=1.3,lwd=2)
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

dateGcc <- matrix(NA,length(sites),2)
path <- '/projectnb/modislc/users/mkmoon/Planet/data/PhenoCam'
for(ss in 1:length(sites)){
  sstr <- paste0(sites[ss],'*3day_transition_dates.csv')
  filename <- list.files(path=path,pattern=glob2rx(sstr),full.names=T,recursive=T)
  dat <- read.csv(filename,skip=16)

  dat1 <- dat[dat$direction=='rising'&dat$gcc_value=='gcc_90',8]
  dat2 <- dat[dat$direction=='falling'&dat$gcc_value=='gcc_90',8]

  # print(dat1[length(dat1)])
  # print(dat2[1])

  dateGcc[ss,1] <- as.numeric(strftime(as.Date(dat1[length(dat1)]), format = "%j"))
  dateGcc[ss,2] <- as.numeric(strftime(as.Date(dat2[1]), format = "%j"))
}

datePP <- matrix(NA,length(sites),2)
dateHH <- matrix(NA,length(sites),2)
rastPP <- vector('list',8)
rastHH <- vector('list',8)
for(ss in 1:6){
  vgt <- c('DB','MF','EN','AG','GR','SH')
  path <- paste0('/projectnb/modislc/users/mkmoon/Planet/phe/',vgt[ss])
  filePhe <- list.files(path,pattern=glob2rx('phe*.rda'),full.names=T)

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
  fileHLS <- paste0('/projectnb/modislc/projects/landsat_sentinel/MSLSP_HLS30/',tile,'/phenoMetrics/MSLSP_',tile,'_2019.nc')
  nc <- nc_open(fileHLS)
  var <- names(nc[['var']])
  for(rr in c(2,6)){
    pheHLS <- raster(fileHLS,varname=var[(rr+2)])
    rastHH[[rr]] <- projectRaster(pheHLS,rastPP[[1]],method='ngb')
  }

  shpPoints <- readOGR(paste('/projectnb/modislc/users/mkmoon/Planet/shp/',tile,'_pts_1.shp',sep=''))

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
  print(ss)
}


#
setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
png(filename='phe_1to1_phenocam.png',width=12,height=6,unit='in',res=300)

par(mfrow=c(1,2),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0))
plot(dateGcc[1,1],datePP[1,1],xlim=c(65,360),ylim=c(65,360),col='red',pch=1,
     cex=1.5,axe=F,ann=F)
box(lty=1)
axis(1,seq(0,400,50),cex.axis=1.5)
axis(2,seq(0,400,50),cex.axis=1.5)
mtext('PhenoCam Dates (DOY)',1,2.6,cex=1.5)
mtext('Planet Dates (DOY)',2,2.6,cex=1.5)
abline(0,1,lty=5)
legend(100,365,vgt,pch=1:6,bty='n',cex=1.5)

for(i in 2:10){
  if(i<=3){
    j <- 2
  }else if(i==4){
    j <- 3
  }else if(i>4&i<9){
    j <- 4
  }else if(i==9){
    j <- 5
  }else{
    j <- 6
  }
  points(dateGcc[i,1],datePP[i,1],col='red',pch=j,cex=1.5)
}
for(i in 1:10){
  if(i==1){
    j <- 1
  }else if(i>1&i<4){
    j <- 2
  }else if(i==4){
    j <- 3
  }else if(i>4&i<9){
    j <- 4
  }else if(i==9){
    j <- 5
  }else{
    j <- 6
  }
  points(dateGcc[i,2],datePP[i,2],col='blue',pch=j,cex=1.5)
}

x1 <- c(dateGcc[,1],dateGcc[,2])
y1 <- c(datePP[,1],datePP[,2])

reg <- round(cor(x1,y1,use='na.or.complete'),3)
rmse <- round(sqrt(mean((x1-y1)^2,na.rm=T)),2)
bias <- round(mean(x1-y1,na.rm=T),2)
# nn <- sum(!is.na(x1))

text(220,120,expression(paste(italic(r),' =',sep='')),cex=1.5,pos=4)
text(250,120,reg,cex=1.5,pos=4)
text(220,100,paste('RMSE = ',rmse,sep=''),cex=1.5,pos=4)
text(220,80,paste('Bias = ',bias,sep=''),cex=1.5,pos=4)

plot(dateGcc[1,1],dateHH[1,1],xlim=c(65,360),ylim=c(65,360),col='red',pch=1,
     cex=1.5,axe=F,ann=F)
box(lty=1)
axis(1,seq(0,400,50),cex.axis=1.5)
axis(2,seq(0,400,50),cex.axis=1.5)
mtext('PhenoCam Dates (DOY)',1,2.6,cex=1.5)
mtext('HLS Dates (DOY)',2,2.6,cex=1.5)
abline(0,1,lty=5)

for(i in 2:10){
  if(i<=3){
    j <- 2
  }else if(i==4){
    j <- 3
  }else if(i>4&i<9){
    j <- 4
  }else if(i==9){
    j <- 5
  }else{
    j <- 6
  }
  points(dateGcc[i,1],dateHH[i,1],col='red',pch=j,cex=1.5)
}
for(i in 1:10){
  if(i==1){
    j <- 1
  }else if(i>1&i<4){
    j <- 2
  }else if(i==4){
    j <- 3
  }else if(i>4&i<9){
    j <- 4
  }else if(i==9){
    j <- 5
  }else{
    j <- 6
  }
  points(dateGcc[i,2],dateHH[i,2],col='blue',pch=j,cex=1.5)
}

x1 <- c(dateGcc[,1],dateGcc[,2])
y1 <- c(dateHH[,1],dateHH[,2])

reg <- round(cor(x1,y1,use='na.or.complete'),3)
rmse <- round(sqrt(mean((x1-y1)^2,na.rm=T)),2)
bias <- round(mean(x1-y1,na.rm=T),2)
# nn <- sum(!is.na(x1))

text(220,120,expression(paste(italic(r),' =',sep='')),cex=1.5,pos=4)
text(250,120,reg,cex=1.5,pos=4)
text(220,100,paste('RMSE = ',rmse,sep=''),cex=1.5,pos=4)
text(220,80,paste('Bias = ',bias,sep=''),cex=1.5,pos=4)

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
