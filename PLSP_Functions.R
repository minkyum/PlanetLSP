###############################

##
saveEVI2stack <- function(imgDir){
  
  path <- imgDir
  
  # Dates
  dfileDN <- list.files(path=path,pattern=glob2rx('*udm2*.tif'))
  
  yy <- substr(dfileDN,3,4)
  mm <- substr(dfileDN,5,6)
  dd <- substr(dfileDN,7,8)
  dates_all <- as.Date(paste(mm,'/',dd,'/',yy,sep=''),'%m/%d/%y')
  dates <- unique(dates_all)
  
  # Full file list
  fileDN <- list.files(path=path,pattern=glob2rx('*udm2*.tif'),full.names=T)
  fileSR <- NULL
  for(i in 1:length(fileDN)){
    fileSR[i] <- list.files(path=path,pattern=glob2rx(paste0(substr(dfileDN[i],1,17),'*SR*.tif')),full.names=T)
  }
  
  # fileSR <- fileSR[yy==as.character(year)]
  # fileDN <- fileDN[yy==as.character(year)]
  # 
  # dates_all <- dates_all[yy==as.character(year)]
  # dates <- unique(dates_all)
  
  imgFull <- NULL
  for(i in 1:length(dates)){
    ids <- which(dates_all==dates[i])  
    if(length(ids)==1){
      imgFull <- i
    }
  }
  imgBase <- raster(fileSR[which(dates_all==dates[imgFull])])  
  
  bandStack <- foreach(i=1:length(dates),.combine='cbind') %dopar% {
    
    ids <- which(dates_all==dates[i])
    
    if(length(ids)>1){
      temp1 <- vector('list',length(ids))
      temp2 <- vector('list',length(ids))
      temp3 <- vector('list',length(ids))
      temp4 <- vector('list',length(ids))
      temp5 <- vector('list',length(ids))
      for(j in 1:length(ids)){
        b1 <- raster(fileSR[ids[j]],band=1)/10000
        b2 <- raster(fileSR[ids[j]],band=2)/10000
        b3 <- raster(fileSR[ids[j]],band=3)/10000
        b4 <- raster(fileSR[ids[j]],band=4)/10000
        
        u2 <- raster(fileDN[ids[j]],band=2)
        u3 <- raster(fileDN[ids[j]],band=3)
        u4 <- raster(fileDN[ids[j]],band=4)
        u5 <- raster(fileDN[ids[j]],band=5)
        u6 <- raster(fileDN[ids[j]],band=6)
        u8 <- raster(fileDN[ids[j]],band=8)
        
        b1[u3==1|u4==1|u5==1|u6==1|u8>0] <- NA
        b2[u3==1|u4==1|u5==1|u6==1|u8>0] <- NA
        b3[u3==1|u4==1|u5==1|u6==1|u8>0] <- NA
        b4[u3==1|u4==1|u5==1|u6==1|u8>0] <- NA
        
        b1[b1 < 0] <- NA
        b2[b2 < 0] <- NA
        b3[b3 < 0] <- NA
        b4[b4 < 0] <- NA
        
        temp1[[j]] <- b1
        temp2[[j]] <- b2
        temp3[[j]] <- b3
        temp4[[j]] <- b4
        temp5[[j]] <- u2
      }
      
      for(rr in 1:length(ids)){
        log <- try(compareRaster(temp1[[rr]],imgBase,extent=F,rowcol=F),
                   silent=T)
        if(inherits(log,'try-error')){
          temp1[[rr]] <- projectRaster(temp1[[rr]],imgBase)    
        }
        
        log <- try(compareRaster(temp2[[rr]],imgBase,extent=F,rowcol=F),
                   silent=T)
        if(inherits(log,'try-error')){
          temp2[[rr]] <- projectRaster(temp2[[rr]],imgBase)    
        }
        
        log <- try(compareRaster(temp3[[rr]],imgBase,extent=F,rowcol=F),
                   silent=T)
        if(inherits(log,'try-error')){
          temp3[[rr]] <- projectRaster(temp3[[rr]],imgBase)    
        }
        
        log <- try(compareRaster(temp4[[rr]],imgBase,extent=F,rowcol=F),
                   silent=T)
        if(inherits(log,'try-error')){
          temp4[[rr]] <- projectRaster(temp4[[rr]],imgBase)    
        }
        
        log <- try(compareRaster(temp5[[rr]],imgBase,extent=F,rowcol=F),
                   silent=T)
        if(inherits(log,'try-error')){
          temp5[[rr]] <- projectRaster(temp5[[rr]],imgBase)    
        }
        
      }
      temp1$fun <- mean; temp2$fun <- mean; temp3$fun <- mean; temp4$fun <- mean; temp5$fun <- max
      temp1$na.rm <- T; temp2$na.rm <- T; temp3$na.rm <- T; temp4$na.rm <- T; temp5$na.rm <- T
      rast1 <- do.call(mosaic,temp1)  
      rast2 <- do.call(mosaic,temp2)  
      rast3 <- do.call(mosaic,temp3)  
      rast4 <- do.call(mosaic,temp4)  
      rast5 <- do.call(mosaic,temp5)  
      
    }else{
      b1 <- raster(fileSR[ids],band=1)/10000
      b2 <- raster(fileSR[ids],band=2)/10000
      b3 <- raster(fileSR[ids],band=3)/10000
      b4 <- raster(fileSR[ids],band=4)/10000
      
      u2 <- raster(fileDN[ids],band=2)
      u3 <- raster(fileDN[ids],band=3)
      u4 <- raster(fileDN[ids],band=4)
      u5 <- raster(fileDN[ids],band=5)
      u6 <- raster(fileDN[ids],band=6)
      u8 <- raster(fileDN[ids],band=8)
      
      b1[u3==1|u4==1|u5==1|u6==1|u8>0] <- NA
      b2[u3==1|u4==1|u5==1|u6==1|u8>0] <- NA
      b3[u3==1|u4==1|u5==1|u6==1|u8>0] <- NA
      b4[u3==1|u4==1|u5==1|u6==1|u8>0] <- NA
      
      b1[b1 < 0] <- NA
      b2[b2 < 0] <- NA
      b3[b3 < 0] <- NA
      b4[b4 < 0] <- NA
      
      rast1 <- b1
      rast2 <- b2
      rast3 <- b3
      rast4 <- b4
      rast5 <- u2
     
    }
    
    if(length(rast1)==length(imgBase)){
      cbind(values(rast1),values(rast2),values(rast3),values(rast4),values(rast5))  
    }else{
      cbind(rep(NA,length(imgBase)),rep(NA,length(imgBase)),
            rep(NA,length(imgBase)),rep(NA,length(imgBase)),rep(NA,length(imgBase)))
    }
  }
  
  b1 <- bandStack[,seq(1,5*length(dates),5)]
  b2 <- bandStack[,seq(2,5*length(dates),5)]
  b3 <- bandStack[,seq(3,5*length(dates),5)]
  b4 <- bandStack[,seq(4,5*length(dates),5)]
  snowPix <- bandStack[,seq(5,5*length(dates),5)]
  
  
  return(list(b1=b1,b2=b2,b3=b3,b4=b4,snowPix=snowPix,
              dates=dates,imgBase=imgBase))
}

##
saveNIRstack <- function(imgDir){
  
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
  
  # fileSR <- fileSR[yy==as.character(year)]
  # fileDN <- fileDN[yy==as.character(year)]
  # 
  # dates_all <- dates_all[yy==as.character(year)]
  # dates <- unique(dates_all)
  
  imgFull <- NULL
  for(i in 1:length(dates)){
    ids <- which(dates_all==dates[i])  
    if(length(ids)==1){
      imgFull <- i
    }
  }
  imgBase <- raster(fileSR[which(dates_all==dates[imgFull])])  
  
  nirStack <- foreach(i=1:length(dates),.combine='c') %dopar% {
    
    ids <- which(dates_all==dates[i])
    
    if(length(ids)>1){
      temp <- vector('list',length(ids))
      for(j in 1:length(ids)){
        # red <- raster(fileSR[ids[j]],band=3)/10000
        nir <- raster(fileSR[ids[j]],band=4)
        udm <- raster(fileDN[ids[j]])
        # vis <- 2.5*(nir-red)/(nir+2.4*red+1)
        vis <- nir
        
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
      # red <- raster(fileSR[ids],band=3)/10000
      nir <- raster(fileSR[ids],band=4)
      udm <- raster(fileDN[ids])
      # vis <- 2.5*(nir-red)/(nir+2.4*red+1)
      vis <- nir
      
      vis[udm>2|is.na(udm)] <- NA
      rast <- vis
    }
  }
  
  return(list(nirStack=nirStack,dates=dates,imgBase=imgBase))
}

##
saveREDstack <- function(imgDir){
  
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
  
  # fileSR <- fileSR[yy==as.character(year)]
  # fileDN <- fileDN[yy==as.character(year)]
  # 
  # dates_all <- dates_all[yy==as.character(year)]
  # dates <- unique(dates_all)
  
  imgFull <- NULL
  for(i in 1:length(dates)){
    ids <- which(dates_all==dates[i])  
    if(length(ids)==1){
      imgFull <- i
    }
  }
  imgBase <- raster(fileSR[which(dates_all==dates[imgFull])])  
  
  redStack <- foreach(i=1:length(dates),.combine='c') %dopar% {
    
    ids <- which(dates_all==dates[i])
    
    if(length(ids)>1){
      temp <- vector('list',length(ids))
      for(j in 1:length(ids)){
        red <- raster(fileSR[ids[j]],band=3)
        # nir <- raster(fileSR[ids[j]],band=4)/10000
        udm <- raster(fileDN[ids[j]])
        # vis <- 2.5*(nir-red)/(nir+2.4*red+1)
        vis <- red
        
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
      red <- raster(fileSR[ids],band=3)
      # nir <- raster(fileSR[ids],band=4)/10000
      udm <- raster(fileDN[ids])
      # vis <- 2.5*(nir-red)/(nir+2.4*red+1)
      vis <- red
      
      vis[udm>2|is.na(udm)] <- NA
      rast <- vis
    }
  }
  
  return(list(redStack=redStack,dates=dates,imgBase=imgBase))
}

  
##
runPhenologyPlanet <- function(imgDir){
  
  eviStack <- saveEVI2stack(imgDir)
  
  dates <- eviStack$dates
  imgBase <- eviStack$imgBase
  eviStack <- eviStack$eviStack
  
  pheMat <- foreach(j=1:length(eviStack[[1]]),.combine=cbind) %dopar% {
    
    eviPoint <- matrix(NA,length(eviStack),1)
    for(i in 1:length(eviStack)){
      eviPoint[i,1] <- eviStack[[i]][j]
    }
    
    
    if(sum(!is.na(eviPoint))>50 & max(diff(dates[!is.na(eviPoint)]))<30){
      bgevi <- quantile(eviPoint[which(as.numeric(substr(as.character(dates),6,7))<3|
                                         as.numeric(substr(as.character(dates),6,7))>10
      )],0.9,na.rm=T)
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
   
  pheRast <- setValues(imgBase,pheme)
  
  return(pheRast)

}

##
# extractTS <- function(shpPoints,datStack,id){
#   
#   imgBase <- datStack$imgBase
#   
#   nest <-as.character(id)
#   shpPoints <- shpPoints[as.character(shpPoints$id) %in% nest,]
#   shpPoints <- spTransform(shpPoints,crs(imgBase))
#   
#   pixNum <- setValues(imgBase,1:length(imgBase))
#   bb <- spTransform(shpPoints,crs(imgBase))
#   pixid <- extract(pixNum,bb)
#   
#   red <- datStack$b3[pixid,]    
#   nir <- datStack$b4[pixid,]    
#   
#   vi    <- 2.5*(nir - red) / (nir + 2.4*red + 1)
#   
#   return(vi)  
# }

extractTS <- function(shpPoints,datStack,imgBase,id=NULL,rad=NULL){
  
  if(!is.null(id)){
    nest <-as.character(id)
    shpPoints <- shpPoints[as.character(shpPoints$id) %in% nest,]
  }
  
  bb <- spTransform(shpPoints,crs(imgBase))
  temp <- extract(datStack[[1]],bb,buffer=rad)
  
  vi <- matrix(NA,length(temp[[1]]),length(datStack))
  for(i in 1:length(datStack)){
    vi[,i] <- unlist(extract(datStack[[i]],bb,buffer=rad))
  }
  
  return(vi)  
}

#---------------------------------------------------------------------
#Calculate pheno metrics for each pixel
#Code adapted by Minkyu Moon  
#---------------------------------------------------------------------
DoPhenologyPlanet <- function(blue, green, red, nir, snow, dates, phenYrs, params){
  
  log <- try({    
    
    vi    <- 2.5*(nir - red) / (nir + 2.4*red + 1)
    ndsi  <- (green-nir)/(green+nir)
    
    pheno_pars <- params$phenology_parameters
    qa_pars    <- params$qa_parameters
    
    #filters
    spikes <- CheckSpike_MultiBand(blue, red, vi, dates, pheno_pars)
    vi[spikes] <- NA
    vi[vi<0]   <- NA
    
    
    vi_dorm <- max(c(0.12,quantile(vi,probs=pheno_pars$dormantQuantile,na.rm=T)))   #Calc vi dormant value
    
    snow[is.na(snow)] <- 1
    snowPix <- Screen_SnowFills(vi,vi_dorm,snow,dates,pheno_pars)
    vi[snowPix] <- vi_dorm
    
    vi[ndsi > -0.1] <- vi_dorm
    vi[nir > 0.2 & red > 0.15] <- vi_dorm
    
    weights <- rep(1,length(dates))
    weights[snowPix==1] <- 0.5
    weights[ndsi> -0.1] <- 0.5
    weights[nir> 0.7] <- 0.5
    
    #
    numDaysFit  <-  365 + (pheno_pars$splineBuffer * 2)
    daysVec <- 1:numDaysFit
    inYear <- daysVec > pheno_pars$splineBuffer & daysVec <= (pheno_pars$splineBuffer+365)
    
    splineStart <- as.Date(as.Date(paste0(phenYrs,'-01-01')) - pheno_pars$splineBuffer) 
    splineEnd <- as.Date(as.Date(paste0(phenYrs,'-12-31')) + pheno_pars$splineBuffer) 
    pred_dates <- seq(splineStart, splineEnd, by="day")
    
    dateRange <- dates >= splineStart & dates <= splineEnd & !is.na(vi)   
    
    dateSub <- dates[dateRange]
    viSub <- vi[dateRange] 
    
    weights <- weights[dateRange]
    
    smoothed_vi <- Smooth_VI(viSub, dateSub, pred_dates, weights, pheno_pars, vi_dorm)
    
    
    #Fit phenology
    peaks <- FindPeaks(smoothed_vi)
    if (all(is.na(peaks))) {pheme <- rep(NA,11);next}
    
    #Find full segments
    full_segs <- GetSegs(peaks, smoothed_vi, pheno_pars)
    if (is.null(full_segs)) {pheme <- rep(NA,11);next}
    
    #Only keep segments with peaks within year *****
    full_segs <- full_segs[inYear[sapply(full_segs, "[[", 2)] ]  #check if peaks are in the year
    if (length(full_segs)==0) {pheme <- rep(NA,11);next}
    
    #Get PhenoDates
    pheno_dates <- GetPhenoDates(full_segs, smoothed_vi, pred_dates, pheno_pars)
    phen <- unlist(pheno_dates, use.names=F)
    phen <- phen - as.numeric(as.Date(paste0((as.numeric(phenYrs)-1),'-12-31')))
    if (all(is.na(phen))) {pheme <- rep(NA,11);next}
    
    #EVI layers
    seg_metrics <- lapply(full_segs, GetSegMetrics,smoothed_vi,viSub,pred_dates,dateSub) #full segment metrics
    un <- unlist(seg_metrics, use.names=F)
    ln <- length(un)
    seg_amp <- un[seq(1, ln, by=9)] * 10000
    seg_max <- un[seq(2, ln, by=9)] * 10000
    seg_int <- un[seq(3, ln, by=9)] * 100  
    gup_rsq <- un[seq(4, ln, by=9)] * 10000
    gup_numObs <- un[seq(5, ln, by=9)]
    gup_maxgap_frac <- un[seq(6, ln, by=9)] * 100
    gdown_rsq <- un[seq(7, ln, by=9)] * 10000
    gdown_numObs <- un[seq(8, ln, by=9)]
    gdown_maxgap_frac <- un[seq(9, ln, by=9)] * 100
    
    # if (gup_maxgap_frac > 30 | gdown_maxgap_frac > 30) {pheme <- rep(NA,11);next}
    
    
    
    
    numRecords <- length(seg_amp)  #how many cycles were recorded
    naCheck <- is.na(seg_amp)
    numCyc <- sum(naCheck == 0)  #how many cycles have good data (seg metrics has valid observations)
    
    if(numCyc == 0){pheme <- rep(NA,11);next}
    
    if(numRecords == 1) {
      # pheme <- c(phen,seg_amp,seg_max,seg_int,numCyc)  
      pheme <- c(phen,seg_amp,seg_max,seg_int,numCyc)
    }else{
      theOrd <- order(seg_amp,decreasing=T)   
      phen1 <- phen[seq(theOrd[1], length(phen), by = numRecords)]
      # pheme <- c(phen1,seg_max[theOrd[1]], seg_amp[theOrd[1]], seg_int[theOrd[1]],numCyc)  
      pheme <- c(phen1,seg_max[theOrd[1]], seg_amp[theOrd[1]], seg_int[theOrd[1]],numCyc)
    }
    
  },silent=TRUE)
  if(inherits(log, "try-error")){pheme <- rep(NA,11)}

  return(pheme)

}

