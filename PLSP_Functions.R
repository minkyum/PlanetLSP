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
