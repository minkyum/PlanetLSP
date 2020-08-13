###############################
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

  return(eviStack)
}
  
  

