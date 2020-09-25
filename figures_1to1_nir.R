library(raster)
library(rgdal)
library(gdalUtils)
library(ncdf4)
library(signal)
library(RColorBrewer)
library(doMC)
library(scales)

source('/usr3/graduate/mkmoon/GitHub/PlanetLSP/PLSP_Functions.R')

###########################################
## 1 to 1
# NIR

load('/projectnb/modislc/users/mkmoon/Planet/data/planet_nirStack.rda')
vgt <- c('db','mf','en','ag','gr','sh')

ptsPP05 <- vector('list',length(vgt))
ptsPP10 <- vector('list',length(vgt))
ptsPP20 <- vector('list',length(vgt))
ptsPP30 <- vector('list',length(vgt))
ptsPP40 <- vector('list',length(vgt))
ptsPP50 <- vector('list',length(vgt))
ptsPP60 <- vector('list',length(vgt))
ptsPP70 <- vector('list',length(vgt))
ptsPP80 <- vector('list',length(vgt))
ptsPP80 <- vector('list',length(vgt))
ptsPP90 <- vector('list',length(vgt))
ptsPP95 <- vector('list',length(vgt))
ptsHH <- vector('list',length(vgt))
for(ss in 1:6){
  shpPoints <- readOGR(paste0('/projectnb/modislc/users/mkmoon/Planet/shp/rp_',vgt[ss],'_1000.shp'))
  load(paste0('/projectnb/modislc/users/mkmoon/Planet/data/rp_',vgt[ss],'_hls_1000.rda'))
  
  ids <- as.numeric(substr(names(theTable),7,9))
  
  nirStack <- lnirStack[[ss]]
  imgBase  <- limgBase[[ss]]
  dates    <- lDates[[ss]]
  
  ptsPLA05 <- vector('list',length(ids))
  ptsPLA10 <- vector('list',length(ids))
  ptsPLA20 <- vector('list',length(ids))
  ptsPLA30 <- vector('list',length(ids))
  ptsPLA40 <- vector('list',length(ids))
  ptsPLA50 <- vector('list',length(ids))
  ptsPLA60 <- vector('list',length(ids))
  ptsPLA70 <- vector('list',length(ids))
  ptsPLA80 <- vector('list',length(ids))
  ptsPLA90 <- vector('list',length(ids))
  ptsPLA95 <- vector('list',length(ids))
  ptsHLS <- vector('list',length(ids))
  for(i in 1:length(ids)){
    datPTSag <- extractTS(shpPoints,nirStack,imgBase,id=ids[i],rad=15)
    hlsPT <- theTable[[i]]
    
    if(sum(!is.na(hlsPT))==8){
      td17 <- hlsPT[["y2017"]][["filled_dates"]]
      td18 <- hlsPT[["y2018"]][["filled_dates"]]
      td19 <- hlsPT[["y2019"]][["filled_dates"]]
      dd17 <- td17[substr(td17,1,4)=='2017']
      dd18 <- td18[substr(td18,1,4)=='2018']
      dd19 <- td19[substr(td19,1,4)=='2019']
      
      b517 <- hlsPT[["y2017"]][["bands"]][["filled_b5"]][substr(td17,1,4)=='2017']
      b518 <- hlsPT[["y2018"]][["bands"]][["filled_b5"]][substr(td18,1,4)=='2018']
      b519 <- hlsPT[["y2019"]][["bands"]][["filled_b5"]][substr(td19,1,4)=='2019']
      
      # b417 <- hlsPT[["y2017"]][["bands"]][["filled_b4"]][substr(td17,1,4)=='2017']
      # b418 <- hlsPT[["y2018"]][["bands"]][["filled_b4"]][substr(td18,1,4)=='2018']
      # b419 <- hlsPT[["y2019"]][["bands"]][["filled_b4"]][substr(td19,1,4)=='2019']
      
      dHLS <- c(dd17,dd18,dd19)
      b5HLS <- c(b517,b518,b519)
      # b4HLS <- c(b417,b418,b419)
      
      datesHLS <- dHLS[order(dHLS)]
      b5HLS    <- b5HLS[order(dHLS)]
      # b4HLS    <- b4HLS[order(dHLS)]
      
      ptsP <- NULL
      ptsH <- NULL
      k <- 1
      for(j in 1:length(dates)){
        if(length(which(dates[j]==datesHLS))==1){
          ptsP[k] <- j
          ptsH[k] <- which(dates[j]==datesHLS)
          k      <- k+1
        }
      }
      ptsPLA05[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.05,na.rm=T)
      ptsPLA10[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.10,na.rm=T)
      ptsPLA20[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.20,na.rm=T)
      ptsPLA30[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.30,na.rm=T)
      ptsPLA40[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.40,na.rm=T)
      ptsPLA50[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.50,na.rm=T)
      ptsPLA60[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.60,na.rm=T)
      ptsPLA70[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.70,na.rm=T)
      ptsPLA80[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.80,na.rm=T)
      ptsPLA90[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.90,na.rm=T)
      ptsPLA95[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.95,na.rm=T)
      ptsHLS[[i]]   <- b5HLS[ptsH]
    }
  }
  
  ptsPP05[[ss]] <- unlist(ptsPLA05)
  ptsPP10[[ss]] <- unlist(ptsPLA10)
  ptsPP20[[ss]] <- unlist(ptsPLA20)
  ptsPP30[[ss]] <- unlist(ptsPLA30)
  ptsPP40[[ss]] <- unlist(ptsPLA40)
  ptsPP50[[ss]] <- unlist(ptsPLA50)
  ptsPP60[[ss]] <- unlist(ptsPLA60)
  ptsPP70[[ss]] <- unlist(ptsPLA70)
  ptsPP80[[ss]] <- unlist(ptsPLA80)
  ptsPP90[[ss]] <- unlist(ptsPLA90)
  ptsPP95[[ss]] <- unlist(ptsPLA95)
  ptsHH[[ss]] <- unlist(ptsHLS)
  
  print(ss)
}

stat1to1db <- matrix(NA,11,5)
stat1to1mf <- matrix(NA,11,5)
stat1to1en <- matrix(NA,11,5)
stat1to1ag <- matrix(NA,11,5)
stat1to1gr <- matrix(NA,11,5)
stat1to1sh <- matrix(NA,11,5)

stat1to1db[1,1] <- cor(ptsPP05[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[1,2] <- sqrt(mean((ptsPP05[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[1,3] <- mean(ptsPP05[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[2,1] <- cor(ptsPP10[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[2,2] <- sqrt(mean((ptsPP10[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[2,3] <- mean(ptsPP10[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[3,1] <- cor(ptsPP20[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[3,2] <- sqrt(mean((ptsPP20[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[3,3] <- mean(ptsPP20[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[4,1] <- cor(ptsPP30[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[4,2] <- sqrt(mean((ptsPP30[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[4,3] <- mean(ptsPP30[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[5,1] <- cor(ptsPP40[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[5,2] <- sqrt(mean((ptsPP40[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[5,3] <- mean(ptsPP40[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[6,1] <- cor(ptsPP50[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[6,2] <- sqrt(mean((ptsPP50[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[6,3] <- mean(ptsPP50[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[7,1] <- cor(ptsPP60[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[7,2] <- sqrt(mean((ptsPP60[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[7,3] <- mean(ptsPP60[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[8,1] <- cor(ptsPP70[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[8,2] <- sqrt(mean((ptsPP70[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[8,3] <- mean(ptsPP70[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[9,1] <- cor(ptsPP80[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[9,2] <- sqrt(mean((ptsPP80[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[9,3] <- mean(ptsPP80[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[10,1] <- cor(ptsPP90[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[10,2] <- sqrt(mean((ptsPP90[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[10,3] <- mean(ptsPP90[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[11,1] <- cor(ptsPP95[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[11,2] <- sqrt(mean((ptsPP95[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[11,3] <- mean(ptsPP95[[1]]-ptsHH[[1]],na.rm=T)

stat1to1mf[1,1] <- cor(ptsPP05[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[1,2] <- sqrt(mean((ptsPP05[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[1,3] <- mean(ptsPP05[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[2,1] <- cor(ptsPP10[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[2,2] <- sqrt(mean((ptsPP10[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[2,3] <- mean(ptsPP10[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[3,1] <- cor(ptsPP20[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[3,2] <- sqrt(mean((ptsPP20[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[3,3] <- mean(ptsPP20[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[4,1] <- cor(ptsPP30[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[4,2] <- sqrt(mean((ptsPP30[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[4,3] <- mean(ptsPP30[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[5,1] <- cor(ptsPP40[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[5,2] <- sqrt(mean((ptsPP40[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[5,3] <- mean(ptsPP40[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[6,1] <- cor(ptsPP50[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[6,2] <- sqrt(mean((ptsPP50[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[6,3] <- mean(ptsPP50[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[7,1] <- cor(ptsPP60[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[7,2] <- sqrt(mean((ptsPP60[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[7,3] <- mean(ptsPP60[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[8,1] <- cor(ptsPP70[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[8,2] <- sqrt(mean((ptsPP70[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[8,3] <- mean(ptsPP70[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[9,1] <- cor(ptsPP80[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[9,2] <- sqrt(mean((ptsPP80[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[9,3] <- mean(ptsPP80[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[10,1] <- cor(ptsPP90[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[10,2] <- sqrt(mean((ptsPP90[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[10,3] <- mean(ptsPP90[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[11,1] <- cor(ptsPP95[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[11,2] <- sqrt(mean((ptsPP95[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[11,3] <- mean(ptsPP95[[2]]-ptsHH[[2]],na.rm=T)

stat1to1en[1,1] <- cor(ptsPP05[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[1,2] <- sqrt(mean((ptsPP05[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[1,3] <- mean(ptsPP05[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[2,1] <- cor(ptsPP10[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[2,2] <- sqrt(mean((ptsPP10[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[2,3] <- mean(ptsPP10[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[3,1] <- cor(ptsPP20[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[3,2] <- sqrt(mean((ptsPP20[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[3,3] <- mean(ptsPP20[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[4,1] <- cor(ptsPP30[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[4,2] <- sqrt(mean((ptsPP30[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[4,3] <- mean(ptsPP30[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[5,1] <- cor(ptsPP40[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[5,2] <- sqrt(mean((ptsPP40[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[5,3] <- mean(ptsPP40[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[6,1] <- cor(ptsPP50[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[6,2] <- sqrt(mean((ptsPP50[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[6,3] <- mean(ptsPP50[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[7,1] <- cor(ptsPP60[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[7,2] <- sqrt(mean((ptsPP60[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[7,3] <- mean(ptsPP60[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[8,1] <- cor(ptsPP70[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[8,2] <- sqrt(mean((ptsPP70[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[8,3] <- mean(ptsPP70[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[9,1] <- cor(ptsPP80[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[9,2] <- sqrt(mean((ptsPP80[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[9,3] <- mean(ptsPP80[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[10,1] <- cor(ptsPP90[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[10,2] <- sqrt(mean((ptsPP90[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[10,3] <- mean(ptsPP90[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[11,1] <- cor(ptsPP95[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[11,2] <- sqrt(mean((ptsPP95[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[11,3] <- mean(ptsPP95[[3]]-ptsHH[[3]],na.rm=T)

stat1to1ag[1,1] <- cor(ptsPP05[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[1,2] <- sqrt(mean((ptsPP05[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[1,3] <- mean(ptsPP05[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[2,1] <- cor(ptsPP10[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[2,2] <- sqrt(mean((ptsPP10[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[2,3] <- mean(ptsPP10[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[3,1] <- cor(ptsPP20[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[3,2] <- sqrt(mean((ptsPP20[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[3,3] <- mean(ptsPP20[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[4,1] <- cor(ptsPP30[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[4,2] <- sqrt(mean((ptsPP30[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[4,3] <- mean(ptsPP30[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[5,1] <- cor(ptsPP40[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[5,2] <- sqrt(mean((ptsPP40[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[5,3] <- mean(ptsPP40[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[6,1] <- cor(ptsPP50[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[6,2] <- sqrt(mean((ptsPP50[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[6,3] <- mean(ptsPP50[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[7,1] <- cor(ptsPP60[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[7,2] <- sqrt(mean((ptsPP60[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[7,3] <- mean(ptsPP60[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[8,1] <- cor(ptsPP70[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[8,2] <- sqrt(mean((ptsPP70[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[8,3] <- mean(ptsPP70[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[9,1] <- cor(ptsPP80[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[9,2] <- sqrt(mean((ptsPP80[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[9,3] <- mean(ptsPP80[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[10,1] <- cor(ptsPP90[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[10,2] <- sqrt(mean((ptsPP90[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[10,3] <- mean(ptsPP90[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[11,1] <- cor(ptsPP95[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[11,2] <- sqrt(mean((ptsPP95[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[11,3] <- mean(ptsPP95[[4]]-ptsHH[[4]],na.rm=T)

stat1to1gr[1,1] <- cor(ptsPP05[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[1,2] <- sqrt(mean((ptsPP05[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[1,3] <- mean(ptsPP05[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[2,1] <- cor(ptsPP10[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[2,2] <- sqrt(mean((ptsPP10[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[2,3] <- mean(ptsPP10[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[3,1] <- cor(ptsPP20[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[3,2] <- sqrt(mean((ptsPP20[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[3,3] <- mean(ptsPP20[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[4,1] <- cor(ptsPP30[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[4,2] <- sqrt(mean((ptsPP30[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[4,3] <- mean(ptsPP30[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[5,1] <- cor(ptsPP40[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[5,2] <- sqrt(mean((ptsPP40[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[5,3] <- mean(ptsPP40[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[6,1] <- cor(ptsPP50[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[6,2] <- sqrt(mean((ptsPP50[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[6,3] <- mean(ptsPP50[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[7,1] <- cor(ptsPP60[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[7,2] <- sqrt(mean((ptsPP60[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[7,3] <- mean(ptsPP60[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[8,1] <- cor(ptsPP70[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[8,2] <- sqrt(mean((ptsPP70[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[8,3] <- mean(ptsPP70[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[9,1] <- cor(ptsPP80[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[9,2] <- sqrt(mean((ptsPP80[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[9,3] <- mean(ptsPP80[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[10,1] <- cor(ptsPP90[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[10,2] <- sqrt(mean((ptsPP90[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[10,3] <- mean(ptsPP90[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[11,1] <- cor(ptsPP95[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[11,2] <- sqrt(mean((ptsPP95[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[11,3] <- mean(ptsPP95[[5]]-ptsHH[[5]],na.rm=T)

stat1to1sh[1,1] <- cor(ptsPP05[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[1,2] <- sqrt(mean((ptsPP05[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[1,3] <- mean(ptsPP05[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[2,1] <- cor(ptsPP10[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[2,2] <- sqrt(mean((ptsPP10[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[2,3] <- mean(ptsPP10[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[3,1] <- cor(ptsPP20[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[3,2] <- sqrt(mean((ptsPP20[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[3,3] <- mean(ptsPP20[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[4,1] <- cor(ptsPP30[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[4,2] <- sqrt(mean((ptsPP30[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[4,3] <- mean(ptsPP30[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[5,1] <- cor(ptsPP40[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[5,2] <- sqrt(mean((ptsPP40[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[5,3] <- mean(ptsPP40[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[6,1] <- cor(ptsPP50[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[6,2] <- sqrt(mean((ptsPP50[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[6,3] <- mean(ptsPP50[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[7,1] <- cor(ptsPP60[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[7,2] <- sqrt(mean((ptsPP60[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[7,3] <- mean(ptsPP60[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[8,1] <- cor(ptsPP70[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[8,2] <- sqrt(mean((ptsPP70[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[8,3] <- mean(ptsPP70[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[9,1] <- cor(ptsPP80[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[9,2] <- sqrt(mean((ptsPP80[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[9,3] <- mean(ptsPP80[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[10,1] <- cor(ptsPP90[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[10,2] <- sqrt(mean((ptsPP90[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[10,3] <- mean(ptsPP90[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[11,1] <- cor(ptsPP95[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[11,2] <- sqrt(mean((ptsPP95[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[11,3] <- mean(ptsPP95[[6]]-ptsHH[[6]],na.rm=T)


# Plot
setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
png(filename='1to1_nir.png',width=12,height=8,unit='in',res=300)

par(mfrow=c(2,3),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0))
spec <- rev(brewer.pal(11,'Spectral'))
mycolRamp = colorRampPalette(c('White',spec))
for(ss in 1:6){
  x1 <- ptsPP50[[ss]]
  y1 <- ptsHH[[ss]]

  # if(ss<5){
    limx <- c(0,8000)
    limy <- c(0,8000)
  # }else if(ss==5){
  #   limx <- c(0,0.4)
  #   limy <- c(0,0.4)
  # }else{
  #   limx <- c(0,0.25)
  #   limy <- c(0,0.25)
  # }
  smoothScatter(x1,y1,xlim=limx,ylim=limy,
                colramp=mycolRamp,nbin=1000,nrpoints=0,
                transformation=function(x)x^.6,axe=F,
                xlab='Planet NIR (x 0.0001)',ylab='HLS NIR (x 0.0001)',
                cex.lab=1.7)
  # if(ss<5){
    axis(1,at=seq(0,10000,2000),cex.axis=1.7)
    axis(2,at=seq(0,10000,2000),cex.axis=1.7)
  # }else if(ss==5){
  #   axis(1,at=seq(0,1,0.2),cex.axis=1.5)
  #   axis(2,at=seq(0,1,0.2),cex.axis=1.5)
  # }else{
  #   axis(1,at=seq(0,1,0.1),cex.axis=1.5)
  #   axis(2,at=seq(0,1,0.1),cex.axis=1.5)
  # }
  abline(0,1,lty=5)
  
  
  reg <- round(cor(x1,y1,use='na.or.complete'),3)
  rmse <- round(sqrt(mean((x1-y1)^2,na.rm=T)))
  bias <- round(mean(x1-y1,na.rm=T))
  # nn <- sum(!is.na(x1))
  # abline(lm(y1~x1),col='red',lwd=2)
  
  text(1100,7700,expression(paste(italic(r),' =',sep='')),cex=1.8,pos=4)
  text(1900,7700,reg,cex=1.8,pos=4)
  text(1100,7100,paste('RMSE = ',rmse,sep=''),cex=1.8,pos=4)
  text(1100,6500,paste('Bias = ',bias,sep=''),cex=1.8,pos=4)
  text(1100,5900,'n = 100000',cex=1.8,pos=4)
  
}

dev.off()





##################################################
# Red
source('/usr3/graduate/mkmoon/GitHub/PlanetLSP/PLSP_Functions.R')

load('/projectnb/modislc/users/mkmoon/Planet/data/planet_redStack.rda')
vgt <- c('db','mf','en','ag','gr','sh')

ptsPP05 <- vector('list',length(vgt))
ptsPP10 <- vector('list',length(vgt))
ptsPP20 <- vector('list',length(vgt))
ptsPP30 <- vector('list',length(vgt))
ptsPP40 <- vector('list',length(vgt))
ptsPP50 <- vector('list',length(vgt))
ptsPP60 <- vector('list',length(vgt))
ptsPP70 <- vector('list',length(vgt))
ptsPP80 <- vector('list',length(vgt))
ptsPP80 <- vector('list',length(vgt))
ptsPP90 <- vector('list',length(vgt))
ptsPP95 <- vector('list',length(vgt))
ptsHH <- vector('list',length(vgt))
for(ss in 1:6){
  shpPoints <- readOGR(paste0('/projectnb/modislc/users/mkmoon/Planet/shp/rp_',vgt[ss],'_1000.shp'))
  load(paste0('/projectnb/modislc/users/mkmoon/Planet/data/rp_',vgt[ss],'_hls_1000.rda'))
  
  ids <- as.numeric(substr(names(theTable),7,9))
  
  redStack <- lredStack[[ss]]
  imgBase  <- limgBase[[ss]]
  dates    <- lDates[[ss]]
  
  ptsPLA05 <- vector('list',length(ids))
  ptsPLA10 <- vector('list',length(ids))
  ptsPLA20 <- vector('list',length(ids))
  ptsPLA30 <- vector('list',length(ids))
  ptsPLA40 <- vector('list',length(ids))
  ptsPLA50 <- vector('list',length(ids))
  ptsPLA60 <- vector('list',length(ids))
  ptsPLA70 <- vector('list',length(ids))
  ptsPLA80 <- vector('list',length(ids))
  ptsPLA90 <- vector('list',length(ids))
  ptsPLA95 <- vector('list',length(ids))
  ptsHLS <- vector('list',length(ids))
  for(i in 1:length(ids)){
    datPTSag <- extractTS(shpPoints,redStack,imgBase,id=ids[i],rad=15)
    hlsPT <- theTable[[i]]
    
    if(sum(!is.na(hlsPT))==8){
      td17 <- hlsPT[["y2017"]][["filled_dates"]]
      td18 <- hlsPT[["y2018"]][["filled_dates"]]
      td19 <- hlsPT[["y2019"]][["filled_dates"]]
      dd17 <- td17[substr(td17,1,4)=='2017']
      dd18 <- td18[substr(td18,1,4)=='2018']
      dd19 <- td19[substr(td19,1,4)=='2019']
      
      # b517 <- hlsPT[["y2017"]][["bands"]][["filled_b5"]][substr(td17,1,4)=='2017']
      # b518 <- hlsPT[["y2018"]][["bands"]][["filled_b5"]][substr(td18,1,4)=='2018']
      # b519 <- hlsPT[["y2019"]][["bands"]][["filled_b5"]][substr(td19,1,4)=='2019']
      
      b417 <- hlsPT[["y2017"]][["bands"]][["filled_b4"]][substr(td17,1,4)=='2017']
      b418 <- hlsPT[["y2018"]][["bands"]][["filled_b4"]][substr(td18,1,4)=='2018']
      b419 <- hlsPT[["y2019"]][["bands"]][["filled_b4"]][substr(td19,1,4)=='2019']
      
      dHLS <- c(dd17,dd18,dd19)
      # b5HLS <- c(b517,b518,b519)
      b4HLS <- c(b417,b418,b419)
      
      datesHLS <- dHLS[order(dHLS)]
      # b5HLS    <- b5HLS[order(dHLS)]
      b4HLS    <- b4HLS[order(dHLS)]
      
      ptsP <- NULL
      ptsH <- NULL
      k <- 1
      for(j in 1:length(dates)){
        if(length(which(dates[j]==datesHLS))==1){
          ptsP[k] <- j
          ptsH[k] <- which(dates[j]==datesHLS)
          k      <- k+1
        }
      }
      ptsPLA05[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.05,na.rm=T)
      ptsPLA10[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.10,na.rm=T)
      ptsPLA20[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.20,na.rm=T)
      ptsPLA30[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.30,na.rm=T)
      ptsPLA40[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.40,na.rm=T)
      ptsPLA50[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.50,na.rm=T)
      ptsPLA60[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.60,na.rm=T)
      ptsPLA70[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.70,na.rm=T)
      ptsPLA80[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.80,na.rm=T)
      ptsPLA90[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.90,na.rm=T)
      ptsPLA95[[i]] <- apply(datPTSag[,ptsP],2,quantile,0.95,na.rm=T)
      # ptsHLS[[i]]   <- b5HLS[ptsH]
      ptsHLS[[i]]   <- b4HLS[ptsH]
    }
  }
  
  ptsPP05[[ss]] <- unlist(ptsPLA05)
  ptsPP10[[ss]] <- unlist(ptsPLA10)
  ptsPP20[[ss]] <- unlist(ptsPLA20)
  ptsPP30[[ss]] <- unlist(ptsPLA30)
  ptsPP40[[ss]] <- unlist(ptsPLA40)
  ptsPP50[[ss]] <- unlist(ptsPLA50)
  ptsPP60[[ss]] <- unlist(ptsPLA60)
  ptsPP70[[ss]] <- unlist(ptsPLA70)
  ptsPP80[[ss]] <- unlist(ptsPLA80)
  ptsPP90[[ss]] <- unlist(ptsPLA90)
  ptsPP95[[ss]] <- unlist(ptsPLA95)
  ptsHH[[ss]] <- unlist(ptsHLS)
  
  print(ss)
}

stat1to1db <- matrix(NA,11,5)
stat1to1mf <- matrix(NA,11,5)
stat1to1en <- matrix(NA,11,5)
stat1to1ag <- matrix(NA,11,5)
stat1to1gr <- matrix(NA,11,5)
stat1to1sh <- matrix(NA,11,5)

stat1to1db[1,1] <- cor(ptsPP05[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[1,2] <- sqrt(mean((ptsPP05[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[1,3] <- mean(ptsPP05[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[2,1] <- cor(ptsPP10[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[2,2] <- sqrt(mean((ptsPP10[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[2,3] <- mean(ptsPP10[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[3,1] <- cor(ptsPP20[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[3,2] <- sqrt(mean((ptsPP20[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[3,3] <- mean(ptsPP20[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[4,1] <- cor(ptsPP30[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[4,2] <- sqrt(mean((ptsPP30[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[4,3] <- mean(ptsPP30[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[5,1] <- cor(ptsPP40[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[5,2] <- sqrt(mean((ptsPP40[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[5,3] <- mean(ptsPP40[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[6,1] <- cor(ptsPP50[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[6,2] <- sqrt(mean((ptsPP50[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[6,3] <- mean(ptsPP50[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[7,1] <- cor(ptsPP60[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[7,2] <- sqrt(mean((ptsPP60[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[7,3] <- mean(ptsPP60[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[8,1] <- cor(ptsPP70[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[8,2] <- sqrt(mean((ptsPP70[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[8,3] <- mean(ptsPP70[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[9,1] <- cor(ptsPP80[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[9,2] <- sqrt(mean((ptsPP80[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[9,3] <- mean(ptsPP80[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[10,1] <- cor(ptsPP90[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[10,2] <- sqrt(mean((ptsPP90[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[10,3] <- mean(ptsPP90[[1]]-ptsHH[[1]],na.rm=T)
stat1to1db[11,1] <- cor(ptsPP95[[1]],ptsHH[[1]],use='na.or.complete')
stat1to1db[11,2] <- sqrt(mean((ptsPP95[[1]]-ptsHH[[1]])^2,na.rm=T))
stat1to1db[11,3] <- mean(ptsPP95[[1]]-ptsHH[[1]],na.rm=T)

stat1to1mf[1,1] <- cor(ptsPP05[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[1,2] <- sqrt(mean((ptsPP05[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[1,3] <- mean(ptsPP05[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[2,1] <- cor(ptsPP10[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[2,2] <- sqrt(mean((ptsPP10[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[2,3] <- mean(ptsPP10[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[3,1] <- cor(ptsPP20[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[3,2] <- sqrt(mean((ptsPP20[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[3,3] <- mean(ptsPP20[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[4,1] <- cor(ptsPP30[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[4,2] <- sqrt(mean((ptsPP30[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[4,3] <- mean(ptsPP30[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[5,1] <- cor(ptsPP40[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[5,2] <- sqrt(mean((ptsPP40[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[5,3] <- mean(ptsPP40[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[6,1] <- cor(ptsPP50[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[6,2] <- sqrt(mean((ptsPP50[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[6,3] <- mean(ptsPP50[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[7,1] <- cor(ptsPP60[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[7,2] <- sqrt(mean((ptsPP60[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[7,3] <- mean(ptsPP60[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[8,1] <- cor(ptsPP70[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[8,2] <- sqrt(mean((ptsPP70[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[8,3] <- mean(ptsPP70[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[9,1] <- cor(ptsPP80[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[9,2] <- sqrt(mean((ptsPP80[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[9,3] <- mean(ptsPP80[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[10,1] <- cor(ptsPP90[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[10,2] <- sqrt(mean((ptsPP90[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[10,3] <- mean(ptsPP90[[2]]-ptsHH[[2]],na.rm=T)
stat1to1mf[11,1] <- cor(ptsPP95[[2]],ptsHH[[2]],use='na.or.complete')
stat1to1mf[11,2] <- sqrt(mean((ptsPP95[[2]]-ptsHH[[2]])^2,na.rm=T))
stat1to1mf[11,3] <- mean(ptsPP95[[2]]-ptsHH[[2]],na.rm=T)

stat1to1en[1,1] <- cor(ptsPP05[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[1,2] <- sqrt(mean((ptsPP05[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[1,3] <- mean(ptsPP05[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[2,1] <- cor(ptsPP10[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[2,2] <- sqrt(mean((ptsPP10[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[2,3] <- mean(ptsPP10[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[3,1] <- cor(ptsPP20[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[3,2] <- sqrt(mean((ptsPP20[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[3,3] <- mean(ptsPP20[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[4,1] <- cor(ptsPP30[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[4,2] <- sqrt(mean((ptsPP30[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[4,3] <- mean(ptsPP30[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[5,1] <- cor(ptsPP40[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[5,2] <- sqrt(mean((ptsPP40[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[5,3] <- mean(ptsPP40[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[6,1] <- cor(ptsPP50[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[6,2] <- sqrt(mean((ptsPP50[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[6,3] <- mean(ptsPP50[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[7,1] <- cor(ptsPP60[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[7,2] <- sqrt(mean((ptsPP60[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[7,3] <- mean(ptsPP60[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[8,1] <- cor(ptsPP70[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[8,2] <- sqrt(mean((ptsPP70[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[8,3] <- mean(ptsPP70[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[9,1] <- cor(ptsPP80[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[9,2] <- sqrt(mean((ptsPP80[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[9,3] <- mean(ptsPP80[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[10,1] <- cor(ptsPP90[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[10,2] <- sqrt(mean((ptsPP90[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[10,3] <- mean(ptsPP90[[3]]-ptsHH[[3]],na.rm=T)
stat1to1en[11,1] <- cor(ptsPP95[[3]],ptsHH[[3]],use='na.or.complete')
stat1to1en[11,2] <- sqrt(mean((ptsPP95[[3]]-ptsHH[[3]])^2,na.rm=T))
stat1to1en[11,3] <- mean(ptsPP95[[3]]-ptsHH[[3]],na.rm=T)

stat1to1ag[1,1] <- cor(ptsPP05[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[1,2] <- sqrt(mean((ptsPP05[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[1,3] <- mean(ptsPP05[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[2,1] <- cor(ptsPP10[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[2,2] <- sqrt(mean((ptsPP10[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[2,3] <- mean(ptsPP10[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[3,1] <- cor(ptsPP20[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[3,2] <- sqrt(mean((ptsPP20[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[3,3] <- mean(ptsPP20[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[4,1] <- cor(ptsPP30[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[4,2] <- sqrt(mean((ptsPP30[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[4,3] <- mean(ptsPP30[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[5,1] <- cor(ptsPP40[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[5,2] <- sqrt(mean((ptsPP40[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[5,3] <- mean(ptsPP40[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[6,1] <- cor(ptsPP50[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[6,2] <- sqrt(mean((ptsPP50[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[6,3] <- mean(ptsPP50[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[7,1] <- cor(ptsPP60[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[7,2] <- sqrt(mean((ptsPP60[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[7,3] <- mean(ptsPP60[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[8,1] <- cor(ptsPP70[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[8,2] <- sqrt(mean((ptsPP70[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[8,3] <- mean(ptsPP70[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[9,1] <- cor(ptsPP80[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[9,2] <- sqrt(mean((ptsPP80[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[9,3] <- mean(ptsPP80[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[10,1] <- cor(ptsPP90[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[10,2] <- sqrt(mean((ptsPP90[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[10,3] <- mean(ptsPP90[[4]]-ptsHH[[4]],na.rm=T)
stat1to1ag[11,1] <- cor(ptsPP95[[4]],ptsHH[[4]],use='na.or.complete')
stat1to1ag[11,2] <- sqrt(mean((ptsPP95[[4]]-ptsHH[[4]])^2,na.rm=T))
stat1to1ag[11,3] <- mean(ptsPP95[[4]]-ptsHH[[4]],na.rm=T)

stat1to1gr[1,1] <- cor(ptsPP05[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[1,2] <- sqrt(mean((ptsPP05[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[1,3] <- mean(ptsPP05[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[2,1] <- cor(ptsPP10[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[2,2] <- sqrt(mean((ptsPP10[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[2,3] <- mean(ptsPP10[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[3,1] <- cor(ptsPP20[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[3,2] <- sqrt(mean((ptsPP20[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[3,3] <- mean(ptsPP20[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[4,1] <- cor(ptsPP30[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[4,2] <- sqrt(mean((ptsPP30[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[4,3] <- mean(ptsPP30[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[5,1] <- cor(ptsPP40[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[5,2] <- sqrt(mean((ptsPP40[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[5,3] <- mean(ptsPP40[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[6,1] <- cor(ptsPP50[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[6,2] <- sqrt(mean((ptsPP50[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[6,3] <- mean(ptsPP50[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[7,1] <- cor(ptsPP60[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[7,2] <- sqrt(mean((ptsPP60[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[7,3] <- mean(ptsPP60[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[8,1] <- cor(ptsPP70[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[8,2] <- sqrt(mean((ptsPP70[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[8,3] <- mean(ptsPP70[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[9,1] <- cor(ptsPP80[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[9,2] <- sqrt(mean((ptsPP80[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[9,3] <- mean(ptsPP80[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[10,1] <- cor(ptsPP90[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[10,2] <- sqrt(mean((ptsPP90[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[10,3] <- mean(ptsPP90[[5]]-ptsHH[[5]],na.rm=T)
stat1to1gr[11,1] <- cor(ptsPP95[[5]],ptsHH[[5]],use='na.or.complete')
stat1to1gr[11,2] <- sqrt(mean((ptsPP95[[5]]-ptsHH[[5]])^2,na.rm=T))
stat1to1gr[11,3] <- mean(ptsPP95[[5]]-ptsHH[[5]],na.rm=T)

stat1to1sh[1,1] <- cor(ptsPP05[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[1,2] <- sqrt(mean((ptsPP05[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[1,3] <- mean(ptsPP05[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[2,1] <- cor(ptsPP10[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[2,2] <- sqrt(mean((ptsPP10[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[2,3] <- mean(ptsPP10[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[3,1] <- cor(ptsPP20[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[3,2] <- sqrt(mean((ptsPP20[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[3,3] <- mean(ptsPP20[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[4,1] <- cor(ptsPP30[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[4,2] <- sqrt(mean((ptsPP30[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[4,3] <- mean(ptsPP30[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[5,1] <- cor(ptsPP40[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[5,2] <- sqrt(mean((ptsPP40[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[5,3] <- mean(ptsPP40[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[6,1] <- cor(ptsPP50[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[6,2] <- sqrt(mean((ptsPP50[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[6,3] <- mean(ptsPP50[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[7,1] <- cor(ptsPP60[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[7,2] <- sqrt(mean((ptsPP60[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[7,3] <- mean(ptsPP60[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[8,1] <- cor(ptsPP70[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[8,2] <- sqrt(mean((ptsPP70[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[8,3] <- mean(ptsPP70[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[9,1] <- cor(ptsPP80[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[9,2] <- sqrt(mean((ptsPP80[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[9,3] <- mean(ptsPP80[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[10,1] <- cor(ptsPP90[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[10,2] <- sqrt(mean((ptsPP90[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[10,3] <- mean(ptsPP90[[6]]-ptsHH[[6]],na.rm=T)
stat1to1sh[11,1] <- cor(ptsPP95[[6]],ptsHH[[6]],use='na.or.complete')
stat1to1sh[11,2] <- sqrt(mean((ptsPP95[[6]]-ptsHH[[6]])^2,na.rm=T))
stat1to1sh[11,3] <- mean(ptsPP95[[6]]-ptsHH[[6]],na.rm=T)


# Plot
png(filename='1to1_red.png',width=12,height=8,unit='in',res=300)

par(mfrow=c(2,3),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0))
spec <- rev(brewer.pal(11,'Spectral'))
mycolRamp = colorRampPalette(c('White',spec))
for(ss in 1:6){
  x1 <- ptsPP50[[ss]]
  y1 <- ptsHH[[ss]]
  
  # if(ss<5){
  limx <- c(0,3000)
  limy <- c(0,3000)
  # }else if(ss==5){
  #   limx <- c(0,0.4)
  #   limy <- c(0,0.4)
  # }else{
  #   limx <- c(0,0.25)
  #   limy <- c(0,0.25)
  # }
  smoothScatter(x1,y1,xlim=limx,ylim=limy,
                colramp=mycolRamp,nbin=1000,nrpoints=0,
                transformation=function(x)x^.6,axe=F,
                xlab='Planet Red (x 0.0001)',ylab='HLS Red (x 0.0001)',
                cex.lab=1.7)
  # if(ss<5){
  axis(1,at=seq(0,10000,1000),cex.axis=1.7)
  axis(2,at=seq(0,10000,1000),cex.axis=1.7)
  # }else if(ss==5){
  #   axis(1,at=seq(0,1,0.2),cex.axis=1.5)
  #   axis(2,at=seq(0,1,0.2),cex.axis=1.5)
  # }else{
  #   axis(1,at=seq(0,1,0.1),cex.axis=1.5)
  #   axis(2,at=seq(0,1,0.1),cex.axis=1.5)
  # }
  abline(0,1,lty=5)
  
  
  reg <- round(cor(x1,y1,use='na.or.complete'),3)
  rmse <- round(sqrt(mean((x1-y1)^2,na.rm=T)))
  bias <- round(mean(x1-y1,na.rm=T))
  # nn <- sum(!is.na(x1))
  # abline(lm(y1~x1),col='red',lwd=2)
  
  text(500,2900,expression(paste(italic(r),' =',sep='')),cex=1.8,pos=4)
  text(900,2900,reg,cex=1.8,pos=4)
  text(500,2700,paste('RMSE = ',rmse,sep=''),cex=1.8,pos=4)
  text(500,2500,paste('Bias = ',bias,sep=''),cex=1.8,pos=4)
  text(500,2300,'n = 100000',cex=1.8,pos=4)
  
}

dev.off()