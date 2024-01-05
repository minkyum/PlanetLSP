#Load required libraries
library(raster)
library(rgdal)
library(gdalUtils)
library(rgeos)
library(foreach)
library(iterators)
library(doMC)

library(matrixStats)
library(WGCNA)
library(zoo)

library(RcppRoll)

library(rjson)

ss <- 1

tiles <- c('17SQD','18TYN','19TEL','16TCK','13TEF','10TGP')
vgt <- c('db','mf','en','ag','gr','sh')

for(ss in 1:6){
  #Inputs
  ############
  
  #Tile 
  tile <- tiles[ss]
  codeVersion <- 'V1'      #run V0 or V1?
  
  numCores <- 8
  
  imgYrs <- 2016:2019
  phenYrs <- 2016:2019
  
  #What data folder
  chunkBase <- '/projectnb/modislc/projects/landsat_sentinel/MSLSP_HLS30/'
  imgBase <-   '/projectnb/modislc/projects/landsat_sentinel/v1_4/HLS30/'
  
  
  #Define and create output directory
  outFolder <- '/projectnb/modislc/projects/landsat_sentinel/MSLSP_assessment/plots/'
  
  
  #Define path to function
  functions_V0 <- '/usr2/postdoc/dbolt/CodeFolder/git_repos/MuSLI_LSP/AWS/r/MuSLI_LSP_Functions.r'
  functions_V1 <- '/usr3/graduate/mkmoon/GitHub/MSLSP/MSLSP_Functions.r'
  functions_diagnostics <- "/usr3/graduate/mkmoon/GitHub/MSLSP/Development/MSLSP_Diagnostic_Functions_V1_0.r"
  
  #Name of shapefile. Must be in same projection as the tile. Must have "id" column
  shpName <- paste0('/projectnb/modislc/projects/landsat_sentinel/MSLSP_assessment/shps/',tile,'_pts.shp')
  # shpName <- paste0('/projectnb/modislc/users/mkmoon/Planet/shp/',tile,'_pts_1.shp')
  shpName <- paste0('/projectnb/modislc/users/mkmoon/Planet/shp/rp_',vgt[ss],'_1000.shp')
  
  #Json file where phenology parameters are defined. Will just use the phenology paramaters from this 
  jsonFile <- "/usr3/graduate/mkmoon/GitHub/MSLSP/MSLSP_Parameters.json"
  
  
  showObservations <- T
  showSpline <- T
  showPhenDates <- F
  showFilledData <- F
  showDespiked <- F
  showSnow <- F
  yrsToPlot <- 2016:2019
  
  #Get default parameters
  params <- fromJSON(file=jsonFile)
  
  
  #Alter the jsonFile?
  params$phenology_parameters$splineSpar=0.3      #Alter the smoothing parameter
  params$phenology_parameters$min_seg_amplitude=0.03    #Set minimum seg amplitude
  #params$phenology_parameters$vegetation_index='ndvi_re1'  #Alter the vegetation index (Find list of vegetation indices in MSLSP_Functions.r file: CalcIndex function)
  
  
  
  
  registerDoMC(cores=numCores)
  
  #Load functions 
  if (codeVersion=='V0') {source(file=functions_V0)}
  if (codeVersion=='V1') {source(file=functions_V1)}
  source(file=functions_diagnostics)
  
  
  
  
  
  
  #Where is the data?
  imgDir <- paste0(imgBase,tile,'/images/')
  chunkDir <- paste0(chunkBase,tile,'/imageChunks/')
  
  
  
  
  theTable <- Extract_Timeseries(tile, imgDir, chunkDir, imgYrs, phenYrs, numCores, params, shpName=shpName,codeVersion=codeVersion) 
  
  
  if(ss==1){
    hlsDB <- theTable  
  }else if(ss==2){
    hlsMF <- theTable  
  }else if(ss==3){
    hlsEN <- theTable
  }else if(ss==4){
    hlsAG <- theTable  
  }else if(ss==5){
    hlsGR <- theTable  
  }else{
    hlsSH <- theTable  
  }
  
  print(ss)  
}


t1HH <- matrix(NA,6,12)
for(ss in 1:6){
  
  t1h <- matrix(NA,1000,12)
  for(i in 1:1000){
    
    if(ss==1){
      hlsDB[[i]]$dates[is.na(hlsDB[[i]]$original_VI)] <- NA
      temp <- na.omit(hlsDB[[i]]$dates)
    }else if(ss==2){
      hlsMF[[i]]$dates[is.na(hlsMF[[i]]$original_VI)] <- NA
      temp <- na.omit(hlsMF[[i]]$dates)
    }else if(ss==3){
      if(i==106){
        hlsEN[[105]]$dates[is.na(hlsEN[[105]]$original_VI)] <- NA
        temp <- na.omit(hlsEN[[105]]$dates)  
      }else{
        hlsEN[[i]]$dates[is.na(hlsEN[[i]]$original_VI)] <- NA
        temp <- na.omit(hlsEN[[i]]$dates)
      }
    }else if(ss==4){
      hlsAG[[i]]$dates[is.na(hlsAG[[i]]$original_VI)] <- NA
      temp <- na.omit(hlsAG[[i]]$dates)
    }else if(ss==5){
      hlsGR[[i]]$dates[is.na(hlsGR[[i]]$original_VI)] <- NA
      temp <- na.omit(hlsGR[[i]]$dates)
    }else{
      hlsSH[[i]]$dates[is.na(hlsSH[[i]]$original_VI)] <- NA
      temp <- na.omit(hlsSH[[i]]$dates)
    }
    
    temp1 <- temp[which(substr(temp,1,4)=='2017')]
    temp1 <- temp1[which(as.numeric(substr(temp1,6,7))>2&as.numeric(substr(temp1,6,7))<11)]
    temp2 <- temp[which(substr(temp,1,4)=='2018')]
    temp2 <- temp2[which(as.numeric(substr(temp2,6,7))>2&as.numeric(substr(temp2,6,7))<11)]
    temp3 <- temp[which(substr(temp,1,4)=='2019')]
    temp3 <- temp3[which(as.numeric(substr(temp3,6,7))>2&as.numeric(substr(temp3,6,7))<11)]
    temp4 <- c(temp1,temp2,temp3)
    
    t1h[i,1] <- mean(diff(temp1))
    t1h[i,2] <- median(diff(temp1))
    t1h[i,3] <- max(diff(temp1))
    t1h[i,4] <- mean(diff(temp2))
    t1h[i,5] <- median(diff(temp2))
    t1h[i,6] <- max(diff(temp2))
    t1h[i,7] <- mean(diff(temp3))
    t1h[i,8] <- median(diff(temp3))
    t1h[i,9] <- max(diff(temp3))
    t1h[i,10] <- mean(diff(temp4))
    t1h[i,11] <- median(diff(temp4))
    t1h[i,12] <- max(diff(temp4))
  }
  
  t1HH[ss,] <- apply(t1h,2,mean,na.rm=T)
  
  print(ss)
}


####################
##
source('/usr3/graduate/mkmoon/GitHub/PlanetLSP/PLSP_Functions.R')

load('/projectnb/modislc/users/mkmoon/Planet/data/planet_eviStack.rda')

# Time series
t1PP <- matrix(NA,6,12)
for(ss in 1:6){
  shpPoints <- readOGR(paste0('/projectnb/modislc/users/mkmoon/Planet/shp/rp_',vgt[ss],'_1000.shp'))
  
  t1p <- matrix(NA,1000,12)
  for(i in 1:1000){
    
    dd <- lDates[[ss]]
    datPTS <- extractTS(shpPoints,leviStack[[ss]],limgBase[[ss]],id=(i-1),rad=5)[1,]
    dd[is.na(datPTS)] <- NA
    temp <- na.omit(dd)
    
    temp1 <- temp[which(substr(temp,1,4)=='2017')]
    temp1 <- temp1[which(as.numeric(substr(temp1,6,7))>2&as.numeric(substr(temp1,6,7))<11)]
    temp2 <- temp[which(substr(temp,1,4)=='2018')]
    temp2 <- temp2[which(as.numeric(substr(temp2,6,7))>2&as.numeric(substr(temp2,6,7))<11)]
    temp3 <- temp[which(substr(temp,1,4)=='2019')]
    temp3 <- temp3[which(as.numeric(substr(temp3,6,7))>2&as.numeric(substr(temp3,6,7))<11)]
    temp4 <- temp[which(substr(temp,1,4)!='2020')]
    temp4 <- temp4[which(as.numeric(substr(temp4,6,7))>2&as.numeric(substr(temp4,6,7))<11)]
    
    t1p[i,1] <- mean(diff(temp1))
    t1p[i,2] <- median(diff(temp1))
    t1p[i,3] <- max(diff(temp1))
    t1p[i,4] <- mean(diff(temp2))
    t1p[i,5] <- median(diff(temp2))
    t1p[i,6] <- max(diff(temp2))
    t1p[i,7] <- mean(diff(temp3))
    t1p[i,8] <- median(diff(temp3))
    t1p[i,9] <- max(diff(temp3))
    t1p[i,10] <- mean(diff(temp4))
    t1p[i,11] <- median(diff(temp4))
    t1p[i,12] <- max(diff(temp4))
  }
  
  t1PP[ss,] <- apply(t1p,2,mean,na.rm=T)
  
  print(ss)
}


##################
setwd('/projectnb/modislc/users/mkmoon/Planet/data/')
write.csv(t1PP,file='t1p_gws_1000.csv')
write.csv(t1HH,file='t1h_gws_1000.csv')


