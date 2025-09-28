library(raster)
library(rgdal)
library(gdalUtils)

par(mfrow=c(2,3))
for(tt in 4:6){
  if(tt==1){
    path <- '/projectnb/modislc/projects/landsat_sentinel/v1_4/HLS30/18TYN/images'  
  }else if(tt==2){
    path <- '/projectnb/modislc/projects/landsat_sentinel/v1_4/HLS30/19TEL/images'  
  }else if(tt==3){
    path <- '/projectnb/modislc/projects/landsat_sentinel/v1_4/HLS30/13TEF/images'  
  }else if(tt==4){
    path <- '/projectnb/modislc/users/mkmoon/Planet/data/mslsp/input/HLS30/10TGP/images'  
  }else if(tt==5){
    path <- '/projectnb/modislc/users/mkmoon/Planet/data/mslsp/input/HLS30/16TCK/images'  
  }else{
    path <- '/projectnb/modislc/users/mkmoon/Planet/data/mslsp/input/HLS30/17SQD/images'  
  }
  
  sstr <- 'HLS*.2019*hdf'
  files <- list.files(path,pattern=glob2rx(sstr),full.names=T)
  
  sza <- NULL
  for(i in 1:length(files)){
    if(substr(files[i],74,76)=='L30' |substr(files[i],80,82)=='L30'){
      aa <- gdalinfo(files[i])[19]  
      sza[i] <- as.numeric(substr(aa,25,30))
    }else{
      aa <- gdalinfo(files[i])[20]
      sza[i] <- as.numeric(substr(aa,30,35))
    }
    # print(aa)
    if(i%%30==0) print(paste(i,aa))
  }
  plot(sza)
}


