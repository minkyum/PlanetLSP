library(sp)
library(raster)
library(terra)
library(sf)

library(rjson)
library(geojsonR)

library(doMC)
library(doParallel)

###############################
args <- commandArgs()
print(args)

numSite <- as.numeric(args[3])
# numSite <- 53


###############################
params <- fromJSON(file='/usr3/graduate/mkmoon/GitHub/PlanetLSP/data_paper/PLSP_Parameters.json')
source(params$setup$rFunctions)


########################################
strSite <- list.dirs(params$setup$outDir,full.names=F,recursive=F)[numSite]
print(strSite)

imgDir <- paste0(params$setup$outDir,strSite,'/mosaic')
print(imgDir)



###############################
dfiles <- list.files(path=imgDir,pattern=glob2rx('*mosaic.tif'))
files  <- list.files(path=imgDir,pattern=glob2rx('*mosaic.tif'),full.names=T)

# Dates
yy <- substr(dfiles,3,4)
mm <- substr(dfiles,5,6)
dd <- substr(dfiles,7,8)
dates <- as.Date(paste(mm,'/',dd,'/',yy,sep=''),'%m/%d/%y')

print(length(dates))


# Divide in to chunks
imgBase <- raster(paste0(params$setup$outDir,strSite,'/base_image.tif'))

numCk <- params$setup$numChunks
chunk <- length(imgBase)%/%numCk

# ckDir <- paste0(params$setup$outDir,strSite,'/chunk')
ckDir <- paste0('/projectnb/modislc/users/mkmoon/Planet/rawImage/chunks/',strSite)
if (!dir.exists(ckDir)) {dir.create(ckDir)}

# ckDirTemp <- paste0(params$setup$outDir,strSite,'/chunk/temp')
ckDirTemp <- paste0('/projectnb/modislc/users/mkmoon/Planet/rawImage/chunks/',strSite,'/temp')
if (!dir.exists(ckDirTemp)) {dir.create(ckDirTemp)}



###############################
# Save chunks as temporal files
registerDoMC(params$setup$numCores)

foreach(i=1:length(dates)) %dopar%{
  band1 <- values(raster(files[i],1))
  band2 <- values(raster(files[i],2))
  band3 <- values(raster(files[i],3))
  band4 <- values(raster(files[i],4))
  
  foreach(cc=1:numCk) %dopar%{
    ckNum <- sprintf('%03d',cc)
    dirTemp <- paste0(ckDirTemp,'/',ckNum)
    if (!dir.exists(dirTemp)) {dir.create(dirTemp)}
    
    if(cc==numCk){
      chunks <- c((chunk*(cc-1)+1):length(imgBase))
    }else{
      chunks <- c((chunk*(cc-1)+1):(chunk*cc))
    }
    b1 <- band1[chunks]
    b2 <- band2[chunks]
    b3 <- band3[chunks]
    b4 <- band4[chunks]  
    
    save(b1,b2,b3,b4,file=paste0(dirTemp,'/',yy[i],mm[i],dd[i],'.rda'))
  }
}


###############################
# Load, merge, and save
foreach(cc=1:numCk) %dopar%{
  ckNum <- sprintf('%03d',cc)
  dirTemp <- paste0(ckDirTemp,'/',ckNum)
  files <- list.files(dirTemp,full.names=T)
  
  if(cc==numCk){
    chunks <- c((chunk*(cc-1)+1):length(imgBase))
  }else{
    chunks <- c((chunk*(cc-1)+1):(chunk*cc))
  }
  
  if(length(files)==length(dates)){
    band1 <- matrix(NA,length(chunks),length(dates))
    band2 <- matrix(NA,length(chunks),length(dates))
    band3 <- matrix(NA,length(chunks),length(dates))
    band4 <- matrix(NA,length(chunks),length(dates))
    for(i in 1:length(dates)){
      load(files[i])
    
      band1[,i] <- b1
      band2[,i] <- b2
      band3[,i] <- b3
      band4[,i] <- b4
    }
    # Save
    save(band1,band2,band3,band4,dates,
         file=paste0(ckDir,'/chunk_',ckNum,'.rda'))
  }else{
    print('No good!')
  }
}
  
  
########################################
## Remove temporary files
system(paste0('rm -r ',ckDirTemp))



