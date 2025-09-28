
datePP <- matrix(NA,length(sites)*3,2)
rastPP <- vector('list',8)
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
    
    tiles <- c('17SQD','18TYN','19TEL','16TCK','13TEF','10TGP')
    tile <- tiles[ss]
    shpPoints <- readOGR(paste('/projectnb/modislc/users/mkmoon/Planet/shp/',tile,'_pts_1.shp',sep=''))
    
    if(yy==2019){
      if(ss==1){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[1,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[3]),na.rm=T)
        datePP[1,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[3]),na.rm=T)
      }else if(ss==2){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[2,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[1]),na.rm=T)
        datePP[2,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[1]),na.rm=T)
        datePP[3,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[2]),na.rm=T)
        datePP[3,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[2]),na.rm=T)
        
      }else if(ss==3){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[4,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[3]),na.rm=T)
        datePP[4,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[3]),na.rm=T)
        
      }else if(ss==4){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[5,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[1]),na.rm=T)
        datePP[5,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[1]),na.rm=T)
        datePP[6,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[2]),na.rm=T)
        datePP[6,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[2]),na.rm=T)
        datePP[7,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[3]),na.rm=T)
        datePP[7,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[3]),na.rm=T)
        datePP[8,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[4]),na.rm=T)
        datePP[8,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[4]),na.rm=T)
        
      }else if(ss==5){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[9,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[3]),na.rm=T)
        datePP[9,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[3]),na.rm=T)
        
      }else{
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[10,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[4]),na.rm=T)
        datePP[10,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[4]),na.rm=T)
        
      }  
    }else if(yy==2018){
      if(ss==1){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[1+10,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[3]),na.rm=T)
        datePP[1+10,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[3]),na.rm=T)
        
      }else if(ss==2){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[2+10,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[1]),na.rm=T)
        datePP[2+10,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[1]),na.rm=T)
        datePP[3+10,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[2]),na.rm=T)
        datePP[3+10,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[2]),na.rm=T)
        
      }else if(ss==3){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[4+10,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[3]),na.rm=T)
        datePP[4+10,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[3]),na.rm=T)
        
      }else if(ss==4){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[5+10,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[1]),na.rm=T)
        datePP[5+10,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[1]),na.rm=T)
        datePP[6+10,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[2]),na.rm=T)
        datePP[6+10,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[2]),na.rm=T)
        datePP[7+10,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[3]),na.rm=T)
        datePP[7+10,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[3]),na.rm=T)
        datePP[8+10,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[4]),na.rm=T)
        datePP[8+10,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[4]),na.rm=T)
        
      }else if(ss==5){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[9+10,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[3]),na.rm=T)
        datePP[9+10,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[3]),na.rm=T)
        
      }else{
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[10+10,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[4]),na.rm=T)
        datePP[10+10,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[4]),na.rm=T)
        
      }
    }else{
      if(ss==1){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[1+20,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[3]),na.rm=T)
        datePP[1+20,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[3]),na.rm=T)
        
      }else if(ss==2){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[2+20,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[1]),na.rm=T)
        datePP[2+20,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[1]),na.rm=T)
        datePP[3+20,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[2]),na.rm=T)
        datePP[3+20,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[2]),na.rm=T)
        
      }else if(ss==3){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[4+20,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[3]),na.rm=T)
        datePP[4+20,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[3]),na.rm=T)
        
      }else if(ss==4){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[5+20,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[1]),na.rm=T)
        datePP[5+20,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[1]),na.rm=T)
        datePP[6+20,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[2]),na.rm=T)
        datePP[6+20,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[2]),na.rm=T)
        datePP[7+20,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[3]),na.rm=T)
        datePP[7+20,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[3]),na.rm=T)
        datePP[8+20,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[4]),na.rm=T)
        datePP[8+20,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[4]),na.rm=T)
        
      }else if(ss==5){
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[9+20,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[3]),na.rm=T)
        datePP[9+20,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[3]),na.rm=T)
        
      }else{
        shp <- spTransform(shpPoints,crs(rastPP[[2]]))
        datePP[10+20,1] <- median(unlist(extract(rastPP[[2]],shp,buffer=4.5)[4]),na.rm=T)
        datePP[10+20,2] <- median(unlist(extract(rastPP[[6]],shp,buffer=4.5)[4]),na.rm=T)
        
      }
    }
    print(ss)
  }  
}


setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
png(filename='phe_1to1_pp_medi.png',width=7,height=6,unit='in',res=300)

par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0))

plot(aa[,1],datePP[,1],xlim=c(45,360),ylim=c(45,360),cex=2,pch=5,axe=F,ann=F)
box(lty=1)
axis(1,seq(0,400,50),cex.axis=1.2)
axis(2,seq(0,400,50),cex.axis=1.2)
mtext('Planet median (DOY)',1,2.6,cex=1.5)
mtext('Planet one pixel (DOY)',2,2.6,cex=1.5)
abline(0,1,lty=5)

points(aa[,2],datePP[,2],cex=2)

# points(aa[30,1],datePP[30,1],cex=2,pch=19)
# points(aa[10,2],datePP[10,2],cex=2,pch=17)

dev.off()