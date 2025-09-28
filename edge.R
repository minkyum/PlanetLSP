library(raster)
library(rgdal)
library(gdalUtils)
library(ncdf4)
library(signal)
library(RColorBrewer)
library(doMC)

###############################
##
source('/usr3/graduate/mkmoon/GitHub/PlanetLSP/PLSP_Functions.R')


vgt <- c('DB','MF','EN','AG','GR','SH')

load('/projectnb/modislc/users/mkmoon/Planet/data/planet_eviStack.rda')


###############################
# Time series
shpPoints <- readOGR('/projectnb/modislc/users/mkmoon/MuSLI/geo_data/shp/17SQD_pts_edge.shp')

###############################
xlims <- as.Date(c('2018-11-1','2020-02-1'))  #Always use the first of a month! (for labelling purposes)
ylims <-  c(0.05,0.9)

outFolder <- '/projectnb/modislc/users/mkmoon/MuSLI/V1_0/figures/edge/Planet/'
for(ii in 1:25){
  datPTS1 <- extractTS(shpPoints,leviStack[[1]],limgBase[[1]],id=ii,rad=15)
  d2019 <- c(min(which(substr(lDates[[1]],1,7)=='2018-11')):max(which(substr(lDates[[1]],1,7)=='2020-02')))
  
  outDir <- paste0(outFolder,sprintf('%03d',ii),'/')
  if (!dir.exists(outDir)) {dir.create(outDir)}
  
  for(i in 1:dim(datPTS1)[1]){
    output_name = paste0(outDir,'edge_',sprintf('%03d',i),'.png')
    png(output_name,res = 600,width = 6,height = 3,units = "in",pointsize = 8)
    
    par(fig=c(0.02,.98,.02,.98),mai=c(0.33,0.33,.3,.1),mgp=c(1.5,0.2,0))
    plot(lDates[[1]][d2019],datPTS1[i,d2019],
         pch=21,bg='#2c7fb8',
         xlim=xlims,ylim=ylims,tck=0.025,
         xlab='Date',ylab='EVI2',xaxt='n',
         main=paste0('edge_',sprintf('%03d',ii),'_',sprintf('%03d',i)))
    
    axis.Date(1,at=seq(xlims[1], xlims[2],by="months"),
              labels=format(seq(xlims[1], xlims[2],by="months"),'%b'),
              tck=0.025)  
    
    dat <- cbind(lDates[[1]][d2019],datPTS1[i,d2019])
    dat <- na.omit(dat)
    spl <- smooth.spline(dat, spar=0.55)
    xSmooth <- predict(spl, dat[,1])$y
    lines(dat[,1],xSmooth,col='#31a354',lwd=2)
    
    points(lDates[[1]][d2019],datPTS1[i,d2019],pch=21,bg='#2c7fb8')
    
    dev.off()  
  } 
  
  print(ii)
}

