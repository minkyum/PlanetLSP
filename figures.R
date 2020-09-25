library(raster)
library(rgdal)
library(gdalUtils)
library(ncdf4)
library(signal)
library(RColorBrewer)
library(doMC)
library(scales)



load('/projectnb/modislc/users/mkmoon/Planet/data/planet_eviStack.rda')


load('/projectnb/modislc/users/mkmoon/Planet/data/17SQD_hlsTable.rda')
hlsDB <- theTable
load('/projectnb/modislc/users/mkmoon/Planet/data/18TYN_hlsTable.rda')
hlsMF <- theTable
load('/projectnb/modislc/users/mkmoon/Planet/data/19TEL_hlsTable.rda')
hlsEN <- theTable
load('/projectnb/modislc/users/mkmoon/Planet/data/16TCK_hlsTable.rda')
hlsAG <- theTable
load('/projectnb/modislc/users/mkmoon/Planet/data/13TEF_hlsTable.rda')
hlsGR <- theTable
load('/projectnb/modislc/users/mkmoon/Planet/data/10TGP_hlsTable.rda')
hlsSH <- theTable

rm(theTable)

#
setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
png(filename='revisit_1.png',width=13,height=6,unit='in',res=300)

par(mfrow=c(1,1),oma=c(2,1,0,1),mar=c(2,4,2,1),mgp=c(2.5,1,0))

plot(lDates[[1]][1:202],rep(3.5,length(lDates[[1]]))[1:202],pch=15,cex=0.8,
     xlim=c(17137,(17167+365*3+50)),ylim=c(0.7,3.8),col='red',axe=F,ann=F)

box()
axis(2,at=seq(3.5,1,-0.5),c('DB','MF','EN','AG','GR','SH'),cex.axis=1.5)
axis(1,at=as.Date(c('2017-1-1','2018-1-1','2019-1-1','2020-1-1'),
                  origin='1970-1-1'),
     2017:2020,cex.axis=1.5)

legend('topleft',c('Planet','HLS'),pch=15,cex=1.5,bty='n',
       col=c('red','blue'))
# segments(as.Date('2017-3-7',origin='1970-1-1'),0.8,
#          as.Date('2017-3-7',origin='1970-1-1'),3.6,
#          lty=5,lwd=1.0)

points(lDates[[2]][1:155],rep(3.0,length(lDates[[2]]))[1:155],pch=15,cex=0.8,col='red')
points(lDates[[3]][1:194],rep(2.5,length(lDates[[3]]))[1:194],pch=15,cex=0.8,col='red')
points(lDates[[4]][1:312],rep(2.0,length(lDates[[4]]))[1:312],pch=15,cex=0.8,col='red')
points(lDates[[5]][1:365],rep(1.5,length(lDates[[5]]))[1:365],pch=15,cex=0.8,col='red')
points(lDates[[6]][1:341],rep(1.0,length(lDates[[6]]))[1:341],pch=15,cex=0.8,col='red')

text((17167+365*3+50),3.55,length(lDates[[1]][1:202]),col='red',cex=1.5)
text((17167+365*3+50),3.05,length(lDates[[2]][1:155]),col='red',cex=1.5)
text((17167+365*3+50),2.55,length(lDates[[3]][1:194]),col='red',cex=1.5)
text((17167+365*3+50),2.05,length(lDates[[4]][1:213]),col='red',cex=1.5)
text((17167+365*3+50),1.55,length(lDates[[5]][1:365]),col='red',cex=1.5)
text((17167+365*3+50),1.05,length(lDates[[6]][1:341]),col='red',cex=1.5)


hlsDB[[1]]$dates[is.na(hlsDB[[1]]$original_VI)] <- NA
points(hlsDB[[1]]$dates[120:438],rep(3.4,length(hlsDB[[1]]$dates))[120:438],
       col='blue',pch=15,cex=0.8)
hlsMF[[1]]$dates[is.na(hlsMF[[1]]$original_VI)] <- NA
points(hlsMF[[1]]$dates[106:387],rep(2.9,length(hlsMF[[1]]$dates))[106:387],
       col='blue',pch=15,cex=0.8)
hlsEN[[1]]$dates[is.na(hlsEN[[1]]$original_VI)] <- NA
points(hlsEN[[1]]$dates[121:404],rep(2.4,length(hlsEN[[1]]$dates))[121:404],
       col='blue',pch=15,cex=0.8)
hlsAG[[1]]$dates[is.na(hlsAG[[1]]$original_VI)] <- NA
points(hlsAG[[1]]$dates[76:354],rep(1.9,length(hlsAG[[1]]$dates))[76:354],
       col='blue',pch=15,cex=0.8)
hlsGR[[1]]$dates[is.na(hlsGR[[1]]$original_VI)] <- NA
points(hlsGR[[1]]$dates[101:446],rep(1.4,length(hlsGR[[1]]$dates))[101:446],
       col='blue',pch=15,cex=0.8)
hlsSH[[1]]$dates[is.na(hlsSH[[1]]$original_VI)] <- NA
points(hlsSH[[1]]$dates[126:466],rep(0.9,length(hlsSH[[1]]$dates))[126:466],
       col='blue',pch=15,cex=0.8)

text((17167+365*3+50),3.35,sum(!is.na((hlsDB[[1]]$dates[120:438]))),col='blue',cex=1.5)
text((17167+365*3+50),2.85,sum(!is.na((hlsMF[[1]]$dates[106:387]))),col='blue',cex=1.5)
text((17167+365*3+50),2.35,sum(!is.na((hlsEN[[1]]$dates[121:404]))),col='blue',cex=1.5)
text((17167+365*3+50),1.85,sum(!is.na((hlsAG[[1]]$dates[76:354]))),col='blue',cex=1.5)
text((17167+365*3+50),1.35,sum(!is.na((hlsGR[[1]]$dates[101:446]))),col='blue',cex=1.5)
text((17167+365*3+50),0.85,sum(!is.na((hlsSH[[1]]$dates[126:446]))),col='blue',cex=1.5)

dev.off()

## Data requisition stat
t1p <- matrix(NA,18,4)
for(i in 1:6){
  temp <- lDates[[i]]
  
  temp1 <- temp[which(substr(temp,1,4)=='2017')]
  temp1 <- temp1[which(as.numeric(substr(temp1,6,7))>2&as.numeric(substr(temp1,6,7))<11)]
  temp2 <- temp[which(substr(temp,1,4)=='2018')]
  temp2 <- temp2[which(as.numeric(substr(temp2,6,7))>2&as.numeric(substr(temp2,6,7))<11)]
  temp3 <- temp[which(substr(temp,1,4)=='2019')]
  temp3 <- temp3[which(as.numeric(substr(temp3,6,7))>2&as.numeric(substr(temp3,6,7))<11)]
  temp4 <- temp[which(substr(temp,1,4)!='2020')]
  temp4 <- temp4[which(as.numeric(substr(temp4,6,7))>2&as.numeric(substr(temp4,6,7))<11)]
  
  t1p[(3*(i-1)+1),1] <- mean(diff(temp1))
  t1p[(3*(i-1)+2),1] <- median(diff(temp1))
  t1p[(3*(i-1)+3),1] <- max(diff(temp1))
  t1p[(3*(i-1)+1),2] <- mean(diff(temp2))
  t1p[(3*(i-1)+2),2] <- median(diff(temp2))
  t1p[(3*(i-1)+3),2] <- max(diff(temp2))
  t1p[(3*(i-1)+1),3] <- mean(diff(temp3))
  t1p[(3*(i-1)+2),3] <- median(diff(temp3))
  t1p[(3*(i-1)+3),3] <- max(diff(temp3))
  t1p[(3*(i-1)+1),4] <- mean(diff(temp4))
  t1p[(3*(i-1)+2),4] <- median(diff(temp4))
  t1p[(3*(i-1)+3),4] <- max(diff(temp4))
}
t1p <- round(t1p,1)

t1h <- matrix(NA,18,4)
for(i in 1:6){
  if(i==1){
    temp <- na.omit(hlsDB[[1]]$dates)
  }else if(i==2){
    temp <- na.omit(hlsMF[[1]]$dates)
  }else if(i==3){
    temp <- na.omit(hlsEN[[1]]$dates)
  }else if(i==4){
    temp <- na.omit(hlsAG[[1]]$dates)
  }else if(i==5){
    temp <- na.omit(hlsGR[[1]]$dates)
  }else{
    temp <- na.omit(hlsSH[[1]]$dates)
  }
  
  temp1 <- temp[which(substr(temp,1,4)=='2017')]
  temp1 <- temp1[which(as.numeric(substr(temp1,6,7))>2&as.numeric(substr(temp1,6,7))<11)]
  temp2 <- temp[which(substr(temp,1,4)=='2018')]
  temp2 <- temp2[which(as.numeric(substr(temp2,6,7))>2&as.numeric(substr(temp2,6,7))<11)]
  temp3 <- temp[which(substr(temp,1,4)=='2019')]
  temp3 <- temp3[which(as.numeric(substr(temp3,6,7))>2&as.numeric(substr(temp3,6,7))<11)]
  temp4 <- c(temp1,temp2,temp3)
  
  t1h[(3*(i-1)+1),1] <- mean(diff(temp1))
  t1h[(3*(i-1)+2),1] <- median(diff(temp1))
  t1h[(3*(i-1)+3),1] <- max(diff(temp1))
  t1h[(3*(i-1)+1),2] <- mean(diff(temp2))
  t1h[(3*(i-1)+2),2] <- median(diff(temp2))
  t1h[(3*(i-1)+3),2] <- max(diff(temp2))
  t1h[(3*(i-1)+1),3] <- mean(diff(temp3))
  t1h[(3*(i-1)+2),3] <- median(diff(temp3))
  t1h[(3*(i-1)+3),3] <- max(diff(temp3))
  t1h[(3*(i-1)+1),4] <- mean(diff(temp4))
  t1h[(3*(i-1)+2),4] <- median(diff(temp4))
  t1h[(3*(i-1)+3),4] <- max(diff(temp4))
}
t1h <- round(t1h,1)

setwd('/projectnb/modislc/users/mkmoon/Planet/data/')
write.csv(t1p,file='t1p_gws.csv')
write.csv(t1h,file='t1h_gws.csv')




############################
source('/usr3/graduate/mkmoon/GitHub/PlanetLSP/PLSP_Functions.R')
vgt <- c('DB','MF','EN','AG','GR','SH')

# Fig 2 HF ts
# Time series
shpPoints <- readOGR('/projectnb/modislc/users/mkmoon/Planet/shp/18TYN_pts_1.shp')

datPTS1 <- extractTS(shpPoints,leviStack[[2]],limgBase[[2]],id=1,rad=5)
datPTS2 <- extractTS(shpPoints,leviStack[[2]],limgBase[[2]],id=2,rad=5)

setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')

png(filename='hf_2019_ts_hls_planet_1.png',width=13,height=5.5,unit='in',res=300)

par(mfrow=c(2,1),oma=c(2,1,0,1),mar=c(0.2,5,1,5),mgp=c(2.5,1,0))
plot(lDates[[2]][1:155],apply(datPTS1[,1:155],2,median,na.rm=T),
     col=alpha('red',0.7),ylim=c(0,1),axe=F,ann=F,
     pch=19,cex=1.1) # Shapefile point 2
box(lty=1)
axis(2,seq(0,1,0.5),cex.axis=1.3)
mtext('EVI2',2,2.4,cex=1.3)
text(18250,0.85,'DB',pos=2,cex=2)
abline(v=18034,lty=5,lwd=1.5)

legend(17480,1,c('Planet','HLS','PhenoCam'),pch=19,cex=1.3,
       col=c('red','blue','forestgreen'),bty='n',
       pt.cex=c(1.3,1.3,1.0))


points(hlsMF[[2]]$dates,hlsMF[[2]]$original_VI,
       col=alpha('blue',0.7),pch=19,cex=1.1)

par(new = TRUE)
plot(as.Date(99:(365*3),origin='2016-12-31'),datGcc[99:(365*3),3],
     axes=F,bty="n",xlab="",ylab="",
     ylim=c(0.29,0.50),pch=19,cex=0.9,col=alpha('forestgreen',0.5))
axis(4,seq(0,1,0.1),cex.axis=1.3)
mtext(expression(italic(G[CC])),4,2.5,cex=1.3)


plot(lDates[[2]][1:155],apply(datPTS2[,1:155],2,median,na.rm=T),
     col=alpha('red',0.7),ylim=c(0,1),axe=F,ann=F,
     pch=19,cex=1.1) # Shapefile point 2
box(lty=1)
axis(2,seq(0,1,0.5),cex.axis=1.3)
mtext('EVI2',2,2.4,cex=1.3)
text(18250,0.85,'EN',pos=2,cex=2)
axis(1,at=as.Date(c(99,366,(365*2+1),(365*3+1)),origin='2016-12-31'),
     c('Apr 2017','Jan 2018','Jan 2019','Jan 2020'),
     cex.axis=1.3)
abline(v=18034,lty=5,lwd=1.5)

points(hlsMF[[1]]$dates,hlsMF[[1]]$original_VI,
       col=alpha('blue',0.7),pch=19,cex=1.1)

par(new = TRUE)
plot(as.Date(99:(365*3),origin='2016-12-31'),datGcc[99:(365*3),4],
     axes=F,bty="n",xlab="",ylab="",
     ylim=c(0.33,0.47),pch=19,cex=0.9,col=alpha('forestgreen',0.5))
axis(4,seq(0,1,0.05),cex.axis=1.3)
mtext(expression(italic(G[CC])),4,2.5,cex=1.3)


dev.off()



#######
# AG
# Time series
shpPoints <- readOGR('/projectnb/modislc/users/mkmoon/Planet/shp/16TCK_pts_1.shp')

datPTS1 <- extractTS(shpPoints,leviStack[[4]],limgBase[[4]],id=1,rad=5)
datPTS2 <- extractTS(shpPoints,leviStack[[4]],limgBase[[4]],id=2,rad=5)
datPTS3 <- extractTS(shpPoints,leviStack[[4]],limgBase[[4]],id=3,rad=5)
datPTS4 <- extractTS(shpPoints,leviStack[[4]],limgBase[[4]],id=4,rad=5)

png(filename='uief_2019_ts_hls_1.png',width=13,height=6.5,unit='in',res=300)

par(mfrow=c(4,1),oma=c(2,1,0,1),mar=c(0.2,5,1,5),mgp=c(2.5,1,0))
# for(i in 1:nrow(datPTS1)){
plot(lDates[[4]][209:312],apply(datPTS1[,209:312],2,median,na.rm=T),
     col=alpha('red',0.7),ylim=c(0,1),axe=F,ann=F,
     pch=19,cex=1.5) # Shapefile point 1
box(lty=1)
axis(2,seq(0,1,0.5),cex.axis=1.8)
mtext('EVI2',2,2.7,cex=1.3)
text(18250,0.85,'1',pos=2,cex=3)
abline(v=18057,lty=5,lwd=1.5)

points(hlsAG[[1]]$dates,hlsAG[[1]]$original_VI,
       col=alpha('blue',0.7),pch=19,cex=1.5)

legend(17950,1,c('Planet','HLS','PhenoCam'),pch=19,cex=2,
       col=c('red','blue','forestgreen'),bty='n',
       pt.cex=c(1.7,1.7,1.4))

par(new = TRUE)
plot(as.Date(3:359,origin='2018-12-31'),datGcc[(731+2):(1095-6),8], 
     axes=F,bty="n",xlab="",ylab="",
     ylim=c(0.32,0.47),pch=19,cex=1.2,col=alpha('forestgreen',0.7))
axis(4,seq(0,1,0.05),cex.axis=1.8)
mtext(expression(italic(G[CC])),4,3,cex=1.3)


plot(lDates[[4]][209:312],apply(datPTS2[,209:312],2,median,na.rm=T),
     col=alpha('red',0.7),ylim=c(0,1),axe=F,ann=F,
     pch=19,cex=1.5) # Shapefile point 2
box(lty=1)
axis(2,seq(0,1,0.5),cex.axis=1.8)
mtext('EVI2',2,2.7,cex=1.3)
text(18250,0.85,'2',pos=2,cex=3)
abline(v=18057,lty=5,lwd=1.5)

points(hlsAG[[4]]$dates,hlsAG[[4]]$original_VI,
       col=alpha('blue',0.7),pch=19,cex=1.5)

par(new = TRUE)
plot(as.Date(3:359,origin='2018-12-31'),datGcc[(731+2):(1095-6),7], 
     axes=F,bty="n",xlab="",ylab="",
     ylim=c(0.30,0.50),pch=19,cex=1.2,col=alpha('forestgreen',0.7))
axis(4,seq(0,1,0.05),cex.axis=1.8)
mtext(expression(italic(G[CC])),4,3,cex=1.3)

plot(lDates[[4]][209:312],apply(datPTS3[,209:312],2,median,na.rm=T),
     col=alpha('red',0.7),ylim=c(0,1),axe=F,ann=F,
     pch=19,cex=1.5) # Shapefile point 3
box(lty=1)
axis(2,seq(0,1,0.5),cex.axis=1.8)
mtext('EVI2',2,2.7,cex=1.3)
text(18250,0.85,'3',pos=2,cex=3)
abline(v=18057,lty=5,lwd=1.5)

points(hlsAG[[2]]$dates,hlsAG[[2]]$original_VI,
       col=alpha('blue',0.7),pch=19,cex=1.5)

par(new = TRUE)
plot(as.Date(3:359,origin='2018-12-31'),datGcc[(731+2):(1095-6),6], 
     axes=F,bty="n",xlab="",ylab="",
     ylim=c(0.29,0.45),pch=19,cex=1.2,col=alpha('forestgreen',0.7))
axis(4,seq(0,1,0.05),cex.axis=1.8)
mtext(expression(italic(G[CC])),4,3,cex=1.3)

plot(lDates[[4]][209:312],apply(datPTS4[,209:312],2,median,na.rm=T),
     col=alpha('red',0.7),ylim=c(0,1),axe=F,ann=F,
     pch=19,cex=1.5) # Shapefile point 4
box(lty=1)
axis(2,seq(0,1,0.5),cex.axis=1.8)
axis(1,at=as.Date(c(15,74,135,196,258,319,(365+15)),origin='2018-12-31'),
     c('Jan','Mar','May','Jul','Sep','Nov','Jan'),
     cex.axis=1.8)
mtext('EVI2',2,2.7,cex=1.3)
text(18250,0.85,'4',pos=2,cex=3)
abline(v=18057,lty=5,lwd=1.5)

points(hlsAG[[3]]$dates,hlsAG[[3]]$original_VI,
       col=alpha('blue',0.7),pch=19,cex=1.5)

par(new = TRUE)
plot(as.Date(3:359,origin='2018-12-31'),datGcc[(731+2):(1095-6),5], 
     axes=F,bty="n",xlab="",ylab="",
     ylim=c(0.31,0.42),pch=19,cex=1.2,col=alpha('forestgreen',0.7))
axis(4,seq(0,1,0.05),cex.axis=1.8)
mtext(expression(italic(G[CC])),4,3,cex=1.3)


dev.off()



###
# DB, EN, GR, and SH
# Time series
shpPoints <- readOGR('/projectnb/modislc/users/mkmoon/Planet/shp/17SQD_pts_1.shp')
datPTSdb <- extractTS(shpPoints,leviStack[[1]],limgBase[[1]],id=1,rad=5)
shpPoints <- readOGR('/projectnb/modislc/users/mkmoon/Planet/shp/19TEL_pts_1.shp')
datPTSen <- extractTS(shpPoints,leviStack[[3]],limgBase[[3]],id=1,rad=5)
shpPoints <- readOGR('/projectnb/modislc/users/mkmoon/Planet/shp/13TEF_pts_1.shp')
datPTSgr <- extractTS(shpPoints,leviStack[[5]],limgBase[[5]],id=1,rad=5)
shpPoints <- readOGR('/projectnb/modislc/users/mkmoon/Planet/shp/10TGP_pts_1.shp')
datPTSsh <- extractTS(shpPoints,leviStack[[6]],limgBase[[6]],id=1,rad=5)


png(filename='others_ts_1.png',width=12,height=7,unit='in',res=300)

par(mfrow=c(4,1),oma=c(2,1,0,1),mar=c(0.2,5,1,5),mgp=c(2.5,1,0))

# DB
plot(lDates[[1]][1:199],apply(datPTSdb[,1:199],2,median,na.rm=T),
     col=alpha('red',0.7),ylim=c(0,1),axe=F,ann=F,xlim=c(17167,17167+365*3),
     pch=19,cex=1.5) # Shapefile point 1
box(lty=1)
axis(2,seq(0,1,0.5),cex.axis=1.8)
mtext('EVI2',2,2.4,cex=1.3)
text(18250,0.85,'1',pos=2,cex=3)

legend(17167+300,1.05,c('Planet','HLS','PhenoCam'),pch=19,cex=1.8,
       col=c('red','blue','forestgreen'),bty='n',
       pt.cex=c(1.7,1.7,1.4))

points(hlsDB[[1]]$dates,hlsDB[[1]]$original_VI,
       col=alpha('blue',0.7),pch=19,cex=1.5)

par(new = TRUE)
plot(as.Date(1:(365*3),origin='2016-12-31'),datGcc[1:(365*3),2],
     axes=F,bty="n",xlab="",ylab="",
     ylim=c(0.30,0.54),pch=19,cex=1.2,col=alpha('forestgreen',0.5))
axis(4,seq(0,1,0.1),cex.axis=1.8)
mtext(expression(italic(G[CC])),4,3,cex=1.3)
abline(v=as.Date(125,origin='2018-12-31'),lty=5,lwd=1.5)

# EN
plot(lDates[[3]][1:194],apply(datPTSen[,1:194],2,median,na.rm=T),
     col=alpha('red',0.7),ylim=c(0,0.7),axe=F,ann=F,xlim=c(17167,17167+365*3),
     pch=19,cex=1.5) # Shapefile point 1
box(lty=1)
axis(2,seq(0,1,0.3),cex.axis=1.8)
mtext('EVI2',2,2.4,cex=1.3)
text(18250,0.61,'2',pos=2,cex=3)

points(hlsEN[[1]]$dates,hlsEN[[1]]$original_VI,
       col=alpha('blue',0.7),pch=19,cex=1.5)

par(new = TRUE)
plot(as.Date(1:(365*3),origin='2016-12-31'),datGcc[1:(365*3),11],
     axes=F,bty="n",xlab="",ylab="",
     ylim=c(0.325,0.423),pch=19,cex=1.2,col=alpha('forestgreen',0.5))
axis(4,seq(0,1,0.05),cex.axis=1.8)
mtext(expression(italic(G[CC])),4,3,cex=1.3)
abline(v=as.Date(125,origin='2018-12-31'),lty=5,lwd=1.5)

# GR
plot(lDates[[5]][1:365],apply(datPTSgr[,1:365],2,median,na.rm=T),
     col=alpha('red',0.7),ylim=c(0,0.4),axe=F,ann=F,xlim=c(17167,17167+365*3),
     pch=19,cex=1.5) # Shapefile point 1
box(lty=1)
axis(2,seq(0,1,0.2),cex.axis=1.8)
mtext('EVI2',2,2.4,cex=1.3)
text(18250,0.35,'3',pos=2,cex=3)

points(hlsGR[[4]]$dates,hlsGR[[4]]$original_VI,
       col=alpha('blue',0.7),pch=19,cex=1.5)

par(new = TRUE)
plot(as.Date(1:(365*3),origin='2016-12-31'),datGcc[1:(365*3),10],
     axes=F,bty="n",xlab="",ylab="",
     ylim=c(0.305,0.415),pch=19,cex=1.2,col=alpha('forestgreen',0.5))
axis(4,seq(0,1,0.03),cex.axis=1.8)
mtext(expression(italic(G[CC])),4,3,cex=1.3)
abline(v=as.Date(125,origin='2018-12-31'),lty=5,lwd=1.5)

# SH
plot(lDates[[6]][1:342],apply(datPTSsh[,1:342],2,median,na.rm=T),
     col=alpha('red',0.7),ylim=c(0,0.25),axe=F,ann=F,xlim=c(17167,17167+365*3),
     pch=19,cex=1.5) # Shapefile point 1
box(lty=1)
axis(2,seq(0,1,0.1),cex.axis=1.8)
mtext('EVI2',2,2.4,cex=1.3)
text(18250,0.215,'4',pos=2,cex=3)

points(hlsSH[[4]]$dates,hlsSH[[4]]$original_VI,
       col=alpha('blue',0.7),pch=19,cex=1.5)

par(new = TRUE)
plot(as.Date(1:(365*3),origin='2016-12-31'),datGcc[1:(365*3),9],
     axes=F,bty="n",xlab="",ylab="",
     ylim=c(0.33,0.378),pch=19,cex=1.2,col=alpha('forestgreen',0.5))
axis(4,seq(0,1,0.03),cex.axis=1.8)
mtext(expression(italic(G[CC])),4,3,cex=1.3)

axis(1,at=as.Date(c(1,366,(365*2+1),(365*3+1)),origin='2016-12-31'),
     c(2017,2018,2019,2020),
     cex.axis=1.8)
abline(v=as.Date(125,origin='2018-12-31'),lty=5,lwd=1.5)

dev.off()


###########################################
## 1 to 1

load('/projectnb/modislc/users/mkmoon/Planet/data/planet_eviStack.rda')
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
  
  eviStack <- leviStack[[ss]]
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
    datPTSag <- extractTS(shpPoints,eviStack,imgBase,id=ids[i],rad=15)
    hlsPT <- theTable[[i]]
    
    if(sum(!is.na(hlsPT))==8){
      ptsP <- NULL
      ptsH <- NULL
      k <- 1
      for(j in 1:length(dates)){
        if(length(which(dates[j]==hlsPT$dates))==1){
          ptsP[k] <- j
          ptsH[k] <- which(dates[j]==hlsPT$dates)
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
      ptsHLS[[i]] <- hlsPT$original_VI[ptsH]
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

# plot
setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')

png(filename='stat_1to1_r.png',width=13,height=8.5,unit='in',res=300)

par(fig=c(0,0.318,0,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0))
plot(c(5,seq(10,90,10),95),stat1to1db[,1],pch=1,cex=1.3,
     xlim=c(0,100),ylim=c(0.89,0.975),type='o',lwd=2,axe=F,ann=F)
box(lty=1)
axis(1,c(5,25,50,75,95),cex.axis=1.3)
axis(2,seq(0.88,1,0.01),cex.axis=1.3)
mtext('Quantile (%)',1,2.5,cex=1.5)
mtext('Correlation',2,2.5,cex=1.5)

points(c(5,seq(10,90,10),95),stat1to1mf[,1],type='o',pch=2,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),stat1to1ag[,1],type='o',pch=4,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),stat1to1gr[,1],type='o',pch=5,cex=1.3,lwd=2)

par(new=T)
plot(c(5,seq(10,90,10),95),stat1to1en[,1],type='o',
     axes=F,bty="n",xlab="",ylab="",
     ylim=c(0.77,0.96),col='darkgreen',lwd=2,cex=1.3,pch=3)
axis(4,seq(0.82,0.86,0.02),cex.axis=1.3,col='darkgreen',line=0.2,col.axis='darkgreen')
axis(4,seq(0.82,0.86,0.02),cex.axis=1.3,col='darkgreen',line=0.2,col.axis='darkgreen')

par(new=T)
plot(c(5,seq(10,90,10),95),stat1to1sh[,1],type='o',
     axes=F,bty="n",xlab="",ylab="",
     ylim=c(0.74,0.95),col='darkblue',lwd=2,cex=1.3,pch=6)
axis(4,seq(0.74,0.780,0.02),cex.axis=1.3,col='darkblue',line=0.2,col.axis='darkblue')
axis(4,seq(0.74,0.780,0.02),cex.axis=1.3,col='darkblue',line=0.2,col.axis='darkblue')

par(fig=c(0.342,0.679,0,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0),new=T)
plot(c(5,seq(10,90,10),95),stat1to1db[,2],pch=1,cex=1.3,
     xlim=c(0,100),ylim=c(0.005,0.11),type='o',lwd=2,axe=F,ann=F)
box(lty=1)
axis(1,c(5,25,50,75,95),cex.axis=1.3)
axis(2,seq(0,0.1,0.02),cex.axis=1.3)
mtext('Quantile (%)',1,2.5,cex=1.5)
mtext('RMSE',2,2.5,cex=1.5)

legend('topright',c('DB','MF','EN','AG','GR','SH'),pch=1:6,cex=1.3,bty='n',lwd=2)

points(c(5,seq(10,90,10),95),stat1to1mf[,2],type='o',pch=2,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),stat1to1en[,2],type='o',pch=3,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),stat1to1ag[,2],type='o',pch=4,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),stat1to1gr[,2],type='o',pch=5,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),stat1to1sh[,2],type='o',pch=6,cex=1.3,lwd=2)

par(fig=c(0.681,1,0,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0),new=T)
plot(c(5,seq(10,90,10),95),stat1to1db[,3],pch=1,cex=1.3,
     xlim=c(0,100),ylim=c(-0.1,0.02),type='o',lwd=2,axe=F,ann=F)
box(lty=1)
axis(1,c(5,25,50,75,95),cex.axis=1.3)
axis(2,seq(-0.1,0.02,0.02),cex.axis=1.3)
mtext('Quantile (%)',1,2.5,cex=1.5)
mtext('Bias',2,2.5,cex=1.5)

points(c(5,seq(10,90,10),95),stat1to1mf[,3],type='o',pch=2,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),stat1to1en[,3],type='o',pch=3,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),stat1to1ag[,3],type='o',pch=4,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),stat1to1gr[,3],type='o',pch=5,cex=1.3,lwd=2)
points(c(5,seq(10,90,10),95),stat1to1sh[,3],type='o',pch=6,cex=1.3,lwd=2)
abline(h=0,lty=5)

dev.off()

# # plot
# par(mfrow=c(2,3),oma=c(2,2,2,2),mar=c(4,4,4,4),mgp=c(2.5,1,0))
# 
# for(ss in 1:6){
#   plot(ptsPP[[ss]],ptsHH[[ss]],xlim=c(0,1),ylim=c(0,1))
#   abline(0,1)
#   summary(lm(ptsHH[[ss]]~ptsPP[[ss]]))
#   abline(lm(ptsHH[[ss]]~ptsPP[[ss]]),col='red')
# }

png(filename='1to1_evi.png',width=12,height=8,unit='in',res=300)

par(mfrow=c(2,3),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.5,1,0))
spec <- rev(brewer.pal(11,'Spectral'))
mycolRamp = colorRampPalette(c('White',spec))
for(ss in 1:6){
  x1 <- ptsPP50[[ss]]
  y1 <- ptsHH[[ss]]

  # if(ss<5){
    limx <- c(0,0.9)
    limy <- c(0,0.9)
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
                xlab='Planet EVI2',ylab='HLS EVI2',
                cex.lab=1.7)
  # if(ss<5){
    axis(1,at=seq(0,1,0.3),cex.axis=1.7)
    axis(2,at=seq(0,1,0.3),cex.axis=1.7)
  # }else if(ss==5){
  #   axis(1,at=seq(0,1,0.2),cex.axis=1.5)
  #   axis(2,at=seq(0,1,0.2),cex.axis=1.5)
  # }else{
  #   axis(1,at=seq(0,1,0.1),cex.axis=1.5)
  #   axis(2,at=seq(0,1,0.1),cex.axis=1.5)
  # }
  abline(0,1,lty=5)
  
  
  reg <- round(cor(x1,y1,use='na.or.complete'),3)
  rmse <- round(sqrt(mean((x1-y1)^2,na.rm=T)),2)
  bias <- round(mean(x1-y1,na.rm=T),2)
  # nn <- sum(!is.na(x1))
  
  text(0.53,0.23,expression(paste(italic(r),' =',sep='')),cex=1.8,pos=4)
  text(0.61,0.23,reg,cex=1.8,pos=4)
  text(0.53,0.16,paste('RMSE = ',rmse,sep=''),cex=1.8,pos=4)
  text(0.53,0.09,paste('Bias = ',bias,sep=''),cex=1.8,pos=4)
  text(0.53,0.02,'n = 100000',cex=1.8,pos=4)
  
}

dev.off()




#######################
# AG histogram
rastPP <- raster('/projectnb/modislc/users/mkmoon/Planet/phe/AG/phe_Planet_2019_sos_AG.tif')

fileHLS <- '/projectnb/modislc/projects/landsat_sentinel/MSLSP_HLS30/16TCK/phenoMetrics/MSLSP_16TCK_2019.nc'
rastHH <- raster(fileHLS,varname='50PCGI')
rastHH <- projectRaster(rastHH,rastPP,method='ngb')

hist(rastPP)

htPP  <- hist(rastPP,breaks=seq(0,400,2),plot=F)
htHH <- hist(rastHH,breaks=seq(0,400,2),plot=F)
htPP$counts <- htPP$counts/max(htPP$counts)
htHH$counts <- htHH$counts/max(htHH$counts)

png(filename='hist_ag.png',width=11.5,height=3.5,unit='in',res=300)
par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.7,1,0))
plot(htPP,xlim=c(80,260),col=rgb(1,0,0,0.5),main='',
     xlab='50% Greenup dates (day of year)',
     ylab='Relative frequency',
     cex.lab=1.5,cex.axis=1.5)
plot(htHH,col=rgb(0,0,1,0.5),add=T)
legend('topright',c('Planet','HLS'),pch=22,pt.bg=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),
       bty='n',cex=1.5,pt.cex=2)
dev.off()


# CDL
rastCDL <- raster('/projectnb/modislc/data/lc_database/regional/united_states/cropland_data_layer/national_2019/2019_30m_cdls.img')
pr3 <- projectExtent(rastPP,crs(rastCDL))
rastCDL <- crop(rastCDL,pr3)
plot(rastCDL)
barplot(table(values(rastCDL)))


### HF hist & NLCD
rastPP <- raster('/projectnb/modislc/users/mkmoon/Planet/phe/MF/phe_Planet_2019_sos_MF.tif')

fileHLS <- '/projectnb/modislc/projects/landsat_sentinel/MSLSP_HLS30/18TYN/phenoMetrics/MSLSP_18TYN_2019.nc'
rastHH <- raster(fileHLS,varname='50PCGI')
rastHH <- projectRaster(rastHH,rastPP,method='ngb')

htPP  <- hist(rastPP,breaks=seq(0,400,1),plot=F)
htHH <- hist(rastHH,breaks=seq(0,400,1),plot=F)
htPP$counts <- htPP$counts/max(htPP$counts)
htHH$counts <- htHH$counts/max(htHH$counts)

# png(filename='hist_mf.png',width=11.5,height=3.5,unit='in',res=300)
# 
# par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.7,1,0))
# plot(htPP,xlim=c(125,165),col=rgb(1,0,0,0.5),main='',
#      xlab='50% Greenup dates (day of year)',
#      ylab='Relative frequency',
#      cex.lab=1.5,cex.axis=1.5)
# plot(htHH,col=rgb(0,0,1,0.5),add=T)
# legend('topright',c('Planet','HLS'),pch=22,pt.bg=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),
#        bty='n',cex=1.5,pt.cex=2)
# 
# dev.off()


rastNLCD <- raster('/projectnb/modislc/data/lc_database/regional/united_states/NLCD_2016_Land_Cover_L48_20190424/NLCD_2016_Land_Cover_L48_20190424.img')
pr3 <- projectExtent(rastPP,crs(rastNLCD))
rastNLCD <- crop(rastNLCD,pr3)
plot(rastNLCD)
barplot(table(values(rastNLCD)))








