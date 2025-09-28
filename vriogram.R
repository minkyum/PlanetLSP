library(gstat)
library(raster)
library(rgdal)
library(gdalUtils)
library(RColorBrewer)

# # Harvard Forest
# rast <- raster('/projectnb/modislc/users/mkmoon/Planet/phe/2019/MF/phe_Planet_2019_sos_MF.tif')
# xy <- cbind(rep(seq(rast@extent@xmin+1.5,rast@extent@xmax+1.5,3),1001),
#             rep(seq(rast@extent@ymin+1.5,rast@extent@ymax+1.5,3),each=1001))
# xy <- data.frame(xy)
# names(xy) <- c('x','y')
# dat <- cbind(extract(rast, xy, df = T),xy)
# dat <- na.omit(dat)
# dat <- dat[sample(1:dim(dat)[1],100000),]
# coordinates(dat)=~x+y
# class(dat)
# 
# variMFs  <- variogram(phe_Planet_2019_sos_MF~1,data=dat,width=1.5,cutoff=1000)
# # plot(variMFs$dist,variMFs$gamma,xlim=c(0,1000))
# # abline(v=30)
# 
# rast <- raster('/projectnb/modislc/users/mkmoon/Planet/phe/2019/MF/phe_Planet_2019_eos_MF.tif')
# xy <- cbind(rep(seq(rast@extent@xmin+1.5,rast@extent@xmax+1.5,3),1001),
#             rep(seq(rast@extent@ymin+1.5,rast@extent@ymax+1.5,3),each=1001))
# xy <- data.frame(xy)
# names(xy) <- c('x','y')
# dat <- cbind(extract(rast, xy, df = T),xy)
# dat <- na.omit(dat)
# dat <- dat[sample(1:dim(dat)[1],100000),]
# coordinates(dat)=~x+y
# class(dat)
# 
# variMFe  <- variogram(phe_Planet_2019_eos_MF~1,data=dat,width=1.5,cutoff=1000)
# 
# 
# # UIEF
# rast <- raster('/projectnb/modislc/users/mkmoon/Planet/phe/2019/AG/phe_Planet_2019_sos_AG.tif')
# xy <- cbind(rep(seq(rast@extent@xmin+1.5,rast@extent@xmax+1.5,3),1001),
#             rep(seq(rast@extent@ymin+1.5,rast@extent@ymax+1.5,3),each=1001))
# xy <- data.frame(xy)
# names(xy) <- c('x','y')
# dat <- cbind(extract(rast, xy, df = T),xy)
# dat <- na.omit(dat)
# dat <- dat[sample(1:dim(dat)[1],100000),]
# coordinates(dat)=~x+y
# class(dat)
# 
# variAGs  <- variogram(phe_Planet_2019_sos_AG~1,data=dat,width=1.5,cutoff=1000)
# # plot(variAGs$dist,variAGs$gamma,xlim=c(0,1000))
# 
# rast <- raster('/projectnb/modislc/users/mkmoon/Planet/phe/2019/AG/phe_Planet_2019_eos_AG.tif')
# xy <- cbind(rep(seq(rast@extent@xmin+1.5,rast@extent@xmax+1.5,3),1001),
#             rep(seq(rast@extent@ymin+1.5,rast@extent@ymax+1.5,3),each=1001))
# xy <- data.frame(xy)
# names(xy) <- c('x','y')
# dat <- cbind(extract(rast, xy, df = T),xy)
# dat <- na.omit(dat)
# dat <- dat[sample(1:dim(dat)[1],100000),]
# coordinates(dat)=~x+y
# class(dat)
# 
# variAGe  <- variogram(phe_Planet_2019_eos_AG~1,data=dat,width=1.5,cutoff=1000)
# 
# 
# #################
# setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
# png(filename='semivariance_phe_1.png',width=13,height=6,unit='in',res=300)
# 
# par(mfrow=c(1,2),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.3,1,0))
# plot(variMFs$dist,sqrt(variMFs$gamma),xlim=c(0,120),ylim=c(2,17),
#      xlab='Distance (meter)',ylab=expression(paste(sqrt('Semi-variance'),' (days)')),
#      cex.lab=1.5,cex.axis=1.3,
#      pch=19,cex=1.3,axe=F)
# points(variMFe$dist,sqrt(variMFe$gamma),cex=1.3)
# abline(v=30,lty=5,lwd=1.5)
# box()
# axis(1,at=seq(0,120,30),cex.axis=1.5)
# axis(2,at=seq(0,20,5),cex.axis=1.5)
# legend('topright',c('Spring','Fall'),pch=c(19,1),cex=1.7,bty='n',pt.cex=1.3)
# 
# plot(variAGs$dist,sqrt(variAGs$gamma),xlim=c(0,120),ylim=c(2,17),
#      xlab='Distance (meter)',ylab=expression(paste(sqrt('Semi-variance'),' (days)')),
#      cex.lab=1.5,cex.axis=1.3,
#      pch=19,cex=1.3,axe=F)
# points(variAGe$dist,sqrt(variAGe$gamma),cex=1.3)
# abline(v=30,lty=5,lwd=1.5)
# box()
# axis(1,at=seq(0,120,30),cex.axis=1.5)
# axis(2,at=seq(0,20,5),cex.axis=1.5)
# 
# dev.off()
# 
# # #
# # setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
# # png(filename='semivariance_phe_1.png',width=6.5,height=6,unit='in',res=300)
# # 
# # par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.3,1,0))
# # plot(variMFs$dist,sqrt(variMFs$gamma),xlim=c(0,150),ylim=c(1.5,16),
# #      xlab='Distance (meter)',ylab=expression(paste(sqrt('Semi-variance'),' (days)')),
# #      cex.lab=1.5,cex.axis=1.3,
# #      pch=19,cex=1.3)
# # points(variMFe$dist,sqrt(variMFe$gamma),cex=1.3)
# # points(variAGs$dist,sqrt(variAGs$gamma),pch=17,cex=1.3)
# # points(variAGe$dist,sqrt(variAGe$gamma),pch=2,cex=1.3)
# # abline(v=30,lty=5,lwd=1.5)
# # legend('topleft',c('MF Spring','MF Fall','AG Spring','AG Fall'),
# #        pch=c(19,1,17,2),cex=1.7,bty='n',pt.cex=1.3)
# # 
# # dev.off()
# #################
# 
# 
# 
# #####################################################3
# # all phenometrics
# 
# # Harvard Forest
# variMFall <- vector('list',7)
# for(i in 1:7){
#         rast <- raster(paste('/projectnb/modislc/users/mkmoon/Planet/phe/2019/MF/phe_Planet_2019_MF_',i,'.tif',sep=''))
#         xy <- cbind(rep(seq(rast@extent@xmin+1.5,rast@extent@xmax+1.5,3),1001),
#                     rep(seq(rast@extent@ymin+1.5,rast@extent@ymax+1.5,3),each=1001))
#         xy <- data.frame(xy)
#         names(xy) <- c('x','y')
#         dat <- cbind(extract(rast, xy, df = T),xy)
#         dat <- na.omit(dat)
#         dat <- dat[sample(1:dim(dat)[1],100000),]
#         names(dat) <- c('ID','date','x','y')
#         coordinates(dat)=~x+y
#         class(dat)
#         
#         variMFall[[i]]  <- variogram(date~1,data=dat,width=1.5,cutoff=1000)
#         
#         print(i)
# }
# 
# # UIEF
# variAGall <- vector('list',7)
# for(i in 1:7){
#         rast <- raster(paste('/projectnb/modislc/users/mkmoon/Planet/phe/2019/AG/phe_Planet_2019_AG_',i,'.tif',sep=''))
#         xy <- cbind(rep(seq(rast@extent@xmin+1.5,rast@extent@xmax+1.5,3),1001),
#                     rep(seq(rast@extent@ymin+1.5,rast@extent@ymax+1.5,3),each=1001))
#         xy <- data.frame(xy)
#         names(xy) <- c('x','y')
#         dat <- cbind(extract(rast, xy, df = T),xy)
#         dat <- na.omit(dat)
#         dat <- dat[sample(1:dim(dat)[1],100000),]
#         names(dat) <- c('ID','date','x','y')
#         coordinates(dat)=~x+y
#         class(dat)
#         
#         variAGall[[i]]  <- variogram(date~1,data=dat,width=1.5,cutoff=1000)
#         
#         print(i)
# }
# 
# # setwd('/projectnb/modislc/users/mkmoon/Planet/data/')
# # save(variMFall,variAGall,
# #      file='semivari_phe_all.rda')
# 
# #################
# load('/projectnb/modislc/users/mkmoon/Planet/data/semivari_phe_all.rda')
# 
# setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
# png(filename='semivariance_phe_all_0.png',width=13,height=6,unit='in',res=300)
# 
# par(mfrow=c(1,2),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.3,1,0))
# ppch <- c(15,16,17,4,2,1,0)
# for(i in 1:7){
#         if(i==1){
#                 plot(variMFall[[i]]$dist,sqrt(variMFall[[i]]$gamma),xlim=c(0,150),ylim=c(0,20),
#                      xlab='Distance (meter)',ylab=expression(paste(sqrt('Semi-variance'),' (days)')),
#                      cex.lab=1.5,cex.axis=1.3,
#                      pch=ppch[i],cex=1.3,axe=F)
#                 abline(v=30,lty=5,lwd=1.5)
#                 box()
#                 axis(1,at=seq(0,150,30),cex.axis=1.5)
#                 axis(2,at=seq(0,20,5),cex.axis=1.5)
#         }else{
#                 points(variMFall[[i]]$dist,sqrt(variMFall[[i]]$gamma),cex=1.3,pch=ppch[i])        
#         }
# }
# for(i in 1:7){
#         if(i==1){
#                 plot(variAGall[[i]]$dist,sqrt(variAGall[[i]]$gamma),xlim=c(0,150),ylim=c(0,20),
#                      xlab='Distance (meter)',ylab=expression(paste(sqrt('Semi-variance'),' (days)')),
#                      cex.lab=1.5,cex.axis=1.3,
#                      pch=ppch[i],cex=1.3,axe=F)
#                 abline(v=30,lty=5,lwd=1.5)
#                 box()
#                 axis(1,at=seq(0,150,30),cex.axis=1.5)
#                 axis(2,at=seq(0,20,5),cex.axis=1.5)    
#                 legend('bottomright',
#                        c('15% greenup','50% greenup','90% greenup','Peak greenness',
#                          '90% greendown','50% greendown','15% greendown'),
#                        pch=ppch,cex=1.3,bty='n',pt.cex=1.5)        
#         }else{
#                 points(variAGall[[i]]$dist,sqrt(variAGall[[i]]$gamma),cex=1.3,pch=ppch[i])
#         }
# }
# 
# dev.off()
# #################


#####################################################3
# all phenometrics & all site
vgt <- c('DB','MF','EN','AG','GR','SH')

variDBall <- vector('list',7)
variMFall <- vector('list',7)
variENall <- vector('list',7)
variAGall <- vector('list',7)
variGRall <- vector('list',7)
variSHall <- vector('list',7)

nuggets <- matrix(NA,6,7)

#
for(j in 1:6){
        for(i in 1:7){
                rast <- raster(paste('/projectnb/modislc/users/mkmoon/Planet/phe/2019/',vgt[j],'/phe_Planet_2019_',vgt[j],'_',i,'.tif',sep=''))
                xy <- cbind(rep(seq(rast@extent@xmin+1.5,rast@extent@xmax+1.5,3),1001),
                            rep(seq(rast@extent@ymin+1.5,rast@extent@ymax+1.5,3),each=1001))
                xy <- data.frame(xy)
                names(xy) <- c('x','y')
                dat <- cbind(extract(rast, xy, df = T),xy)
                dat <- na.omit(dat)
                dat <- dat[sample(1:dim(dat)[1],100000),]
                names(dat) <- c('ID','date','x','y')
                coordinates(dat)=~x+y
                class(dat)
                
                if(j==1){
                        variDBall[[i]]  <- variogram(date~1,data=dat,width=1.5,cutoff=1000)
                        FittedModel <- fit.variogram(variDBall[[i]], model=TheVariogramModel)
                }else if(j==2){
                        variMFall[[i]]  <- variogram(date~1,data=dat,width=1.5,cutoff=1000)  
                        FittedModel <- fit.variogram(variMFall[[i]], model=TheVariogramModel)
                }else if(j==3){
                        variENall[[i]]  <- variogram(date~1,data=dat,width=1.5,cutoff=1000)  
                        FittedModel <- fit.variogram(variENall[[i]], model=TheVariogramModel)
                }else if(j==4){
                        variAGall[[i]]  <- variogram(date~1,data=dat,width=1.5,cutoff=1000)  
                        FittedModel <- fit.variogram(variAGall[[i]], model=TheVariogramModel)
                }else if(j==5){
                        variGRall[[i]]  <- variogram(date~1,data=dat,width=1.5,cutoff=1000)  
                        FittedModel <- fit.variogram(variGRall[[i]], model=TheVariogramModel)
                }else{
                        variSHall[[i]]  <- variogram(date~1,data=dat,width=1.5,cutoff=1000)  
                        FittedModel <- fit.variogram(variSHall[[i]], model=TheVariogramModel)
                }
                
                nuggets[j,i] <- FittedModel$psill[1]      
                
                print(paste(j,' ;',i))
        }
}

# setwd('/projectnb/modislc/users/mkmoon/Planet/data/')
# save(variDBall,variMFall,variENall,
#      variAGall,variGRall,variSHall,
#      nuggets,
#      file='semivari_phe_all_sites.rda')



#################
load('/projectnb/modislc/users/mkmoon/Planet/data/semivari_phe_all_sites.rda')

setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
png(filename='semivariance_phe_all_sites_0.png',width=12,height=8,unit='in',res=300)

par(mfrow=c(2,3),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.3,1,0))
ppch <- c(21,22,23,24,23,22,21)
mycol <- rev(brewer.pal(11,'RdYlGn'))
mycol <- c(mycol[1:3],mycol[6],mycol[9:11])
for(j in 1:6){
        if(j==1){
                dat <- variDBall
        }else if(j==2){
                dat <- variMFall
        }else if(j==3){
                dat <- variENall
        }else if(j==4){
                dat <- variAGall
        }else if(j==5){
                dat <- variGRall
        }else{
                dat <- variSHall
        }
        for(i in 1:7){
                if(i==1){
                        plot(dat[[i]]$dist,sqrt(dat[[i]]$gamma),xlim=c(0,90),ylim=c(0,35),
                             xlab='Distance (meter)',ylab=expression(paste(sqrt('Semi-variance'),' (days)')),
                             cex.lab=1.5,cex.axis=1.3,
                             pch=ppch[i],cex=1.8,axe=F,bg=mycol[i])
                        abline(v=30,lty=5,lwd=1.5)
                        box()
                        axis(1,at=seq(0,400,30),cex.axis=1.5)
                        axis(2,at=seq(0,50,10),cex.axis=1.5)
                }else{
                        points(dat[[i]]$dist,sqrt(dat[[i]]$gamma),cex=1.8,pch=ppch[i],bg=mycol[i])        
                }
                if(j==2){
                        legend('topright',
                               c('15% greenup','50% greenup','90% greenup','Peak greenness',
                                 '90% greendown','50% greendown','15% greendown'),
                               pch=ppch,cex=1.5,bty='n',pt.cex=1.8,pt.bg=mycol)
                }
        }
        
}

dev.off()
#################

ppch <- c(21,22,23,24,23,22,21)
mycol <- rev(brewer.pal(11,'RdYlGn'))
mycol <- c(mycol[1:3],mycol[6],mycol[9:11])

setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
png(filename='semivariance_nugget.png',width=9,height=7,unit='in',res=300)

par(oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.8,1,0))
for(i in 1:6){
        if(i==1){
                plot(1:6,sqrt(nuggets)[,i],ylim=c(0,22),pch=ppch[i],cex=1.8,bg=mycol[i],axe=F,
                     xlab='',ylab='Nugget (days)',cex.lab=1.5)                
                lines(1:6,sqrt(nuggets)[,i],col=mycol[i])                
        }else{
                points(1:6,sqrt(nuggets)[,i],pch=ppch[i],cex=1.8,bg=mycol[i])
                lines(1:6,sqrt(nuggets)[,i],col=mycol[i])                
        }
}
box()
axis(1,at=1:6,c('DB','MF','EN','AG','GR','SH'),cex.axis=1.5)
axis(2,at=seq(0,30,5),cex.axis=1.5)
legend('topleft',
       c('15% greenup','50% greenup','90% greenup','Peak greenness',
         '90% greendown','50% greendown','15% greendown'),
       pch=ppch,cex=1.3,bty='n',pt.cex=1.8,pt.bg=mycol)

dev.off()

# #####################################################3
# # EVIamp 
# 
# # Harvard Forest
# rast <- raster('/projectnb/modislc/users/mkmoon/Planet/phe/2019/MF/phe_Planet_2019_amp_MF.tif')
# xy <- cbind(rep(seq(rast@extent@xmin+1.5,rast@extent@xmax+1.5,3),1001),
#             rep(seq(rast@extent@ymin+1.5,rast@extent@ymax+1.5,3),each=1001))
# xy <- data.frame(xy)
# names(xy) <- c('x','y')
# dat <- cbind(extract(rast, xy, df = T),xy)
# dat <- na.omit(dat)
# dat <- dat[sample(1:dim(dat)[1],100000),]
# coordinates(dat)=~x+y
# class(dat)
# 
# variMF  <- variogram(phe_Planet_2019_amp_MF~1,data=dat,width=1.5,cutoff=1000)
# 
# # UIEF
# rast <- raster('/projectnb/modislc/users/mkmoon/Planet/phe/2019/AG/phe_Planet_2019_amp_AG.tif')
# xy <- cbind(rep(seq(rast@extent@xmin+1.5,rast@extent@xmax+1.5,3),1001),
#             rep(seq(rast@extent@ymin+1.5,rast@extent@ymax+1.5,3),each=1001))
# xy <- data.frame(xy)
# names(xy) <- c('x','y')
# dat <- cbind(extract(rast, xy, df = T),xy)
# dat <- na.omit(dat)
# dat <- dat[sample(1:dim(dat)[1],100000),]
# coordinates(dat)=~x+y
# class(dat)
# 
# variAG  <- variogram(phe_Planet_2019_amp_AG~1,data=dat,width=1.5,cutoff=1000)
# # plot(variAGs$dist,variAGs$gamma,xlim=c(0,1000))
# 
# 
# #################
# setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
# png(filename='semivariance_amp_0.png',width=13,height=6,unit='in',res=300)
# 
# par(mfrow=c(1,2),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.3,1,0))
# plot(variMF$dist,sqrt(variMF$gamma),xlim=c(0,150),ylim=c(0.01,0.13),
#      xlab='Distance (meter)',ylab=expression(sqrt('Semi-variance')),
#      cex.lab=1.5,cex.axis=1.3,
#      pch=19,cex=1.3)
# plot(variAG$dist,sqrt(variAG$gamma),xlim=c(0,150),ylim=c(0.01,0.13),
#      xlab='Distance (meter)',ylab=expression(sqrt('Semi-variance')),
#      cex.lab=1.5,cex.axis=1.3,
#      pch=19,cex=1.3)
# 
# dev.off()
# #################
# 
# #################
# setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
# png(filename='semivariance_amp_2.png',width=6.5,height=6,unit='in',res=300)
# 
# par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.3,1,0))
# plot(variMF$dist,sqrt(variMF$gamma),xlim=c(0,120),ylim=c(0.01,0.13),
#      xlab='Distance (meter)',ylab=expression(sqrt('Semi-variance')),
#      cex.lab=1.5,cex.axis=1.3,
#      pch=19,cex=1.3,axe=F)
# points(variAG$dist,sqrt(variAG$gamma),pch=2,cex=1.3)
# abline(v=30,lty=5,lwd=1.5)
# box()
# axis(1,at=seq(0,120,30),cex.axis=1.5)
# axis(2,at=seq(0,20,0.03),cex.axis=1.5)
# legend('topleft',c('MF','AG'),pch=c(19,2),cex=1.7,bty='n',pt.cex=1.3)
# 
# dev.off()
# #################
# 
# 
# 
# #####################################################3
# # EVI at exact date
# 
# ## MF
# imgDir <- '/projectnb/modislc/users/mkmoon/Planet/data/Planet_SR/MF/files'
# path <- imgDir
# 
# # Base image
# fileSR <- list.files(path=path,pattern=glob2rx('*SR*.tif'))
# fileDN <- list.files(path=path,pattern=glob2rx('*DN*.tif'))
# 
# yy <- substr(fileSR,3,4)
# mm <- substr(fileSR,5,6)
# dd <- substr(fileSR,7,8)
# dates_all <- as.Date(paste(mm,'/',dd,'/',yy,sep=''),'%m/%d/%y')
# dates <- unique(dates_all)
# 
# fileSR <- list.files(path=path,pattern=glob2rx('*SR*.tif'),full.names=T)
# fileDN <- list.files(path=path,pattern=glob2rx('*DN*.tif'),full.names=T)
# 
# imgFull <- NULL
# for(i in 1:length(dates)){
#         ids <- which(dates_all==dates[i])  
#         if(length(ids)==1){
#                 imgFull <- i
#         }
# }
# imgBase <- raster(fileSR[which(dates_all==dates[imgFull])]) 
# 
# # Dates
# fileSR <- list.files(path=path,pattern=glob2rx('*20190518*SR*.tif'),full.names=T)
# fileDN <- list.files(path=path,pattern=glob2rx('*20190518*DN*.tif'),full.names=T)
# 
# temp <- vector('list',length(fileSR))
# for(j in 1:length(fileSR)){
# red <- raster(fileSR[j],band=3)/10000
#         nir <- raster(fileSR[j],band=4)/10000
#         udm <- raster(fileDN[j])
#         vis <- 2.5*(nir-red)/(nir+2.4*red+1)
#         vis[udm>2|is.na(udm)] <- NA
#         temp[[j]] <- vis
# }
# for(rr in 1:length(fileSR)){
#         log <- try(compareRaster(temp[[rr]],imgBase,extent=F,rowcol=F),
#                    silent=T)
# if(inherits(log,'try-error')){
#         temp[[rr]] <- projectRaster(temp[[rr]],imgBase)    
#         }
# }
# temp$fun <- mean
# temp$na.rm <- T
# rast <- do.call(mosaic,temp)  
# 
# xy <- cbind(rep(seq(rast@extent@xmin+1.5,rast@extent@xmax+1.5,3),1001),
#             rep(seq(rast@extent@ymin+1.5,rast@extent@ymax+1.5,3),each=1001))
# xy <- data.frame(xy)
# names(xy) <- c('x','y')
# dat <- cbind(extract(rast, xy, df = T),xy)
# dat <- na.omit(dat)
# dat <- dat[sample(1:dim(dat)[1],100000),]
# coordinates(dat)=~x+y
# class(dat)
# 
# variMFd  <- variogram(layer~1,data=dat,width=1.5,cutoff=1000)
# # plot(variMFd)
# 
# 
# ## AG
# imgDir <- '/projectnb/modislc/users/mkmoon/Planet/data/Planet_SR/AG/files'
# path <- imgDir
# 
# # Base image
# fileSR <- list.files(path=path,pattern=glob2rx('*SR*.tif'))
# fileDN <- list.files(path=path,pattern=glob2rx('*DN*.tif'))
# 
# yy <- substr(fileSR,3,4)
# mm <- substr(fileSR,5,6)
# dd <- substr(fileSR,7,8)
# dates_all <- as.Date(paste(mm,'/',dd,'/',yy,sep=''),'%m/%d/%y')
# dates <- unique(dates_all)
# 
# fileSR <- list.files(path=path,pattern=glob2rx('*SR*.tif'),full.names=T)
# fileDN <- list.files(path=path,pattern=glob2rx('*DN*.tif'),full.names=T)
# 
# imgFull <- NULL
# for(i in 1:length(dates)){
#         ids <- which(dates_all==dates[i])  
#         if(length(ids)==1){
#                 imgFull <- i
#         }
# }
# imgBase <- raster(fileSR[which(dates_all==dates[imgFull])]) 
# 
# # Dates
# fileSR <- list.files(path=path,pattern=glob2rx('*20190610*SR*.tif'),full.names=T)
# fileDN <- list.files(path=path,pattern=glob2rx('*20190610*DN*.tif'),full.names=T)
# 
# temp <- vector('list',length(fileSR))
# for(j in 1:length(fileSR)){
#         red <- raster(fileSR[j],band=3)/10000
#         nir <- raster(fileSR[j],band=4)/10000
#         udm <- raster(fileDN[j])
#         vis <- 2.5*(nir-red)/(nir+2.4*red+1)
#         vis[udm>2|is.na(udm)] <- NA
#         temp[[j]] <- vis
# }
# for(rr in 1:length(fileSR)){
#         log <- try(compareRaster(temp[[rr]],imgBase,extent=F,rowcol=F),
#                    silent=T)
#         if(inherits(log,'try-error')){
#                 temp[[rr]] <- projectRaster(temp[[rr]],imgBase)    
#         }
# }
# temp$fun <- mean
# temp$na.rm <- T
# rast <- do.call(mosaic,temp)  
# 
# xy <- cbind(rep(seq(rast@extent@xmin+1.5,rast@extent@xmax+1.5,3),1001),
#             rep(seq(rast@extent@ymin+1.5,rast@extent@ymax+1.5,3),each=1001))
# xy <- data.frame(xy)
# names(xy) <- c('x','y')
# dat <- cbind(extract(rast, xy, df = T),xy)
# dat <- na.omit(dat)
# dat <- dat[sample(1:dim(dat)[1],100000),]
# coordinates(dat)=~x+y
# class(dat)
# 
# variAGd  <- variogram(layer~1,data=dat,width=1.5,cutoff=1000)
# # plot(variAGd)
# 
# # setwd('/projectnb/modislc/users/mkmoon/Planet/data/')
# # save(variMFd,variAGd,
# #      file='semivari_evi_0.rda')
# 
# #################
# load('/projectnb/modislc/users/mkmoon/Planet/data/semivari_evi_0.rda')
# 
# setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
# png(filename='semivariance_evi_1.png',width=6.5,height=6,unit='in',res=300)
# 
# par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.3,1,0))
# plot(variMFd$dist,sqrt(variMFd$gamma),xlim=c(0,150),ylim=c(0,0.09),
#      xlab='Distance (meter)',ylab=expression(sqrt('Semi-variance')),
#      cex.lab=1.5,cex.axis=1.3,
#      pch=19,cex=1.3,axe=F)
# points(variAGd$dist,sqrt(variAGd$gamma),pch=2,cex=1.3)
# abline(v=30,lty=5,lwd=1.5)
# box()
# axis(1,at=seq(0,150,30),cex.axis=1.5)
# axis(2,at=seq(0,20,0.03),cex.axis=1.5)
# legend('topleft',c('MF','AG'),pch=c(19,2),cex=1.7,bty='n',pt.cex=1.3)
# 
# dev.off()
# #################




#####################################################3
# EVI at exact date, all sites
vgt <- c('DB','MF','EN','AG','GR','SH')

variAlld <- vector('list',6)

for(jj in 1:6){
        imgDir <- paste('/projectnb/modislc/users/mkmoon/Planet/data/Planet_SR/',vgt[jj],'/files',sep='')
        path <- imgDir
        
        # Base image
        fileSR <- list.files(path=path,pattern=glob2rx('*SR*.tif'))
        fileDN <- list.files(path=path,pattern=glob2rx('*DN*.tif'))
        
        yy <- substr(fileSR,3,4)
        mm <- substr(fileSR,5,6)
        dd <- substr(fileSR,7,8)
        dates_all <- as.Date(paste(mm,'/',dd,'/',yy,sep=''),'%m/%d/%y')
        dates <- unique(dates_all)
        
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
        
        # Dates
        if(jj==1){
                # fileSR <- list.files(path=path,pattern=glob2rx('*20190507*SR*.tif'),full.names=T)
                # fileDN <- list.files(path=path,pattern=glob2rx('*20190507*DN*.tif'),full.names=T)                       
                fileSR <- list.files(path=path,pattern=glob2rx('*20190502*SR*.tif'),full.names=T)
                fileDN <- list.files(path=path,pattern=glob2rx('*20190502*DN*.tif'),full.names=T)
        }else if(jj==2){
                fileSR <- list.files(path=path,pattern=glob2rx('*20190518*SR*.tif'),full.names=T)
                fileDN <- list.files(path=path,pattern=glob2rx('*20190518*DN*.tif'),full.names=T)                
        }else if(jj==3){
                # fileSR <- list.files(path=path,pattern=glob2rx('*20190505*SR*.tif'),full.names=T)
                # fileDN <- list.files(path=path,pattern=glob2rx('*20190505*DN*.tif'),full.names=T)                
                fileSR <- list.files(path=path,pattern=glob2rx('*20190512*SR*.tif'),full.names=T)
                fileDN <- list.files(path=path,pattern=glob2rx('*20190512*DN*.tif'),full.names=T)
        }else if(jj==4){
                fileSR <- list.files(path=path,pattern=glob2rx('*20190610*SR*.tif'),full.names=T)
                fileDN <- list.files(path=path,pattern=glob2rx('*20190610*DN*.tif'),full.names=T)
        }else if(jj==5){
                # fileSR <- list.files(path=path,pattern=glob2rx('*20190504*SR*.tif'),full.names=T)
                # fileDN <- list.files(path=path,pattern=glob2rx('*20190504*DN*.tif'),full.names=T)                
                fileSR <- list.files(path=path,pattern=glob2rx('*20190517*SR*.tif'),full.names=T)
                fileDN <- list.files(path=path,pattern=glob2rx('*20190517*DN*.tif'),full.names=T)
        }else{
                fileSR <- list.files(path=path,pattern=glob2rx('*20190505*SR*.tif'),full.names=T)
                fileDN <- list.files(path=path,pattern=glob2rx('*20190505*DN*.tif'),full.names=T)
        }
        
        
        temp <- vector('list',length(fileSR))
        for(j in 1:length(fileSR)){
                red <- raster(fileSR[j],band=3)/10000
                nir <- raster(fileSR[j],band=4)/10000
                udm <- raster(fileDN[j])
                vis <- 2.5*(nir-red)/(nir+2.4*red+1)
                vis[udm>2|is.na(udm)] <- NA
                temp[[j]] <- vis
        }
        for(rr in 1:length(fileSR)){
                log <- try(compareRaster(temp[[rr]],imgBase,extent=F,rowcol=F),
                           silent=T)
                if(inherits(log,'try-error')){
                        temp[[rr]] <- projectRaster(temp[[rr]],imgBase)    
                }
        }
        temp$fun <- mean
        temp$na.rm <- T
        rast <- do.call(mosaic,temp)  
        
        # ###
        # ext <- extent(rast@extent@xmin+1000,rast@extent@xmin+2000,
        #               rast@extent@ymin+1000,rast@extent@ymin+2000)
        # rast <- crop(rast,ext)
        # ###
        
        xy <- cbind(rep(seq(rast@extent@xmin+1.5,rast@extent@xmax+1.5,3),1001),
                    rep(seq(rast@extent@ymin+1.5,rast@extent@ymax+1.5,3),each=1001))
        xy <- data.frame(xy)
        names(xy) <- c('x','y')
        dat <- cbind(extract(rast, xy, df = T),xy)
        dat <- na.omit(dat)
        dat <- dat[sample(1:dim(dat)[1],10000),]
        coordinates(dat)=~x+y
        class(dat)
        
        variAlld[[jj]]  <- variogram(layer~1,data=dat,width=1.5,cutoff=1000)               
        print(jj)
}

setwd('/projectnb/modislc/users/mkmoon/Planet/data/')
save(variAlld,
     file='semivari_evi_all_sites_1.rda')

#################
load('/projectnb/modislc/users/mkmoon/Planet/data/semivari_evi_all_sites.rda')

setwd('/projectnb/modislc/users/mkmoon/Planet/figure/')
png(filename='semivariance_evi_all_sites_0.png',width=6.5,height=6,unit='in',res=300)

vgt <- c('DB','MF','EN','AG','GR','SH')
ppch <- c(15,19,17,0,3,4)
mycol <- brewer.pal(7,'Set1')
mycol <- c(mycol[1:5],mycol[7])
par(mfrow=c(1,1),oma=c(1,1,1,1),mar=c(4,4,1,1),mgp=c(2.3,1,0))
plot(variAlld[[1]]$dist,sqrt(variAlld[[1]]$gamma),xlim=c(0,150),ylim=c(0,0.09),
     xlab='Distance (meter)',ylab=expression(sqrt('Semi-variance')),
     cex.lab=1.5,cex.axis=1.5,
     pch=21,cex=1.6,axe=F,bg=mycol[1])
for(i in 2:6){
        points(variAlld[[i]]$dist,sqrt(variAlld[[i]]$gamma),pch=21,cex=1.6,bg=mycol[i])        
}
abline(v=30,lty=5,lwd=1.5)
box()
axis(1,at=seq(0,150,30),cex.axis=1.5)
axis(2,at=seq(0,20,0.03),cex.axis=1.5)
legend('topleft',vgt,pch=21,cex=1.4,bty='n',pt.cex=1.6,pt.bg=mycol)

dev.off()
#################
