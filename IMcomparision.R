#Program to compare various Computer Vision techniques on different data-sets
#Written by AA on 30July2018

setwd("/media/aakanksha/f41d5ac2-703c-4b56-a960-cd3a54f21cfb/aakanksha/Documents/Backup/Phd/Analysis/Chapter1")
#Read data file
dat = read.csv("Ipcomparison_exbound.csv") 
dat=dat[which(!is.na(dat$failed.percent) & !is.na(dat$false.percent)),]

head(dat)
summary(dat)

colnames(dat)
library("ggplot2")
library("hexbin")
library("gridExtra")
####################################################################################################
#1. Comparision of error across techniques
####################################################################################################
#Variables to quantify variation
rname = c("BBfail","BBfalse","Ffail","Ffalse","Wfail","Wfalse");
cname = c("Seg","Sub","NN");
avgVar = matrix(data=NA,nrow=length(rname),ncol=length(cname),dimnames=list(rname,cname)); #average of variation within videos
varMean = matrix(data=NA,nrow=length(rname),ncol=length(cname),dimnames=list(rname,cname));# Variation in means
Fstat = matrix(data=NA,nrow=length(rname),ncol=length(cname),dimnames=list(rname,cname)); # varMean/avgVar = var between tratments/var withi treatments
# lev=paste(dat$Method.Name," ",dat$Species)
# n1=c("turquoise","orange") #1:length(unique(dat$Method.Name))
# n2=length(unique(dat$Species))
# clr=rep(n1,each=n2)

## For Fish
datN=NA
datN=dat[which(dat$Species=="Fish" & dat$Boundary != 1),]

#Graphs for technique comparision
#Boxplots
means <- tapply(datN$failed.percent,datN$Method.Name,mean)
failed = ggplot(datN, aes(x = Method.Name, y = (failed.percent*100),fill=datN$Method.Name)) +
         labs(y="% failed detections",x="Technique name") +
         geom_boxplot(varwidth=TRUE,width=0.1, outlier.shape = 1, show.legend = FALSE,notch=TRUE) + 
         scale_fill_manual(values = c('turquoise','orange','pink')) +
         stat_summary(fun.y=mean, colour="darkred", geom="point",shape=18, size=3,show_guide = FALSE) +
         #geom_hline(yintercept=20,color='orangered') +
         coord_flip()

means <- tapply(datN$false.percent,datN$Method.Name,mean)
false = ggplot(datN, aes(x = Method.Name, y = (false.percent*100),fill=datN$Method.Name)) +
  labs(y="% false detections",x="") +
  geom_boxplot(varwidth=TRUE,width=0.1, outlier.shape = 1, show.legend = FALSE,notch=TRUE) + 
  scale_fill_manual(values = c('turquoise','orange','pink')) +
  stat_summary(fun.y=mean, colour="darkred", geom="point",shape=18, size=3,show_guide = FALSE) +
  #geom_hline(yintercept=20,color='orangered') +
  coord_flip()


#Error histograms
xdensity <- ggplot(datN, aes(x = failed.percent*100, fill=datN$Method.Name)) + 
  labs(x="",y="") +
  geom_density(alpha=.5, show.legend = FALSE) + 
  scale_fill_manual(values = c('turquoise','orange','pink')) #+ geom_vline(xintercept=20,color='orangered')
ydensity <- ggplot(datN, aes(x = false.percent*100, fill=datN$Method.Name)) + 
  labs(x="",y="") +
  geom_density(alpha=.5, show.legend = FALSE) + 
  scale_fill_manual(values = c('turquoise','orange','pink')) #+ geom_vline(xintercept=20,color='orangered')

#arrange all graphs together
#grid.arrange(xdensity, ydensity,failed,false, nrow = 2,heights = c(3, 2))
library(cowplot)
png(filename="Figures/Fish_techniques.png",width=1280,height=720,units="px",res=150)
plot_grid(xdensity, ydensity,failed,false, nrow = 2, align="v",labels=c('A','B','C','D'))
dev.off();



# Error landscape
png(filename="Figures/Fish_ErrorLandscape.png",width=720,height=480,units="px",pointsize = 16,res=150)
ggplot(datN[datN$Method.Name == "Segmentation" | datN$Method.Name == "NN",], aes (x = failed.percent, y = false.percent,main="Fish", col=Method.Name,pch=Method.Name),geom='quasirandom') + 
  labs(x="% failed detections",y="% false detections",color="",pch="") + ylim(c(0,1))+ xlim(c(0,1))+
  stat_density2d()+
  geom_point(data=datN[datN$Method.Name == "Subtraction",], aes (x = failed.percent, y = false.percent))
dev.off();

#ggplot(datN, aes (x = failed.percent, y = false.percent,main="Fish", col=Method.Name,pch=Method.Name)) + 
# labs(x="% failed detections",y="% false detections",color="",pch="") +
#   geom_point()


## For blackbuck
rm(datN)
datN=dat[which(dat$Species=="Blackbuck"),]


#Graphs for technique comparision
#Boxplots
means <- tapply(datN$failed.percent,datN$Method.Name,mean)
failed = ggplot(datN, aes(x = Method.Name, y = (failed.percent*100),fill=datN$Method.Name)) +
  labs(y="% failed detections",x="Technique name") +
  geom_boxplot(varwidth=TRUE,width=0.1, outlier.shape = 1, show.legend = FALSE,notch=TRUE) + 
  scale_fill_manual(values = c('turquoise','orange','pink')) +
  stat_summary(fun.y=mean, colour="darkred", geom="point",shape=18, size=3,show_guide = FALSE) +
  #geom_hline(yintercept=20,color='orangered') +
  coord_flip()

means <- tapply(datN$false.percent,datN$Method.Name,mean)
false = ggplot(datN, aes(x = Method.Name, y = (false.percent*100),fill=datN$Method.Name)) +
  labs(y="% false detections",x="") +
  geom_boxplot(varwidth=TRUE,width=0.1, outlier.shape = 1, show.legend = FALSE,notch=TRUE) + 
  scale_fill_manual(values = c('turquoise','orange','pink')) +
  stat_summary(fun.y=mean, colour="darkred", geom="point",shape=18, size=3,show_guide = FALSE) +
  #geom_hline(yintercept=20,color='orangered') +
  coord_flip()


#Error histograms
xdensity <- ggplot(datN, aes(x = failed.percent*100, fill=datN$Method.Name)) + 
  labs(x="",y="") +
  geom_density(alpha=.5, show.legend = FALSE) + 
  scale_fill_manual(values = c('turquoise','orange','pink')) #+ geom_vline(xintercept=20,color='orangered')
ydensity <- ggplot(datN, aes(x = false.percent*100, fill=datN$Method.Name)) + 
  labs(x="",y="") +
  geom_density(alpha=.5, show.legend = FALSE) + 
  scale_fill_manual(values = c('turquoise','orange','pink')) #+ geom_vline(xintercept=20,color='orangered')

#arrange all graphs together
#grid.arrange(xdensity, ydensity,failed,false, nrow = 2,heights = c(3, 2))
library(cowplot)
png(filename="Figures/BB_techniques.png",width=1280,height=720,units="px",res=150)
plot_grid(xdensity, ydensity,failed,false, nrow = 2, align="v",labels=c('A','B','C','D'))
dev.off();

# Error landscape
png(filename="Figures/BB_ErrorLandscape.png",width=720,height=480,units="px",pointsize = 16,res=150)
ggplot(datN, aes (x = failed.percent, y = false.percent,main="FBlackbuck", col=Method.Name,pch=Method.Name)) + 
  stat_density_2d(aes( geom = "polygon")) + ylim(c(0,1))+xlim(c(0,1))+  labs(x="% failed detections",y="% false detections",color="",pch="")
dev.off();

## For Wasp
rm(datN)
datN=dat[which(dat$Species=="Wasp"),]


#Graphs for technique comparision
#Boxplots
means <- tapply(datN$failed.percent,datN$Method.Name,mean)
failed = ggplot(datN, aes(x = Method.Name, y = (failed.percent*100),fill=datN$Method.Name)) +
  labs(y="% failed detections",x="Technique name") +
  geom_boxplot(varwidth=TRUE,width=0.1, outlier.shape = 1, show.legend = FALSE,notch=TRUE) + 
  scale_fill_manual(values = c('turquoise','orange','pink')) +
  stat_summary(fun.y=mean, colour="darkred", geom="point",shape=18, size=3,show_guide = FALSE) +
  #geom_hline(yintercept=20,color='orangered') +
  coord_flip()

means <- tapply(datN$false.percent,datN$Method.Name,mean)
false = ggplot(datN, aes(x = Method.Name, y = (false.percent*100),fill=datN$Method.Name)) +
  labs(y="% false detections",x="") +
  geom_boxplot(varwidth=TRUE,width=0.1, outlier.shape = 1, show.legend = FALSE,notch=TRUE) + 
  scale_fill_manual(values = c('turquoise','orange','pink')) +
  stat_summary(fun.y=mean, colour="darkred", geom="point",shape=18, size=3,show_guide = FALSE) +
  #geom_hline(yintercept=20,color='orangered') +
  coord_flip()


#Error histograms
xdensity <- ggplot(datN, aes(x = failed.percent*100, fill=datN$Method.Name)) + 
  labs(x="",y="") +
  geom_density(alpha=.5, show.legend = FALSE) + 
  scale_fill_manual(values = c('turquoise','orange','pink')) #+ geom_vline(xintercept=20,color='orangered')
ydensity <- ggplot(datN, aes(x = false.percent*100, fill=datN$Method.Name)) + 
  labs(x="",y="") +
  geom_density(alpha=.5, show.legend = FALSE) + 
  scale_fill_manual(values = c('turquoise','orange','pink')) #+ geom_vline(xintercept=20,color='orangered')

#arrange all graphs together
#grid.arrange(xdensity, ydensity,failed,false, nrow = 2,heights = c(3, 2))
library(cowplot)
png(filename="Figures/Wasp_techniques.png",width=1280,height=720,units="px",res=150)
plot_grid(xdensity, ydensity,failed,false, nrow = 2, align="v",labels=c('A','B','C','D'))
dev.off();

# Error landscape
png(filename="Figures/Wasp_ErrorLandscape.png",width=720,height=480,units="px",pointsize = 16,res=150)
ggplot(datN[datN$Method.Name == "Segmentation" | datN$Method.Name == "Subtraction",], aes (x = failed.percent, y = false.percent,main="Fish", col=Method.Name,pch=Method.Name),geom='quasirandom') + 
  labs(x="% failed detections",y="% false detections",color="",pch="") + ylim(c(0,1))+ xlim(c(0,1))+
  stat_density2d()+
  geom_point(data=datN[datN$Method.Name == "NN",], aes (x = failed.percent, y = false.percent))
dev.off();


#####################################################################################################3
#2. Variation inerrors~ video id
######################################################################################################3
#Graphs - video id is predictor variable


##############################Segmentation#####################################
## For blackbuck
rm(datN)
datN<-dat[which(dat$Species=="Blackbuck" & dat$Method.Name=="Segmentation"),]
datN$VideoName=factor(datN$VideoName) #Remove undesired factors
m <- tapply(X=datN$failed.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=names(m)[o])

##Measure of variability across and within videos
lmean=tapply(datN$failed.percent,datN$VideoName,mean)
lvar=tapply(datN$failed.percent,datN$VideoName,var)
lmean=as.numeric(paste(unlist(lmean)))
lvar=as.numeric(paste(unlist(lvar)))
VMfailed = round(mean(na.omit(lvar)),2)
avgVar["BBfail","Seg"] = mean(na.omit(lvar))
varMean["BBfail","Seg"] = var(na.omit(lmean))

lmean=tapply(datN$false.percent,datN$VideoName,mean)
lvar=tapply(datN$false.percent,datN$VideoName,var)
lmean=as.numeric(paste(unlist(lmean)))
lvar=as.numeric(paste(unlist(lvar)))
VMfalse = round(mean(na.omit(lvar)),2)
avgVar["BBfalse","Seg"] = mean(na.omit(lvar))
varMean["BBfalse","Seg"] = var(na.omit(lmean))


par(mfrow=c(3,2))

Fstat = round(varMean/avgVar,digits=2)

bb_seg1=boxplot(datN$failed.percent*100~datN$VideoName,ylab="% failed detections",xaxt="n",notch=TRUE,col="cyan",las=2,main=paste("Fstat = ",Fstat["BBfail","Seg"]))
m <- tapply(X=datN$false.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=names(m)[o])
bb_seg2=boxplot(datN$false.percent*100~datN$VideoName,ylab="% false detections",xaxt="n",notch=TRUE,col="cyan",las=2,main=paste("Fstat = ",Fstat["BBfalse","Seg"]))
par(xpd=TRUE)



##Fish
datN=NA
datN=dat[which(dat$Species=="Fish" & dat$Method.Name=="Segmentation" & dat$Boundary != 1),]
datN$VideoName=factor(datN$VideoName) #Remove undesired factors
m <- tapply(X=datN$failed.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=names(m)[o])

##Measure of variability across and within videos

lmean=tapply(datN$failed.percent,datN$VideoName,mean)
lvar=tapply(datN$failed.percent,datN$VideoName,var)
lmean=as.numeric(paste(unlist(lmean)))
lvar=as.numeric(paste(unlist(lvar)))
VMfailed = round(mean(na.omit(lvar)),2)
avgVar["Ffail","Seg"] = mean(na.omit(lvar))
varMean["Ffail","Seg"] = var(na.omit(lmean))

lmean=tapply(datN$false.percent,datN$VideoName,mean)
lvar=tapply(datN$false.percent,datN$VideoName,var)
lmean=as.numeric(paste(unlist(lmean)))
lvar=as.numeric(paste(unlist(lvar)))
VMfalse = round(mean(na.omit(lvar)),2)
avgVar["Ffalse","Seg"] = mean(na.omit(lvar))
varMean["Ffalse","Seg"] = var(na.omit(lmean))


Fstat = round(varMean/avgVar,digits=2)

fish_seg1=boxplot(datN$failed.percent*100~datN$VideoName,ylab="% failed detections",xaxt="n",notch=TRUE,col="pink",las=2,main=paste("Fstat = ",Fstat["Ffail","Seg"]))
m <- tapply(X=datN$false.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=names(m)[o])
fish_seg2=boxplot(datN$false.percent*100~datN$VideoName,ylab="% false detections",xaxt="n",notch=TRUE,col="pink",las=2,main=paste("Fstat = ",Fstat["Ffalse","Seg"]))
par(xpd=TRUE)
mtext("Fish", outer =F, cex = 0.5)

##Wasp


rm(datN)
datN=dat[which(dat$Species=="Wasp" & dat$Method.Name=="Segmentation"),]
datN$VideoName=factor(datN$VideoName) #Remove undesired factors
m <- tapply(X=datN$failed.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=names(m)[o])

##Measure of variability across and within videos

lmean=tapply(datN$failed.percent,datN$VideoName,mean)
lvar=tapply(datN$failed.percent,datN$VideoName,var)
lmean=as.numeric(paste(unlist(lmean)))
lvar=as.numeric(paste(unlist(lvar)))
VMfailed = round(mean(na.omit(lvar)),2)
avgVar["Wfail","Seg"] = mean(na.omit(lvar))
varMean["Wfail","Seg"] = var(na.omit(lmean))

lmean=tapply(datN$false.percent,datN$VideoName,mean)
lvar=tapply(datN$false.percent,datN$VideoName,var)
lmean=as.numeric(paste(unlist(lmean)))
lvar=as.numeric(paste(unlist(lvar)))
VMfalse = round(mean(na.omit(lvar)),2)
avgVar["Wfalse","Seg"] = mean(na.omit(lvar))
varMean["Wfalse","Seg"] = var(na.omit(lmean))

Fstat = round(varMean/avgVar,digits=2)

wasp_seg1=boxplot(datN$failed.percent*100~datN$VideoName,ylab="% failed detections",xaxt="n",notch=TRUE,col="orange",las=2,main=paste("Fstat = ",Fstat["Wfail","Seg"]))
m <- tapply(X=datN$false.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=names(m)[o])
wasp_seg2=boxplot(datN$false.percent*100~datN$VideoName,ylab="% false detections",xaxt="n",notch=TRUE,col="orange",las=2,main=paste("Fstat = ",Fstat["Wfalse","Seg"]))
par(xpd=TRUE)


#########################################################################################

##############################Subtraction#####################################
## For blackbuck
rm(datN)
datN<-dat[which(dat$Species=="Blackbuck" & dat$Method.Name=="Subtraction"),]
datN$VideoName=factor(datN$VideoName) #Remove undesired factors
m <- tapply(X=datN$failed.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=names(m)[o])

##Measure of variability across and within videos
lmean=tapply(datN$failed.percent,datN$VideoName,mean)
lvar=tapply(datN$failed.percent,datN$VideoName,var)
lmean=as.numeric(paste(unlist(lmean)))
lvar=as.numeric(paste(unlist(lvar)))
VMfailed = round(mean(na.omit(lvar)),2)
avgVar["BBfail","Sub"] = mean(na.omit(lvar))
varMean["BBfail","Sub"] = var(na.omit(lmean))

lmean=tapply(datN$false.percent,datN$VideoName,mean)
lvar=tapply(datN$false.percent,datN$VideoName,var)
lmean=as.numeric(paste(unlist(lmean)))
lvar=as.numeric(paste(unlist(lvar)))
VMfalse = round(mean(na.omit(lvar)),2)
avgVar["BBfalse","Sub"] = mean(na.omit(lvar))
varMean["BBfalse","Sub"] = var(na.omit(lmean))


par(mfrow=c(3,2))

Fstat = round(varMean/avgVar,digits=2)

bb_seg1=boxplot(datN$failed.percent*100~datN$VideoName,ylab="% failed detections",xaxt="n",notch=TRUE,col="cyan",las=2,main=paste("Fstat = ",Fstat["BBfail","Seg"]))
m <- tapply(X=datN$false.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=names(m)[o])
bb_seg2=boxplot(datN$false.percent*100~datN$VideoName,ylab="% false detections",xaxt="n",notch=TRUE,col="cyan",las=2,main=paste("Fstat = ",Fstat["BBfalse","Seg"]))
par(xpd=TRUE)



##Fish
datN=NA
datN=dat[which(dat$Species=="Fish" & dat$Method.Name=="Subtraction" & dat$Boundary != 1),]
datN$VideoName=factor(datN$VideoName) #Remove undesired factors
m <- tapply(X=datN$failed.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=names(m)[o])

##Measure of variability across and within videos

lmean=tapply(datN$failed.percent,datN$VideoName,mean)
lvar=tapply(datN$failed.percent,datN$VideoName,var)
lmean=as.numeric(paste(unlist(lmean)))
lvar=as.numeric(paste(unlist(lvar)))
VMfailed = round(mean(na.omit(lvar)),2)
avgVar["Ffail","Seg"] = mean(na.omit(lvar))
varMean["Ffail","Seg"] = var(na.omit(lmean))

lmean=tapply(datN$false.percent,datN$VideoName,mean)
lvar=tapply(datN$false.percent,datN$VideoName,var)
lmean=as.numeric(paste(unlist(lmean)))
lvar=as.numeric(paste(unlist(lvar)))
VMfalse = round(mean(na.omit(lvar)),2)
avgVar["Ffalse","Seg"] = mean(na.omit(lvar))
varMean["Ffalse","Seg"] = var(na.omit(lmean))


Fstat = round(varMean/avgVar,digits=2)

fish_seg1=boxplot(datN$failed.percent*100~datN$VideoName,ylab="% failed detections",xaxt="n",notch=TRUE,col="pink",las=2,main=paste("Fstat = ",Fstat["Ffail","Seg"]))
m <- tapply(X=datN$false.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=names(m)[o])
fish_seg2=boxplot(datN$false.percent*100~datN$VideoName,ylab="% false detections",xaxt="n",notch=TRUE,col="pink",las=2,main=paste("Fstat = ",Fstat["Ffalse","Seg"]))
par(xpd=TRUE)
mtext("Fish", outer =F, cex = 0.5)

datN=NA
datN=dat[which(dat$Species=="Wasp" & dat$Method.Name=="Subtraction"),]
datN$VideoName=factor(datN$VideoName) #Remove undesired factors
m <- tapply(X=datN$failed.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=names(m)[o])

##Measure of variability across and within videos

lmean=tapply(datN$failed.percent,datN$VideoName,mean)
lvar=tapply(datN$failed.percent,datN$VideoName,var)
lmean=as.numeric(paste(unlist(lmean)))
lvar=as.numeric(paste(unlist(lvar)))
VMfailed = round(mean(na.omit(lvar)),2)
avgVar["Wfail","Seg"] = mean(na.omit(lvar))
varMean["Wfail","Seg"] = var(na.omit(lmean))

lmean=tapply(datN$false.percent,datN$VideoName,mean)
lvar=tapply(datN$false.percent,datN$VideoName,var)
lmean=as.numeric(paste(unlist(lmean)))
lvar=as.numeric(paste(unlist(lvar)))
VMfalse = round(mean(na.omit(lvar)),2)
avgVar["Wfalse","Seg"] = mean(na.omit(lvar))
varMean["Wfalse","Seg"] = var(na.omit(lmean))

Fstat = varMean/avgVar

wasp_seg1=boxplot(datN$failed.percent*100~datN$VideoName,ylab="% failed detections",xaxt="n",notch=TRUE,col="orange",las=2,main=paste("Fstat = ",Fstat["Wfail","Seg"]))
m <- tapply(X=datN$false.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=names(m)[o])
wasp_seg2=boxplot(datN$false.percent*100~datN$VideoName,ylab="% false detections",xaxt="n",notch=TRUE,col="orange",las=2,main=paste("Fstat = ",Fstat["Wfalse","Seg"]))
par(xpd=TRUE)

##############################NN#####################################
## For blackbuck
rm(datN)
datN<-dat[which(dat$Species=="Blackbuck" & dat$Method.Name=="NN"),]
datN$VideoName=factor(datN$VideoName) #Remove undesired factors
m <- tapply(X=datN$failed.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=names(m)[o])

##Measure of variability across and within videos
lmean=tapply(datN$failed.percent,datN$VideoName,mean)
lvar=tapply(datN$failed.percent,datN$VideoName,var)
lmean=as.numeric(paste(unlist(lmean)))
lvar=as.numeric(paste(unlist(lvar)))
VMfailed = round(mean(na.omit(lvar)),2)
avgVar["BBfail","Seg"] = mean(na.omit(lvar))
varMean["BBfail","Seg"] = var(na.omit(lmean))

lmean=tapply(datN$false.percent,datN$VideoName,mean)
lvar=tapply(datN$false.percent,datN$VideoName,var)
lmean=as.numeric(paste(unlist(lmean)))
lvar=as.numeric(paste(unlist(lvar)))
VMfalse = round(mean(na.omit(lvar)),2)
avgVar["BBfalse","Seg"] = mean(na.omit(lvar))
varMean["BBfalse","Seg"] = var(na.omit(lmean))


par(mfrow=c(3,2))

Fstat = varMean/avgVar

bb_seg1=boxplot(datN$failed.percent*100~datN$VideoName,ylab="% failed detections",xaxt="n",notch=TRUE,col="cyan",las=2,main=paste("Fstat = ",Fstat["BBfail","Seg"]))
m <- tapply(X=datN$false.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=names(m)[o])
bb_seg2=boxplot(datN$false.percent*100~datN$VideoName,ylab="% false detections",xaxt="n",notch=TRUE,col="cyan",las=2,main=paste("Fstat = ",Fstat["BBfalse","Seg"]))
par(xpd=TRUE)



##Fish
datN=NA
datN=dat[which(dat$Species=="Fish" & dat$Method.Name=="NN" & dat$Boundary != 1),]
datN$VideoName=factor(datN$VideoName) #Remove undesired factors
m <- tapply(X=datN$failed.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=names(m)[o])

##Measure of variability across and within videos

lmean=tapply(datN$failed.percent,datN$VideoName,mean)
lvar=tapply(datN$failed.percent,datN$VideoName,var)
lmean=as.numeric(paste(unlist(lmean)))
lvar=as.numeric(paste(unlist(lvar)))
VMfailed = round(mean(na.omit(lvar)),2)
avgVar["Ffail","Seg"] = mean(na.omit(lvar))
varMean["Ffail","Seg"] = var(na.omit(lmean))

lmean=tapply(datN$false.percent,datN$VideoName,mean)
lvar=tapply(datN$false.percent,datN$VideoName,var)
lmean=as.numeric(paste(unlist(lmean)))
lvar=as.numeric(paste(unlist(lvar)))
VMfalse = round(mean(na.omit(lvar)),2)
avgVar["Ffalse","Seg"] = mean(na.omit(lvar))
varMean["Ffalse","Seg"] = var(na.omit(lmean))


Fstat = varMean/avgVar

fish_seg1=boxplot(datN$failed.percent*100~datN$VideoName,ylab="% failed detections",xaxt="n",notch=TRUE,col="pink",las=2,main=paste("Fstat = ",Fstat["Ffail","Seg"]))
m <- tapply(X=datN$false.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=names(m)[o])
fish_seg2=boxplot(datN$false.percent*100~datN$VideoName,ylab="% false detections",xaxt="n",notch=TRUE,col="pink",las=2,main=paste("Fstat = ",Fstat["Ffalse","Seg"]))
par(xpd=TRUE)
mtext("Fish", outer =F, cex = 0.5)

datN=NA
datN=dat[which(dat$Species=="Wasp" & dat$Method.Name=="Subtraction"),]
datN$VideoName=factor(datN$VideoName) #Remove undesired factors
m <- tapply(X=datN$failed.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=names(m)[o])

##Measure of variability across and within videos

lmean=tapply(datN$failed.percent,datN$VideoName,mean)
lvar=tapply(datN$failed.percent,datN$VideoName,var)
lmean=as.numeric(paste(unlist(lmean)))
lvar=as.numeric(paste(unlist(lvar)))
VMfailed = round(mean(na.omit(lvar)),2)
avgVar["Wfail","Seg"] = mean(na.omit(lvar))
varMean["Wfail","Seg"] = var(na.omit(lmean))

lmean=tapply(datN$false.percent,datN$VideoName,mean)
lvar=tapply(datN$false.percent,datN$VideoName,var)
lmean=as.numeric(paste(unlist(lmean)))
lvar=as.numeric(paste(unlist(lvar)))
VMfalse = round(mean(na.omit(lvar)),2)
avgVar["Wfalse","Seg"] = mean(na.omit(lvar))
varMean["Wfalse","Seg"] = var(na.omit(lmean))

Fstat = varMean/avgVar

wasp_seg1=boxplot(datN$failed.percent*100~datN$VideoName,ylab="% failed detections",xaxt="n",notch=TRUE,col="orange",las=2,main=paste("Fstat = ",Fstat["Wfail","Seg"]))
m <- tapply(X=datN$false.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=names(m)[o])
wasp_seg2=boxplot(datN$false.percent*100~datN$VideoName,ylab="% false detections",xaxt="n",notch=TRUE,col="orange",las=2,main=paste("Fstat = ",Fstat["Wfalse","Seg"]))
par(xpd=TRUE)


#############################################Relation with group and vidoe properties#################################

##Blackbuck-Segmentation
datN=NA
datN=dat[which(dat$Species=="Blackbuck" & dat$Method.Name=="NN"),]
plot(datN$failed.percent~datN$Contrast,pch=23,bg="cyan3")
par(xpd=TRUE)
#legend(0.8,-0.1,levels(datN$Method.Name), pch = 23,col="black",pt.bg=c("yellow2","cyan3","salmon1"))
corCon_BB=cor.test(datN$Contrast,datN$failed.percent)  ##Correlations
abline(lm(datN$failed.percent~datN$Contrast), col="red")
mtext(paste(" p-value = ",round(corCon_BB$p.value,3)))

plot(datN$false.percent~datN$Contrast,pch=23,bg="cyan3")
par(xpd=TRUE)
corCon_BB=cor.test(datN$Contrast,datN$false.percent)  ##Correlations
abline(lm(datN$false.percent~datN$Contrast), col="red")
mtext(paste(" p-value = ",round(corCon_BB$p.value,3)))

#Density
plot(datN$failed.percent~datN$Narea,pch=23,bg="cyan3")
par(xpd=TRUE)
corDen_BB=cor.test(datN$Narea,datN$failed.percent)  ##Correlations
abline(lm(datN$failed.percent~datN$Narea), col="red")
mtext(paste(" p-value = ",round(corDen_BB$p.value,3)))


plot(datN$false.percent~datN$Narea,pch=23,bg="cyan3")
par(xpd=TRUE)
corDen_BB=cor.test(datN$Narea,datN$false.percent)  ##Correlations
abline(lm(datN$false.percent~datN$Narea), col="red")
mtext(paste(" p-value = ",round(corDen_BB$p.value,3)))

##Blackbuck-Subtraction
datN=NA
datN=dat[which(dat$Species=="Blackbuck" & dat$Method.Name=="Subtraction"),]
plot(datN$failed.percent~datN$Contrast,pch=23,bg="cyan3")
par(xpd=TRUE)
#legend(0.8,-0.1,levels(datN$Method.Name), pch = 23,col="black",pt.bg=c("yellow2","cyan3","salmon1"))
corCon_BB=cor.test(datN$Contrast,datN$failed.percent)  ##Correlations
abline(lm(datN$failed.percent~datN$Contrast), col="red")
mtext(paste(" p-value = ",round(corCon_BB$p.value,3)))

plot(datN$false.percent~datN$Contrast,pch=23,bg="cyan3")
par(xpd=TRUE)
#legend(0.8,-0.1,levels(datN$Method.Name), pch = 23,col="black",pt.bg=c("yellow2","cyan3","salmon1"))
corCon_BB=cor.test(datN$Contrast,datN$false.percent)  ##Correlations
abline(lm(datN$failed.percent~datN$Contrast), col="red")
mtext(paste(" p-value = ",round(corCon_BB$p.value,3)))



#Density
plot(datN$failed.percent~datN$Narea,pch=23,bg="cyan3")
par(xpd=TRUE)
corDen_BB=cor.test(datN$Narea,datN$failed.percent)  ##Correlations
abline(lm(datN$failed.percent~datN$Narea), col="red")
mtext(paste(" p-value = ",round(corDen_BB$p.value,3)))


plot(datN$false.percent~datN$Narea,pch=23,bg="cyan3")
par(xpd=TRUE)
corDen_BB=cor.test(datN$Narea,datN$false.percent)  ##Correlations
abline(lm(datN$false.percent~datN$Narea), col="red")
mtext(paste(" p-value = ",round(corDen_BB$p.value,3)))

##Fish-Segmentation
datN=NA
datN=dat[which(dat$Species=="Fish" & dat$Method.Name=="Segmentation"),]
plot(datN$failed.percent~datN$Contrast,pch=23,bg="cyan3")
par(xpd=TRUE)
#legend(0.8,-0.1,levels(datN$Method.Name), pch = 23,col="black",pt.bg=c("yellow2","cyan3","salmon1"))
corCon_BB=cor.test(datN$Contrast,datN$failed.percent)  ##Correlations
abline(lm(datN$failed.percent~datN$Contrast), col="red")
mtext(paste(" p-value = ",round(corCon_BB$p.value,3)))

plot(datN$false.percent~datN$Contrast,pch=23,bg="cyan3")
par(xpd=TRUE)
corCon_BB=cor.test(datN$Contrast,datN$false.percent)  ##Correlations
abline(lm(datN$false.percent~datN$Contrast), col="red")
mtext(paste(" p-value = ",round(corCon_BB$p.value,3)))

#Density
plot(datN$failed.percent~datN$Area,pch=23,bg="cyan3")
par(xpd=TRUE)
corDen_BB=cor.test(datN$Area,datN$failed.percent)  ##Correlations
abline(lm(datN$failed.percent~datN$Area), col="red")
mtext(paste(" p-value = ",round(corDen_BB$p.value,3)))


plot(datN$false.percent~datN$Area,pch=23,bg="cyan3")
par(xpd=TRUE)
corDen_BB=cor.test(datN$Area,datN$false.percent)  ##Correlations
abline(lm(datN$false.percent~datN$Area), col="red")
mtext(paste(" p-value = ",round(corDen_BB$p.value,3)))

##Fish-Subtraction
datN=NA
datN=dat[which(dat$Species=="Fish" & dat$Method.Name=="Subtraction"),]
plot(datN$failed.percent~datN$Contrast,pch=23,bg="cyan3")
par(xpd=TRUE)
#legend(0.8,-0.1,levels(datN$Method.Name), pch = 23,col="black",pt.bg=c("yellow2","cyan3","salmon1"))
corCon_BB=cor.test(datN$Contrast,datN$failed.percent)  ##Correlations
abline(lm(datN$failed.percent~datN$Contrast), col="red")
mtext(paste(" p-value = ",round(corCon_BB$p.value,3)))

plot(datN$false.percent~datN$Contrast,pch=23,bg="cyan3")
par(xpd=TRUE)
#legend(0.8,-0.1,levels(datN$Method.Name), pch = 23,col="black",pt.bg=c("yellow2","cyan3","salmon1"))
corCon_BB=cor.test(datN$Contrast,datN$false.percent)  ##Correlations
abline(lm(datN$failed.percent~datN$Contrast), col="red")
mtext(paste(" p-value = ",round(corCon_BB$p.value,3)))



#Density
plot(datN$failed.percent~datN$Area,pch=23,bg="cyan3")
par(xpd=TRUE)
corDen_BB=cor.test(datN$Area,datN$failed.percent)  ##Correlations
abline(lm(datN$failed.percent~datN$Area), col="red")
mtext(paste(" p-value = ",round(corDen_BB$p.value,3)))


plot(datN$false.percent~datN$Area,pch=23,bg="cyan3")
par(xpd=TRUE)
corDen_BB=cor.test(datN$Area,datN$false.percent)  ##Correlations
abline(lm(datN$false.percent~datN$Area), col="red")
mtext(paste(" p-value = ",round(corDen_BB$p.value,3)))




