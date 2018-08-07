#Program to compare various Computer Vision techniques on different data-sets
#Written by AA on 30July2018

setwd("/media/aakanksha/f41d5ac2-703c-4b56-a960-cd3a54f21cfb/aakanksha/Documents/Backup/Phd/Analysis/Chapter1")
#Read data file
dat = read.csv("Ipcomparison.csv") 
dat=dat[-which(is.na(dat$failed.percent) | is.na(dat$false.percent)),]
head(dat)
summary(dat)

colnames(dat)
library("ggplot2")
library("hexbin")

####################################################################################################
#1. Comparision of error across techniques
####################################################################################################

lev=paste(dat$Method.Name," ",dat$Species)
n1=c("turquoise","orange") #1:length(unique(dat$Method.Name))
n2=length(unique(dat$Species))
clr=rep(n1,each=n2)

#Graphs for technique-species
boxplot((failed.percent*100) ~ lev, data = dat, xlab = "",ylab = "% failed detections", main = "",col=clr,notch=TRUE, boxwex=0.3)
means <- tapply(dat$failed.percent,lev,mean)
points(means*100,col="red",pch=9,cex=1)

boxplot((false.percent*100) ~ lev, data = dat, xlab = "",ylab = "% false detections", main = "",col=clr,notch=TRUE, boxwex=0.3)
means <- tapply(dat$false.percent,lev,mean)
points(means*100,col="red",pch=9,cex=1)


# Error density map for eachtechnique separately
##Segmentation
datN=NA
datN=dat[which(dat$Method.Name=="Segmentation"),]
ggplot (datN, aes (x = failed.percent, y = false.percent,colour=datN$Species,main="Segmentation")) + stat_density2d ()

plot(datN$failed.percent, datN$false.percent, pch=23,bg=c("yellow2","cyan3","salmon1")[unclass(datN$Species)],main="Detection errors when using color thresholding")
par(xpd=TRUE)
legend(0.85,-0.1,levels(datN$Species), pch = 23,col="black",pt.bg=c("yellow2","cyan3","salmon1"))

##Subtraction
datN=NA
datN=dat[which(dat$Method.Name=="Subtraction"),]
ggplot (datN[which(dat$=="Subtraction"),], aes (x = failed.percent, y = false.percent,colour=datN$Species,main="Subtraction")) + geom_point(data = datN,mapping = aes(x = failed.percent, y = false.percent,colour=datN$Species,shape = datN$Species))+stat_density2d ()

plot(datN$failed.percent, datN$false.percent, pch=23,bg=c("yellow2","cyan3","salmon1")[unclass(datN$Species)],main="Detection errors when using image subtraction")
par(xpd=TRUE)
legend(0.85,-0.1,levels(datN$Species), pch = 23,col="black",pt.bg=c("yellow2","cyan3","salmon1"))
#####################################################################################################3
#2. Variation inerrors~ video id
######################################################################################################3
#Graphs - video id is predictor variable

## For blackbuck
datN=NA
datN=dat[which(dat$Species=="Blackbuck"),]
lev=paste(datN$VideoName,datN$Method.Name)

boxplot((datN$failed.percent*100)~lev,data=datN,ylab="% failed detections",notch=TRUE,col=ifelse(levels(datN$Method.Name)=="Segmentation","turquoise","orange"),main="Blackbuck",xaxt="n")
axis(1,at=1:length(lev),labels=substr(lev,1,regexpr(' ',lev)),las=2)
par(xpd=TRUE)
legend(0.85,-0.1,levels(datN$Method.Name),pch=22,pt.bg=ifelse(levels(datN$Method.Name)=="Segmentation","turquoise","orange"))

boxplot((datN$false.percent*100)~lev,data=datN,ylab="% false detections",notch=TRUE,col=ifelse(levels(datN$Method.Name)=="Segmentation","turquoise","orange"),main="Blackbuck",xaxt="n")
axis(1,at=1:length(lev),labels=substr(lev,1,regexpr(' ',lev)),las=2)
legend("topright",levels(datN$Method.Name),pch=22,pt.bg=ifelse(levels(datN$Method.Name)=="Segmentation","turquoise","orange"))

## For Fish 
datF=dat[which(dat$Species=="Fish"),]
lev=paste(datF$VideoName,datF$Method.Name)

boxplot((datF$failed.percent*100)~lev,data=datF,ylab="% failed detections",notch=TRUE,col=ifelse(levels(datF$Method.Name)=="Segmentation","turquoise","orange"),main="Fish",xaxt="n")
axis(1,at=1:length(lev),labels=substr(lev,1,regexpr(' ',lev)),las=2)
par(xpd=TRUE)
legend("topright",levels(datF$Method.Name),pch=22,pt.bg=ifelse(levels(datF$Method.Name)=="Segmentation","turquoise","orange"))

boxplot((datF$false.percent*100)~lev,data=datF,ylab="% false detections",notch=TRUE,col=ifelse(levels(datF$Method.Name)=="Segmentation","turquoise","orange"),main="Fish",xaxt="n")
axis(1,at=1:length(lev),labels=substr(lev,1,regexpr(' ',lev)),las=2)
par(xpd=TRUE)
legend("topright",levels(datF$Method.Name),pch=22,pt.bg=ifelse(levels(datF$Method.Name)=="Segmentation","turquoise","orange"))


################################################################################################################3
##2C. Variation in error across all videos all species
#######################################################################333333333333#########33333333######33

#Same as 1?
##############################################################################################################


#############################################Relation with group and vidoe properties#################################

##Blackbuck-Segmentation
datN=NA
datN=dat[which(dat$Species=="Blackbuck" & dat$Method.Name=="Segmentation"),]
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




