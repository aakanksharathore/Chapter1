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
library("gridExtra")
####################################################################################################
#1. Comparision of error across techniques
####################################################################################################

# lev=paste(dat$Method.Name," ",dat$Species)
# n1=c("turquoise","orange") #1:length(unique(dat$Method.Name))
# n2=length(unique(dat$Species))
# clr=rep(n1,each=n2)

## For Fish
datN=NA
datN=dat[which(dat$Species=="Fish"),]

#Graphs for technique comparision
lmts <- range(datN$failed.percent*100,datN$false.percent*100)
par(mfrow = c(1, 2))
boxplot(data = datN,(failed.percent*100) ~ Method.Name,  xlab = "",ylab = "% failed detections", main = "",col="turquoise",notch=TRUE, boxwex=0.1)
means <- tapply(datN$failed.percent,datN$Method.Name,mean)
points(means*100,bg="orangered",pch=23,cex=1)
abline(h=20,col="orangered")
boxplot(data = datN,(false.percent*100) ~ Method.Name,  xlab = "",ylab = "% false detections", main = "",col="turquoise",notch=TRUE, boxwex=0.1)
means <- tapply(datN$false.percent,datN$Method.Name,mean)
points(means*100,bg="orangered",pch=23,cex=1)
abline(h=20,col="orangered")
mtext("Fish", side = 3, line = -2, outer = TRUE,cex=1.5,font=2)

# Error density map for eachtechnique separately
ggplot(datN, aes (x = failed.percent, y = false.percent,main="Fish", col=Method.Name,pch=Method.Name)) + geom_point()

par(mfrow=c(1,2))
xdensity <- ggplot(datN, aes(x = failed.percent, fill=datN$Method.Name)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('turquoise','orange')) 
ydensity <- ggplot(datN, aes(x = false.percent, fill=datN$Method.Name)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('turquoise','orange')) 
grid.arrange(xdensity, ydensity, nrow = 1)

## For blackbuck
datN=NA
datN=dat[which(dat$Species=="Blackbuck"),]

#Graphs for technique comparision
lmts <- range(datN$failed.percent*100,datN$false.percent*100)
par(mfrow = c(1, 2))
boxplot(data = datN,(failed.percent*100) ~ Method.Name,  xlab = "",ylab = "% failed detections", main = "",col="turquoise",notch=TRUE, boxwex=0.1)
means <- tapply(datN$failed.percent,datN$Method.Name,mean)
points(means*100,bg="orangered",pch=23,cex=1)
abline(h=20,col="orangered")
boxplot(data = datN,(false.percent*100) ~ Method.Name,  xlab = "",ylab = "% false detections", main = "",col="turquoise",notch=TRUE, boxwex=0.1)
means <- tapply(datN$false.percent,datN$Method.Name,mean)
points(means*100,bg="orangered",pch=23,cex=1)
abline(h=20,col="orangered")
mtext("Blackbuck", side = 3, line = -2, outer = TRUE,cex=1.5,font=2)

# Error density map for eachtechnique separately
ggplot (datN, aes (x = failed.percent, y = false.percent,colour=datN$Method.Name)) + stat_density2d ()

xdensity <- ggplot(datN, aes(x = failed.percent, fill=datN$Method.Name)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('turquoise','orange')) 
ydensity <- ggplot(datN, aes(x = false.percent, fill=datN$Method.Name)) + 
  geom_density(alpha=.5) + 
  scale_fill_manual(values = c('turquoise','orange')) 
grid.arrange(xdensity, ydensity, nrow = 1)
#####################################################################################################3
#2. Variation inerrors~ video id
######################################################################################################3
#Graphs - video id is predictor variable

## For blackbuck
datN=NA
datN=dat[which(dat$Species=="Blackbuck" & dat$Method.Name=="Segmentation"),]
m <- tapply(X=datN$failed.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=levels(datN$VideoName)[o])
##Measure of variability across and within videos
l=tapply(datN$failed.percent,datN$VideoName,var)
l=as.numeric(paste(unlist(l)))
VMfailed = round(mean(na.omit(l)),2)
l=tapply(datN$false.percent,datN$VideoName,var)
l=as.numeric(paste(unlist(l)))
VMfalse = round(mean(na.omit(l)),2)
par(mfrow=c(2,2))
boxplot(datN$failed.percent*100~datN$VideoName,ylab="% failed detections",notch=TRUE,col="turquoise",las=2,main=paste("coefV = ",VMfailed))
m <- tapply(X=datN$false.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=levels(datN$VideoName)[o])
boxplot(datN$false.percent*100~datN$VideoName,ylab="% false detections",notch=TRUE,col="turquoise",las=2,main=paste("coefV = ",VMfalse))

datN=NA
datN=dat[which(dat$Species=="Blackbuck" & dat$Method.Name=="Subtraction"),]
m <- tapply(X=datN$failed.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=levels(datN$VideoName)[o])
##Measure of variability across and within videos
l=tapply(datN$failed.percent,datN$VideoName,var)
l=as.numeric(paste(unlist(l)))
VMfailed = round(mean(na.omit(l)),2)
l=tapply(datN$false.percent,datN$VideoName,var)
l=as.numeric(paste(unlist(l)))
VMfalse = round(mean(na.omit(l)),2)
boxplot(datN$failed.percent*100~datN$VideoName,ylab="% failed detections",notch=TRUE,col="orange",las=2,main=paste("coefV = ",VMfailed))
m <- tapply(X=datN$false.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=levels(datN$VideoName)[o])
boxplot(datN$false.percent*100~datN$VideoName,ylab="% false detections",notch=TRUE,col="orange",las=2,main=paste("coefV = ",VMfalse))
par(xpd=TRUE)
#legend("topright",levels(datN$Method.Name),col=ifelse(levels(datN$Method.Name)=="Segmentation","turquoise","orange"))
mtext("Blackbuck", side = 3, line = -2, outer = TRUE,cex=1.5,font=2)

## For Fish 
datN=NA
datN=dat[which(dat$Species=="Fish" & dat$Method.Name=="Segmentation"),]
m <- tapply(X=datN$failed.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=levels(datN$VideoName)[o])
##Measure of variability across and within videos
l=tapply(datN$failed.percent,datN$VideoName,var)
l=as.numeric(paste(unlist(l)))
VMfailed = round(mean(na.omit(l)),2)
l=tapply(datN$false.percent,datN$VideoName,var)
l=as.numeric(paste(unlist(l)))
VMfalse = round(mean(na.omit(l)),2)
par(mfrow=c(2,2))
boxplot(datN$failed.percent*100~datN$VideoName,ylab="% failed detections",notch=TRUE,col="turquoise",las=2,main=paste("coefV = ",VMfailed))
m <- tapply(X=datN$false.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=levels(datN$VideoName)[o])
boxplot(datN$false.percent*100~datN$VideoName,ylab="% false detections",notch=TRUE,col="turquoise",las=2,main=paste("coefV = ",VMfalse))
datN=NA
datN=dat[which(dat$Species=="Fish" & dat$Method.Name=="Subtraction"),]
m <- tapply(X=datN$failed.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=levels(datN$VideoName)[o])
##Measure of variability across and within videos
l=tapply(datN$failed.percent,datN$VideoName,var)
l=as.numeric(paste(unlist(l)))
VMfailed = round(mean(na.omit(l)),2)
l=tapply(datN$false.percent,datN$VideoName,var)
l=as.numeric(paste(unlist(l)))
VMfalse = round(mean(na.omit(l)),2)
boxplot(datN$failed.percent*100~datN$VideoName,ylab="% failed detections",notch=TRUE,col="orange",las=2,main=paste("coefV = ",VMfailed))
m <- tapply(X=datN$false.percent, INDEX=datN$VideoName, FUN = median)
o <- order(m, decreasing = TRUE)
datN$VideoName=factor(datN$VideoName,levels=levels(datN$VideoName)[o])
boxplot(datN$false.percent*100~datN$VideoName,ylab="% false detections",notch=TRUE,col="orange",las=2,main=paste("coefV = ",VMfalse))
par(xpd=TRUE)
#legend("topright",levels(datN$Method.Name),col=ifelse(levels(datN$Method.Name)=="Segmentation","turquoise","orange"))
mtext("Fish", side = 3, line = -2, outer = TRUE,cex=1.5,font=2)


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




