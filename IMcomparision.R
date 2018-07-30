#Program to compare various Computer Vision techniques on different data-sets
#Written by AA on 30July2018

setwd("/media/aakanksha/f41d5ac2-703c-4b56-a960-cd3a54f21cfb/aakanksha/Documents/Backup/Phd/Analysis/Chapter1")
#Read data file
dat = read.csv("Ipcomparison.csv") 
dat=dat[-which(is.na(dat$failed.percent) | is.na(dat$false.percent)),]
head(dat)
summary(dat)

colnames(dat)

##Get mean error method wise and species wise
#out_failed=tapply(dat$failed.percent,list(dat$Method.Name,dat$Species),mean)
#out_false=tapply(dat$false.percent,list(dat$Method.Name,dat$Species),mean)

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


#Graphs for video names