setwd("/media/aakanksha/f41d5ac2-703c-4b56-a960-cd3a54f21cfb/aakanksha/Documents/Backup/Phd/Analysis/Chapter1")
#Read data file
dat = read.csv("Ipcomparison_exbound.csv") 
dat=dat[which(!is.na(dat$failed.percent) & !is.na(dat$false.percent)),]

head(dat)
summary(dat)

#Select data for subtraction technique for both type of errors

datS = dat[dat$Method.Name=="Subtraction",]
View(datS)


##sort the data frame on the basis of false and failed detections

datError = datS[order(datS$false.percent,datS$failed.percent),]
dSub=datError[seq(1,nrow(datError),by=9),]

# hist(dError$failed.percent)
# hist(dError$false.percent)
# barplot(table(dError$Species))

#Select data for segmentation technique for both type of errors
rm(datS)
datS = dat[dat$Method.Name=="Segmentation",]
View(datS)


##sort the data frame on the basis of false and failed detections

datError = datS[order(datS$false.percent,datS$failed.percent),]
dSeg=datError[seq(1,nrow(datError),by=9),]

# hist(dError$failed.percent)
# hist(dError$false.percent)
# barplot(table(dError$Species))

#Select data for NN technique for both type of errors
rm(datS)
datS = dat[dat$Method.Name=="NN",]
View(datS)


##sort the data frame on the basis of false and failed detections

datError = datS[order(datS$false.percent,datS$failed.percent),]
dNN=datError[seq(1,nrow(datError),by=9),]

# hist(dError$failed.percent)
# hist(dError$false.percent)
# barplot(table(dError$Species))

##Create a merged dataset

total = rbind(dSub,dSeg,dNN)
View(total)
newD = data.frame(VideoName=total$VideoName,Frame_Number=total$Frame.number,Contrast=total$Contrast..Fg.BG.,ANN=total$ANN,Movement=total$Movement)
Total=unique(newD)

##Write output csv files
write.csv(Total,file="VideoProperties.csv")
write.csv(dNN,file="VideoPropertiesNN.csv")
write.csv(dSub,file="VideoPropertiesSub.csv")
write.csv(dSeg,file="VideoPropertiesSeg.csv")
