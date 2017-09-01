###############################
###                         ###
###   WIFI LOCATION         ###
###                         ###
###                         ###
###                         ###
###############################


#####Model1 is using all the waps plus NumberOfWaps plus Top1 using a random tree

library("tidyr")
library("dplyr")
library("taRifx")
library("lubridate")
library("rpart")
library("caret")

WD <- "/Users/sediaz/Documents/GitHub/UJILocation"
setwd(WD)

source("Functions.R")
Writing <- FALSE

####Variables#####
Create = FALSE
cptree <- 0.000001
maxd <- 30


####Reading the tables####
Dataset <- read.csv("~/Documents/Ubiqum/Uji/DatasetClean.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
WapSample <- read.csv("~/Documents/Ubiqum/Uji/OnlyWapNormCut.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)
vDataset <- read.csv("~/Documents/Ubiqum/Uji/validationData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
WapVal <- read.csv("~/Documents/Ubiqum/Uji/ValidationWapNorm.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)

#Dataset$LATITUDE <- Dataset$LATITUDE - min(Dataset$LATITUDE)
#Dataset$LONGITUDE <- Dataset$LONGITUDE - min(Dataset$LONGITUDE)

WapSample$NumberOfWaps <- NULL

######WHERE IS MY WAP

Buildingmean <- c()
WapSample <- WapSample[,1:520]

for(i in 1:520){
  
  if(length(which(WapSample[,i] >0)) > 0){
    mymean<-  as.numeric(names(which.max(table(Dataset$BUILDINGID[which(WapSample[,i] > 0)]))))
    #mymean<- round(mean(Dataset$BUILDINGID[which(WapSample[,i] > 0)]))
  }else{
    mymean <- -1
  }
  Buildingmean <- c(Buildingmean,mymean)
}

myBuilding <- c()

for(i in 1:nrow(WapSample)){
  
  mywap <- which(WapSample[i,] > 0.35)
  myb <- as.numeric(names(which.max(table(Buildingmean[mywap]))))
  myBuilding <- c(myBuilding,myb) 
}

prediction <- as.data.frame(myBuilding)
prediction$prediccion <- prediction[,1]
prediction$resultado <- Dataset$BUILDINGID
prediction$check <- ifelse(prediction$resultado == prediction$prediccion,1,0)
successTrain <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
successTrain

#WapSample <- read.csv("data/OnlyWapNormCut.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)
WapSample$BUILDINGID <- myBuilding



WapVal <- WapVal[,1:520]
myBuilding <- c()
for(i in 1:nrow(WapVal)){
  
  mywap <- which(WapVal[i,] > 0.35)
  myb <- as.numeric(names(which.max(table(Buildingmean[mywap]))))
  myBuilding <- c(myBuilding,myb)  
  
}

prediction <- as.data.frame(myBuilding)
prediction$prediccion <- prediction
prediction$resultado <- vDataset$BUILDINGID
prediction$check <- ifelse(prediction$resultado == prediction$prediccion,1,0)
successVal <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
successVal


#WapVal <- read.csv("data/ValidationWapNorm.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)
WapVal$BUILDINGID <- myBuilding

View(WapVal)

if(Writing){
  write.csv(WapSample$BUILDINGID,file="~/Documents/Ubiqum/Uji/DatasetBuildingID.csv",row.names=FALSE)
  write.csv(WapVal$BUILDINGID,file="~/Documents/Ubiqum/Uji/ValidationBuildingID.csv",row.names=FALSE)

  write.csv(WapSample,file="~/Documents/Ubiqum/Uji/OnlyWapBuilding.csv",row.names=FALSE)
  write.csv(WapVal,file="~/Documents/Ubiqum/Uji/validationDataBuilding.csv",row.names=FALSE)
}