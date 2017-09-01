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

set.seed(123)
indexes <- createDataPartition(WapSample$WAP517, p = .80, list = FALSE)
trainData <- WapSample[indexes,1:(ncol(WapSample))]
testData <- WapSample[-indexes,1:(ncol(WapSample))]

trainData$BUILDINGID <- Dataset[indexes,]$BUILDINGID

Bmodel <-  rpart( BUILDINGID~ .,
                  data = trainData,
                  control = rpart.control(maxdepth = maxd,cp=cptree))


prediction <- as.data.frame(round(predict(Bmodel,newdata= testData)))
prediction <- prediction %>% mutate(prediccion = round(prediction[,1]))
prediction <- prediction %>% mutate(resultado = Dataset[-indexes,]$BUILDINGID)
prediction <- prediction %>% mutate(check = ifelse(resultado == prediccion, 1, 0)) #Check if prediction is correct
success <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
success




prediction <- as.data.frame(round(predict(Bmodel,newdata= trainData)))
prediction <- prediction %>% mutate(buildingid = round(prediction[,1]))
prediction <- prediction %>% mutate(resultado = Dataset[indexes,]$BUILDINGID)
prediction <- prediction %>% mutate(check = ifelse(resultado == buildingid, 1, 0)) #Check if prediction is correct
successTrain <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
successTrain


prediction <- as.data.frame(round(predict(Bmodel,newdata= WapVal)))
prediction <- prediction %>% mutate(buildingid = round(prediction[,1]))
prediction <- prediction %>% mutate(resultado = vDataset$BUILDINGID)
prediction <- prediction %>% mutate(check = ifelse(resultado == buildingid, 1, 0)) #Check if prediction is correct
successVal <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
successVal


######WHERE IS MY WAP

Buildingmean <- c()
WapSample <- WapSample[,1:520]

sums <- apply(WapSample,2,mean)

for(i in 1:520){
  
  if(length(which(WapSample[,i] >0)) > 0){
    mymean<- round(mean(Dataset$BUILDINGID[which(WapSample[,i] > 0)]))
  }else{
    mymean <- 0
  }
  Buildingmean <- c(Buildingmean,mymean)
}

myBuilding <- c()

for(i in 1:nrow(WapSample)){
  
  myb <- round(mean(Buildingmean[WapSample[i,] > 0.35]))
  myBuilding <- c(myBuilding,myb)  
  
}

prediction <- as.data.frame(myBuilding)
prediction$prediccion <- prediction[,1]
prediction$resultado <- Dataset$BUILDINGID
prediction$check <- ifelse(prediction$resultado == prediction$prediccion,1,0)
successTrain <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
successTrain

#WapSample <- read.csv("data/OnlyWapNormCut.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)
WapSample$BUILDINGID <- prediction$prediccion



WapVal <- WapVal[,1:520]
myBuilding <- c()
for(i in 1:nrow(WapVal)){
  
  myb <- round(mean(Buildingmean[WapVal[i,] > 0.35]))
  myBuilding <- c(myBuilding,myb)  
  
}

prediction <- as.data.frame(myBuilding)
prediction$prediccion <- prediction[,1]
prediction$resultado <- vDataset$BUILDINGID
prediction$check <- ifelse(prediction$resultado == prediction$prediccion,1,0)
successVal <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
successVal

mymean<-apply(WapVal,1,mean)

#WapVal <- read.csv("data/ValidationWapNorm.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)
WapVal$BUILDINGID <- prediction$prediccion

write.csv(WapSample$BUILDINGID,file="~/Documents/Ubiqum/Uji/DatasetBuildingID.csv",row.names=FALSE)
write.csv(WapVal$BUILDINGID,file="~/Documents/Ubiqum/Uji/ValidationBuildingID.csv",row.names=FALSE)


# set.seed(123)
# indexes <- createDataPartition(WapSample$WAP517, p = .80, list = FALSE)
# trainData <- WapSample[indexes,1:(ncol(WapSample))]
# testData <- WapSample[-indexes,1:(ncol(WapSample))]
# trainData$BUILDINGID <- factor(trainData$BUILDINGID)
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# seed <- 7
# metric <- "Accuracy"
# set.seed(seed)
# mtry <- sqrt(ncol(trainData))
# tunegrid <- expand.grid(.mtry=mtry)
# rf_default <- train(BUILDINGID~., data=trainData, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)

write.csv(WapSample,file="~/Documents/Ubiqum/Uji/OnlyWapBuilding.csv",row.names=FALSE)
write.csv(WapVal,file="~/Documents/Ubiqum/Uji/validationDataBuilding.csv",row.names=FALSE)