###############################
###                         ###
###   WIFI LOCATION         ###
###                         ###
###                         ###
###                         ###
###############################

####Model 3 using data below -20 the Latitude and longitude to predict Building

library("tidyr")
library("dplyr")
library("taRifx")
library("lubridate")
library("rpart")

WD <- "/Users/sediaz/Documents/Ubiqum/Curso online Primavera 2017/R/Course3Task3"
setwd(WD)

source("Functions.R")

####Variables#####
Create = FALSE
cptree <- 0.000001
maxd <- 30


####Reading the tables####
Dataset <- read.csv("DatasetClean.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
WapSample <- read.csv("data/OnlyWapNormCut08.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)
vDataset <- read.csv("validationData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
WapVal <- read.csv("data/ValidationWapNorm.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)

Dataset$LATITUDE <- Dataset$LATITUDE - min(Dataset$LATITUDE)
Dataset$LONGITUDE <- Dataset$LONGITUDE - min(Dataset$LONGITUDE)

WapSample$TIMESTAMP <- NULL
WapVal$TIMESTAMP <- NULL
WapVal$Top1 <- NULL
WapSample$Top1 <- NULL


set.seed(123)
indexes <- createDataPartition(WapSample$WAP006, p = .80, list = FALSE)

trainData <- WapSample[indexes,]
testData <- WapSample[-indexes,]

trainData$LATITUDE <- Dataset[indexes,]$LATITUDE

LatModel <-  rpart(LATITUDE ~. ,
                   data = trainData,
                   control = rpart.control(maxdepth = maxd,cp=cptree))


prediction <- as.data.frame(predict(LatModel,newdata= testData))
prediction <- prediction %>% mutate(latitude = prediction[,1])
prediction <- prediction %>% mutate(resultado = Dataset[-indexes,]$LATITUDE)
prediction <- prediction %>% mutate(distance = abs(resultado - latitude)) #Calculate distance in coordinates
ErrorLat <- sum(prediction$distance)/nrow(prediction) #Average error
ErrorLat


#Assigning predicted Latitudes
testData$LATITUDE <- prediction$latitude

prediction <- as.data.frame(predict(LatModel,newdata= trainData))
prediction <- prediction %>% mutate(latitude = prediction[,1])
trainData$LATITUDE <- prediction$latitude

prediction <- as.data.frame(predict(LatModel,newdata= WapVal))
prediction <- prediction %>% mutate(latitude = prediction[,1])
WapVal$LATITUDE <- prediction$latitude


trainData$LONGITUDE <- Dataset[indexes,]$LONGITUDE


LonModel <-  rpart(LONGITUDE ~. ,
                   data = trainData,
                   control = rpart.control(maxdepth = maxd,cp=cptree))


prediction <- as.data.frame(predict(LonModel,newdata= testData))
prediction <- prediction %>% mutate(longitude = prediction[,1])
prediction <- prediction %>% mutate(resultado = Dataset[-indexes,]$LONGITUDE)
prediction <- prediction %>% mutate(distance = abs(resultado - longitude)) #Calculate distance in coordinates
ErrorLon <- sum(prediction$distance)/nrow(prediction) #Average error
ErrorLon

ErrorDistance <- sqrt(ErrorLon^2 + ErrorLat^2)
ErrorDistance


#Assigning predicted longitudes
testData$LONGITUDE <- prediction$longitude

prediction <- as.data.frame(predict(LonModel,newdata= trainData))
prediction <- prediction %>% mutate(longitude = prediction[,1])

trainData$LONGITUDE <- prediction$longitude

prediction <- as.data.frame(predict(LonModel,newdata= WapVal))
prediction <- prediction %>% mutate(longitude = prediction[,1])
WapVal$LONGITUDE <- prediction$longitude



####Predicting the Building####
trainData$BUILDINGID <- Dataset[indexes,]$BUILDINGID
testData$BUILDINGID <- Dataset[-indexes,]$BUILDINGID
WapVal$BUILDINGID <- vDataset$BUILDINGID

Bmodel <-  rpart( BUILDINGID~. ,
                  data = trainData,
                  control = rpart.control(maxdepth = maxd,cp=cptree))

prediction <- as.data.frame(round(predict(Bmodel,newdata= testData)))
prediction <- prediction %>% mutate(prediccion = round(prediction[,1]))
prediction <- prediction %>% mutate(resultado = testData$BUILDINGID)
prediction <- prediction %>% mutate(check = ifelse(resultado == prediccion, 1, 0)) #Check if prediction is correct
successTest <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
print(paste("Test ",successTest))



#Assigning building IDs
#testData$BUILDINGID <- prediction$prediccion

prediction <- as.data.frame(round(predict(Bmodel,newdata= trainData)))
prediction <- prediction %>% mutate(buildingid = round(prediction[,1]))
prediction <- prediction %>% mutate(resultado = Dataset[indexes,]$BUILDINGID)
prediction <- prediction %>% mutate(check = ifelse(resultado == buildingid, 1, 0)) #Check if prediction is correct
successTrain <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
successTrain

trainData$BUILDINGID <- prediction$buildingid


prediction <- as.data.frame(round(predict(Bmodel,newdata= WapVal)))
prediction <- prediction %>% mutate(buildingid = round(prediction[,1]))
prediction <- prediction %>% mutate(resultado = vDataset$BUILDINGID)
prediction <- prediction %>% mutate(check = ifelse(resultado == buildingid, 1, 0)) #Check if prediction is correct
successVal <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
print(paste("Val ",successVal))

WapVal$BUILDINGID <- prediction$buildingid



###Second iteration of Lat and Lon

repeat{  
  GlobalSuccess = successTrain
  print(GlobalSuccess)
  LatModel <-  rpart(LATITUDE ~. ,
                     data = trainData,
                     control = rpart.control(maxdepth = maxd,cp=cptree))
  
  
  prediction <- as.data.frame(predict(LatModel,newdata= testData))
  prediction <- prediction %>% mutate(latitude = prediction[,1])
  testData$LATITUDE <- prediction$latitude
  
  prediction <- as.data.frame(predict(LatModel,newdata= trainData))
  prediction <- prediction %>% mutate(latitude = prediction[,1])
  
  trainData$LATITUDE <- prediction$latitude
  
  prediction <- as.data.frame(predict(LatModel,newdata= WapVal))
  prediction <- prediction %>% mutate(latitude = prediction[,1])
  WapVal$LATITUDE <- prediction$latitude
  
  LonModel <-  rpart(LONGITUDE ~. ,
                     data = trainData,
                     control = rpart.control(maxdepth = 24,cp=cptree))
  
  
  prediction <- as.data.frame(predict(LonModel,newdata= testData))
  prediction <- prediction %>% mutate(longitude = prediction[,1])
  testData$LONGITUDE <- prediction$longitude
  
  prediction <- as.data.frame(predict(LonModel,newdata= trainData))
  prediction <- prediction %>% mutate(longitude = prediction[,1])
  trainData$LONGITUDE <- prediction$longitude
  
  prediction <- as.data.frame(predict(LonModel,newdata= WapVal))
  prediction <- prediction %>% mutate(longitude = prediction[,1])
  WapVal$LONGITUDE <- prediction$longitude
  
  Bmodel <-  rpart( BUILDINGID~. ,
                    data = trainData,
                    control = rpart.control(maxdepth = maxd,cp=cptree))
  
  prediction <- as.data.frame(round(predict(Bmodel,newdata= testData)))
  prediction <- prediction %>% mutate(prediccion = round(prediction[,1]))
  prediction <- prediction %>% mutate(resultado = Dataset[-indexes,]$BUILDINGID)
  prediction <- prediction %>% mutate(check = ifelse(resultado == prediccion, 1, 0)) #Check if prediction is correct
  successTest <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
  print(paste("Test ",successTest))
  
  
  testData$BUILDINGID <- prediction$prediccion
  
  prediction <- as.data.frame(round(predict(Bmodel,newdata= trainData)))
  prediction <- prediction %>% mutate(prediccion = round(prediction[,1]))
  prediction <- prediction %>% mutate(resultado = Dataset[indexes,]$BUILDINGID)
  prediction <- prediction %>% mutate(check = ifelse(resultado == prediccion, 1, 0)) #Check if prediction is correct
  successTrain <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
  successTrain
  
  trainData$BUILDINGID <- prediction$prediccion
  
  prediction <- as.data.frame(round(predict(Bmodel,newdata= WapVal)))
  prediction <- prediction %>% mutate(prediccion = round(prediction[,1]))
  prediction <- prediction %>% mutate(resultado = vDataset$BUILDINGID)
  prediction <- prediction %>% mutate(check = ifelse(resultado == prediccion, 1, 0)) #Check if prediction is correct
  successVal <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
  print(paste("Val ",successVal))
  
  
  WapVal$BUILDINGID <- prediction$prediccion
  
  if(GlobalSuccess >= successTrain){
    break
  }else if(successTrain == 100){
    break
  } 
}


# WapSample$BUILDINGID <- Dataset$BUILDINGID
# 
# Bmodel <-  rpart( BUILDINGID~. ,
#                   data = WapSample,
#                   control = rpart.control(maxdepth = maxd,cp=cptree))
# prediction <- as.data.frame(round(predict(Bmodel,newdata=WapSample)))
# prediction <- prediction %>% mutate(prediccion = round(prediction[,1]))
# 
# WapSample$BUILDINGID <- prediction$prediccion

#write.csv(WapSample,file="data/OnlyWapBuilding.csv",row.names=FALSE)
#write.csv(WapVal,file="data/validationDataBuilding.csv",row.names=FALSE)

