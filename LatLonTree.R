###############################
###                         ###
###   WIFI LOCATION         ###
###                         ###
###                         ###
###                         ###
###############################

library("tidyr")
library("dplyr")
library("taRifx")
library("lubridate")
library("rpart")
library("caret")

WD <- "/Users/sediaz/Documents/Ubiqum/Curso online Primavera 2017/R/Course3Task3"
setwd(WD)

source("Functions.R")

####Variables#####
Create = FALSE
cptree <- 0.000001
maxd <- 30


####Reading the tables####
Dataset <- read.csv("~/Documents/Ubiqum/Uji/DatasetClean.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
vDataset <- read.csv("~/Documents/Ubiqum/Uji/validationData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
WapVal <- read.csv("~/Documents/Ubiqum/Uji/OnlyWapValBFloor.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)
#WapVal <- read.csv("~/Documents/Ubiqum/Uji/ValidationWapNorm.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)
#WapSample <- read.csv("~/Documents/Ubiqum/Uji/OnlyWapNormCut.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)
WapSample <- read.csv("~/Documents/Ubiqum/Uji/OnlyWapBuilding.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)
OriginalDataset <- read.csv("~/Documents/Ubiqum/Uji/trainingData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)

lat0 <- min(OriginalDataset$LATITUDE)
lon0 <- min(OriginalDataset$LONGITUDE)


vDataset$LATITUDE <- vDataset$LATITUDE - lat0
vDataset$LONGITUDE <- vDataset$LONGITUDE - lon0

set.seed(123)
indexes <- createDataPartition(Dataset$WAP006, p = .80, list = FALSE)
WapSample$FLOOR <- Dataset$FLOOR
WapSample$BUILDINGID <- Dataset$BUILDINGID

trainData <- WapSample[indexes,]
testData <- WapSample[-indexes,]
validationData <- WapVal

#matamos building y floor


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

prediction <- as.data.frame(predict(LatModel,newdata= validationData))
prediction <- prediction %>% mutate(latitude = prediction[,1])

prediction <- prediction %>% mutate(resultado = vDataset$LATITUDE)
prediction <- prediction %>% mutate(distance = abs(resultado - latitude)) #Calculate distance in coordinates
ErrorLatVal <- sum(prediction$distance)/nrow(prediction) #Average error
ErrorLatVal
validationData$LATITUDE <- prediction$latitude

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

prediction <- as.data.frame(predict(LonModel,newdata= validationData))
prediction <- prediction %>% mutate(longitude = prediction[,1])
prediction <- prediction %>% mutate(resultado = vDataset$LONGITUDE)
prediction <- prediction %>% mutate(distance = abs(resultado - longitude)) #Calculate distance in coordinates
ErrorLonVal <- sum(prediction$distance)/nrow(prediction) #Average error
ErrorLonVal


validationData$LONGITUDE <- prediction$longitude



####Predicting the Building####
trainData$BUILDINGID <- Dataset[indexes,]$BUILDINGID
#testData$BUILDINGID <- Dataset[-indexes,]$BUILDINGID
#validationData$BUILDINGID <- vDataset$BUILDINGID


# repeat{  
#   GlobalSuccess = ErrorDistance
#   print(GlobalSuccess)
#   LatModel <-  rpart(LATITUDE ~. ,
#                      data = trainData,
#                      control = rpart.control(maxdepth = maxd,cp=cptree))
#   
#   
#   prediction <- as.data.frame(predict(LatModel,newdata= testData))
#   prediction <- prediction %>% mutate(latitude = prediction[,1])
#   prediction <- prediction %>% mutate(resultado = Dataset[-indexes,]$LATITUDE)
#   prediction <- prediction %>% mutate(distance = abs(resultado - latitude)) #Calculate distance in coordinates
#   ErrorLat <- sum(prediction$distance)/nrow(prediction) #Average error
#   ErrorLat
#   testData$LATITUDE <- prediction$latitude
#   
#   prediction <- as.data.frame(predict(LatModel,newdata= trainData))
#   prediction <- prediction %>% mutate(latitude = prediction[,1])
#   
#   trainData$LATITUDE <- prediction$latitude
#   
#   prediction <- as.data.frame(predict(LatModel,newdata= validationData))
#   prediction <- prediction %>% mutate(latitude = prediction[,1])
#   validationData$LATITUDE <- prediction$latitude
#   
#   LonModel <-  rpart(LONGITUDE ~. ,
#                      data = trainData,
#                      control = rpart.control(maxdepth = 24,cp=cptree))
#   
#   
#   prediction <- as.data.frame(predict(LonModel,newdata= testData))
#   prediction <- prediction %>% mutate(longitude = prediction[,1])
#   prediction <- prediction %>% mutate(resultado = Dataset[-indexes,]$LONGITUDE)
#   prediction <- prediction %>% mutate(distance = abs(resultado - longitude)) #Calculate distance in coordinates
#   ErrorLon <- sum(prediction$distance)/nrow(prediction) #Average error
#   ErrorLon
#   testData$LONGITUDE <- prediction$longitude
#   
#   prediction <- as.data.frame(predict(LonModel,newdata= trainData))
#   prediction <- prediction %>% mutate(longitude = prediction[,1])
#   trainData$LONGITUDE <- prediction$longitude
#   
#   prediction <- as.data.frame(predict(LonModel,newdata= validationData))
#   prediction <- prediction %>% mutate(longitude = prediction[,1])
#   validationData$LONGITUDE <- prediction$longitude
#   
#   ErrorDistance <- sqrt(ErrorLon^2 + ErrorLat^2)
#   ErrorDistance
#   
#   
#   if(GlobalSuccess <= ErrorDistance){
#     break
#   } 
# }


distance <- abs(validationData$LATITUDE - vDataset$LATITUDE) #Calculate distance in coordinates
ErrorLat <- sum(distance)/length(distance) #Average error
ErrorLat


distance <- abs(validationData$LONGITUDE - vDataset$LONGITUDE) #Calculate distance in coordinates
ErrorLon <- sum(distance)/length(distance) #Average error
ErrorLon

ErrorDistance <- sqrt(ErrorLon^2 + ErrorLat^2)
ErrorDistance
