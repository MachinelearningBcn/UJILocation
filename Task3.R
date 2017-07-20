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
  
WD <- "/Users/sediaz/Documents/Ubiqum/Curso online Primavera 2017/R/Course3Task3"
setwd(WD)

source("Functions.R")

###Define Variables####
Verbose = FALSE
Create = TRUE
UsePhone = FALSE
UseFloor = TRUE

####Building The Building####

if(Create){
  Dataset <- read.csv("trainingData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
  Building <- CreateBuilding(1)
  Create = FALSE
  
}

#####Measuring the number of waps with signal

NumberOfWaps <- c()
OnlyWap <- Building[,1:(ncol(Building)-9)]
for(i in 1:nrow(OnlyWap)){
  now <- length(which(OnlyWap[i,] != -100000))
  NumberOfWaps <- c(NumberOfWaps,now)    
}

if(Create) Building <- Building[-which(NumberOfWaps < 1),]

#Predicting Floor
BuildingWAP <- Building
BuildingWAP$FLOOR <- Building$FLOOR
set.seed(123)
inTraining <- createDataPartition(BuildingWAP$WAP006, p = .80, list = FALSE)
B1Train <- BuildingWAP[inTraining,]
B1Test <-  BuildingWAP[-inTraining,]
  
WifiModel <-  rpart(FLOOR ~. ,
                       data = B1Train,
                       control = rpart.control(maxdepth = 24,cp=0.00001))


prediction <- as.data.frame(predict(WifiModel,newdata= B1Test))
prediction <- prediction %>% mutate(prediccion = round(prediction[,1]))
prediction <- prediction %>% mutate(resultado = B1Test$FLOOR)
prediction <- prediction %>% mutate(check = ifelse(resultado == prediccion, 1, 0)) #Check if prediction is correct
success <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
success

### Success rate is 95.96%### 



####Creating Iterative model####
BuildingWAP <- Building[,1:(ncol(Building)-9)]
BuildingWAP$FLOOR <- Building$FLOOR
FloorModel <-  rpart(FLOOR ~. ,
                    data = BuildingWAP,
                    control = rpart.control(maxdepth = 24,cp=0.00001))
predictingFloor <- (predict(FloorModel,newdata= BuildingWAP))



#Predicting Latitude#
BuildingWAP <- Building[,1:(ncol(Building)-9)]
if(UsePhone) BuildingWAP$PHONEID <- Building$PHONEID
if(UseFloor) BuildingWAP$FLOOR <-round(predictingFloor)
BuildingWAP$LATITUDE <- Building$LATITUDE
ErrorLat <- PredictLatitude()
ErrorLat

#Predicting Longitude
BuildingWAP$LONGITUDE <- Building$LONGITUDE
ErrorLon <- PredictLongitude()
ErrorLon

ErrorDist <- sqrt(ErrorLon^2 + ErrorLat^2)
ErrorDist  ####5.67 Error

#Predicting Latitude 2


ErrorDist <- sqrt(ErrorLon^2 + ErrorLat^2)
ErrorDist  ####5.67 Error

BuildingWAP$LATITUDE <- Building$LATITUDE


repeat{
  GlobalError <- ErrorDist
  goodLon <- BuildingWAP$LONGITUDE
  goodLat <- BuildingWAP$LATITUDE
  
  LatitudeModel <-  rpart(LATITUDE ~. ,
                          data = BuildingWAP,
                          control = rpart.control(maxdepth = 24,cp=0.00001))
  PredictingLat <- (predict(LatitudeModel,newdata= BuildingWAP))
  BuildingWAP$LATITUDE <- PredictingLat
  ErrorLon <- PredictLongitude()
  ErrorLon
  
  BuildingWAP$LONGITUDE <- Building$LONGITUDE
  
  LongitudeModel <-  rpart(LONGITUDE ~. ,
                          data = BuildingWAP,
                          control = rpart.control(maxdepth = 24,cp=0.00001))
  PredictingLon <- (predict(LongitudeModel,newdata= BuildingWAP))
  BuildingWAP$LONGITUDE <- PredictingLon
  ErrorLat <- PredictLongitude()
  ErrorLat
  
  ErrorDist <- sqrt(ErrorLon^2 + ErrorLat^2)
  print(ErrorDist)
  
  if(GlobalError <= ErrorDist){
    break
  }
}

GlobalError

###Edges####
goodLatEdge <- goodLat
goodLonEdge <- goodLon

corner <-c()
for(i in 1:length(goodLatEdge)){
  if(goodLonEdge[i] < 30){
    if(goodLatEdge[i] < 50){
      corner <- c(corner,i)
      goodLatEdge[i] =50
    }
  }
}

set.seed(123)
inTraining <- createDataPartition(BuildingWAP$WAP006, p = .80, list = FALSE)
ErrorLATGREAT <- sum(abs(goodLatEdge[-inTraining] - Building[-inTraining,]$LATITUDE))/length(goodLatEdge[-inTraining])
ErrorLONGREAT <- sum(abs(goodLon[-inTraining] - Building[-inTraining,]$LONGITUDE))/length(goodLon[-inTraining])
ErrorDistGREAT <- sqrt(ErrorLONGREAT^2 + ErrorLATGREAT^2)
ErrorDistGREAT


####Outliers####
# OutlierList <- which(abs(goodLon - Building$LONGITUDE) > 40)
# 
# BuildingOut <-BuildingWAP[-OutlierList,1:(ncol(BuildingWAP))]
# 
# 
# columns_to_kill <- c()
# for(i in 1:(ncol(BuildingOut)-9)){
#   if(length(which(BuildingOut[,i]!=-100000)) ==0){ 
#     columns_to_kill <- c(columns_to_kill,i)    
#   }
# }
# 
# BuildingOut[,columns_to_kill] <- NULL



####FLOOR BY FLOOR####
Floor0 <- BuildingWAP[which(BuildingWAP$FLOOR == 0),]
# columns_to_kill <- c()
# for(i in 1:207){
#          columns_to_kill <- c(columns_to_kill,length(which(Floor0[,i]!=100)))    
#    }
# Floor0 <- Floor0[,-columns_to_kill] 
 Floor1 <- Building1[which(Building1$FLOOR == 1),]
 Floor2 <- Building1[which(Building1$FLOOR == 2),]

 
 
#######Validation##########
 
vDataset <- read.csv("validationData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)