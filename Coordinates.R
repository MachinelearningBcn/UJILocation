###############################
###                         ###
###   WIFI LOCATION         ###
###                         ###
###                         ###
###                         ###
###############################

#Creating my functions

rotate_matrix <- function(myangle, mydataframe){
  mydataframe$rot_x <- cos(myangle)*mydataframe[,1] - sin(myangle)*mydataframe[,2]
  mydataframe$rot_y <- sin(angle)*mydataframe[,1] + cos(myangle)*mydataframe[,2]
  return(mydataframe)
}

normalize_columns <- function(mydataframe){
  mydataframe <- lapply(mydataframe, function(x) (x - min(x))/(max(x)-min(x)))
  return(as.data.frame(mydataframe))
}

normalize_rows <- function(mydataframe){
  mydataframe <- as.data.frame(t(apply(mydataframe, 1, function(x) (x - min(x))/(max(x)-min(x)))))
  return(as.data.frame(mydataframe))
}

preprocess_wifi <- function(mydataframe){
  mydataframe <- mydataframe[,1:520]
  mydataframe[mydataframe ==100] <- -110
  #mydataframe <- normalize_columns(mydataframe)
  return(mydataframe)
}

library("data.table")
library("tidyr")
library("dplyr")
library("taRifx")
library("lubridate")
library("rpart")
library("caret")


WD <- "/Users/sediaz/Documents/Ubiqum/Curso online Primavera 2017/R/Course3Task3"
setwd(WD)

run_tests = TRUE
run_val  = FALSE

source("Functions.R")

####Variables#####
Create = FALSE
cptree <- 0.000001
maxd <- 30


####Reading the tables####
Dataset <- read.csv("~/Documents/Ubiqum/Uji/DatasetClean.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
WapSample <- read.csv("~/Documents/Ubiqum/Uji/OnlyWapBuilding.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)
vDataset <- read.csv("~/Documents/Ubiqum/Uji/validationData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
WapVal <- read.csv("~/Documents/Ubiqum/Uji/validationDataBuilding.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)
oDataset <- read.csv("~/Documents/Ubiqum/Uji/trainingData.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)

BuildingID <- read.csv("~/Documents/Ubiqum/Uji/DatasetBuildingID.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
valBuildingID <- read.csv("~/Documents/Ubiqum/Uji/validationBuildingID.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)


Dataset$LATITUDE <- Dataset$LATITUDE - min(Dataset$LATITUDE)
Dataset$LONGITUDE <- Dataset$LONGITUDE - min(Dataset$LONGITUDE)



###Top1 is a factor
WapSample$Top1 <- NULL
WapVal$Top1 <- NULL

set.seed(123)
indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)
trainData <- WapSample[indexes,1:(ncol(WapSample))]
testData <- WapSample[-indexes,1:(ncol(WapSample))]

trueTrain <-  Dataset[indexes,]
trueTest <- Dataset[-indexes,]


trainData$FLOOR <- Dataset[indexes,]$FLOOR
testData$FLOOR <- Dataset[-indexes,]$FLOOR
trainData$BUILDINGID <- Dataset[indexes,]$BUILDINGID
#WapVal$BUILDINGID <- as.numeric(WapVal$BUILDINGID)
WapVal$BUILDINGID <- as.numeric(vDataset$BUILDINGID)

####Floor Building 0####
#Building 0
trainDataB0 <- trainData[which(trainData$BUILDINGID ==0),]
testDataB0 <- testData[which(testData$BUILDINGID ==0),]

WapValB0 <- WapVal[which(WapVal$BUILDINGID ==0),]
vDatasetB0 <- vDataset[which(WapVal$BUILDINGID ==0),]

trainDataB0$BUILDINGID<- NULL
WapValB0$BUILDINGID <- NULL
testDataB0$BUILDINGID <- NULL

trainDataB0[trainDataB0 < 0.35] <- 0
WapValB0[WapValB0 < 0.35] <- 0
testDataB0[testDataB0 < 0.35] <- 0

knnmodel <- knn3(FLOOR ~. , trainDataB0, k=3)

if(run_tests){
  result <- as.data.frame(round(predict(knnmodel,newdata = testDataB0)))

  myfloor<-c()
  for(i in 1:nrow(result)){
    if(length(which(result[i,] ==1))>0){
      myf <- which(result[i,] ==1) -1
    }else{
      myf <- 0
    }
    myfloor <- c(myfloor,myf )
  }
  myfloor_b0_test <- myfloor


  prediction <- as.data.frame(myfloor)
  prediction$floorid <- myfloor
  prediction <- prediction %>% mutate(resultado = testDataB0$FLOOR)
  prediction <- prediction %>% mutate(check = ifelse(resultado == floorid, 1, 0)) #Check if prediction is correct
  successTest <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
  successTest
}

if(run_val){
  result <- as.data.frame(round(predict(knnmodel,newdata = WapValB0)))

  myfloor<-c()
  for(i in 1:nrow(result)){
    if(length(which(result[i,] ==1))>0){
      myf <- which(result[i,] ==1) -1
    }else{
      myf <- 0
    }
    myfloor <- c(myfloor,myf )
  }
  myfloor0 <- myfloor

  prediction <- as.data.frame(myfloor)
  prediction$floorid <- myfloor
  prediction <- prediction %>% mutate(resultado = vDatasetB0$FLOOR)
  prediction <- prediction %>% mutate(check = ifelse(resultado == floorid, 1, 0)) #Check if prediction is correct
  successVal <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
  successVal
}

####Building 1####
trainData$FLOOR <- Dataset[indexes,]$FLOOR
testData$FLOOR <- Dataset[-indexes,]$FLOOR
trainData$BUILDINGID <- Dataset[indexes,]$BUILDINGID
WapVal$BUILDINGID <- as.numeric(WapVal$BUILDINGID)

trainDataB0 <- trainData[which(trainData$BUILDINGID ==1),]
testDataB0 <- testData[which(testData$BUILDINGID ==1),]

WapValB0 <- WapVal[which(WapVal$BUILDINGID ==1),]
vDatasetB0 <- vDataset[which(WapVal$BUILDINGID ==1),]

trainDataB0$BUILDINGID <- NULL
WapValB0$BUILDINGID <- NULL
testDataB0$BUILDINGID <- NULL

trainDataB0[trainDataB0 < 0.8] <- 0
WapValB0[WapValB0 < 0.8] <- 0
testDataB0[testDataB0 < 0.8] <- 0

Neighbors = 3

knnmodel <- knn3(FLOOR ~. , trainDataB0, k= Neighbors)

if(run_tests){
  result <- as.data.frame(round(predict(knnmodel,newdata = testDataB0)))

  myfloor<-c()
  for(i in 1:nrow(result)){
    if(length(which(result[i,] ==1))>0){
      myf <- which(result[i,] ==1) -1
    }else{
      myf <- 0
    }
    myfloor <- c(myfloor,myf )
  }
  myfloor_b1_test <- myfloor

  prediction <- as.data.frame(myfloor)
  prediction$floorid <- myfloor
  prediction <- prediction %>% mutate(resultado = testDataB0$FLOOR)
  prediction <- prediction %>% mutate(check = ifelse(resultado == floorid, 1, 0)) #Check if prediction is correct
  successTest <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
  successTest
}

#Validation Floor 
if(run_val){
  result <- as.data.frame(round(predict(knnmodel,newdata = WapValB0)))

  myfloor<-c()
  for(i in 1:nrow(result)){
    if(length(which(result[i,] ==1))>0){
        myf <- which(result[i,] ==1) -1
    }else{
      myf <- 0
    }
    myfloor <- c(myfloor,myf )
  }
  myfloor1 <- myfloor


  prediction <- as.data.frame(myfloor)
  prediction$floorid <- myfloor
  prediction <- prediction %>% mutate(resultado = vDatasetB0$FLOOR)
  prediction <- prediction %>% mutate(check = ifelse(resultado == floorid, 1, 0)) #Check if prediction is correct
  successVal <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
  successVal
}

####Building 2####
trainDataB0 <- trainData[which(trainData$BUILDINGID ==2),]
testDataB0 <- testData[which(testData$BUILDINGID ==2),]

WapValB0 <- WapVal[which(WapVal$BUILDINGID ==2),]
vDatasetB0 <- vDataset[which(WapVal$BUILDINGID ==2),]

trainDataB0$BUILDINGID<- NULL
WapValB0$BUILDINGID <- NULL
testDataB0$BUILDINGID <- NULL  

trainDataB0[trainDataB0 < 0.35] <- 0
WapValB0[WapValB0 < 0.35] <- 0
testDataB0[testDataB0 < 0.35] <- 0 

knnmodel <- knn3(FLOOR ~. ,trainDataB0,k=3)
#Predicting Floor in Building 2
if(run_tests){
  result <- as.data.frame(round(predict(knnmodel,newdata = testDataB0)))

  myfloor<-c()
  for(i in 1:nrow(result)){
    if(length(which(result[i,] ==1))>0){
      myf <- which(result[i,] ==1) -1
    }else{
      myf <- 0
    }
    myfloor <- c(myfloor,myf )
  }
  myfloor_b2_test <- myfloor

  prediction <- as.data.frame(myfloor)
  prediction$floorid <- myfloor
  prediction <- prediction %>% mutate(resultado = testDataB0$FLOOR)
  prediction <- prediction %>% mutate(check = ifelse(resultado == floorid, 1, 0)) #Check if prediction is correct
  successTest <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
  successTest
}
if(run_val){

  result <- as.data.frame(round(predict(knnmodel,newdata = WapValB0)))

  myfloor<-c()
  for(i in 1:nrow(result)){
    if(length(which(result[i,] ==1))>0){
      myf <- which(result[i,] ==1) -1
    }else{
      myf <- 0
    }
    myfloor <- c(myfloor,myf )
  }
  myfloor2 <- myfloor

  prediction <- as.data.frame(myfloor)
  prediction$floorid <- myfloor
  prediction <- prediction %>% mutate(resultado = vDatasetB0$FLOOR)
  prediction <- prediction %>% mutate(check = ifelse(resultado == floorid, 1, 0)) #Check if prediction is correct
  successVal <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
  successVal
}

####PLots of error by building####
if(run_val){
  #Building 0
  neighbors <- c(3, 5, 6, 7, 9)
  errors <- c(6.07, 5.91, 5.76, 5.89, 6.21)
  knn_dataframe <- as.data.frame(neighbors)
  knn_dataframe$Errors <- errors
  ggplot() + geom_point(data = knn_dataframe, aes(x = neighbors, y = errors)) + 
    geom_line(data = knn_dataframe, aes(x = neighbors, y = errors))

  #Building 1
  neighbors <- c(3, 5, 6, 7, 9)
  errors <- c(11.65, 11.68, 11.72, 11.52, 11.59)
  knn_dataframe <- as.data.frame(neighbors)
  knn_dataframe$Errors <- errors
  ggplot() + geom_point(data = knn_dataframe, aes(x = neighbors, y = errors)) + 
    geom_line(data = knn_dataframe, aes(x = neighbors, y = errors))

  #Building 2
  neighbors <- c(3, 5, 6, 7, 9)
  errors <- c(10.47, 10.79, 10.59, 10.84, 10.77)
  knn_dataframe <- as.data.frame(neighbors)
  knn_dataframe$Errors <- errors
  ggplot() + geom_point(data = knn_dataframe, aes(x = neighbors, y = errors)) + 
    geom_line(data = knn_dataframe, aes(x = neighbors, y = errors))


}

####LATITUDE ####
#### knn Building 0 ####
#Building 0
#Just in case
set.seed(123)
indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)
trainData <- WapSample[indexes,1:(ncol(WapSample))]
testData <- WapSample[-indexes,1:(ncol(WapSample))]

trainData$LATITUDE <- Dataset[indexes,]$LATITUDE
testData$LATITUDE <- Dataset[-indexes, ]$LATITUDE
WapVal$LATITUDE <- vDataset$LATITUDE - min(oDataset$LATITUDE)

trainDataB0 <- trainData[which(trainData$BUILDINGID == 0), ]
testDataB0 <- testData[which(testData$BUILDINGID == 0), ]

WapValB0 <- WapVal[which(WapVal$BUILDINGID == 0), ]
vDatasetB0 <- vDataset[which(WapVal$BUILDINGID == 0), ]

indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)

#create knn model


#knnmodelLat <- knn3(LATITUDE ~. - BUILDINGID, trainDataB0, k=3) # Error in latitude in Val is 6.07
#knnmodelLat <- knn3(LATITUDE ~. - BUILDINGID, trainDataB0, k=5) # Error in latitude in Val is 5.91
knnmodelLat <- knn3(LATITUDE ~. - BUILDINGID, trainDataB0, k=6) # Error in latitude in Val is 5.76
#knnmodelLat <- knn3(LATITUDE ~. - BUILDINGID, trainDataB0, k=7) # Error in latitude in Val is 5.89
#knnmodelLat <- knn3(LATITUDE ~. - BUILDINGID, trainDataB0, k=9) # Error in latitude in Val is 6.21
if(run_tests){
  result <- as.data.frame(predict(knnmodelLat,newdata = testDataB0))

  predicted_lat <- round(apply(result, 1, function(x) as.numeric(names(which.max(x)))),2)
  predicted_lat
  lat_b0_test_knn <- predicted_lat
  distance <- testDataB0$LATITUDE - predicted_lat
  plot(distance)
  my_distance <- sqrt(sum(distance*distance)/length(distance))
  my_distance
  distance_B0_knn_test <- distance
}

if(run_val){
  result <- as.data.frame(predict(knnmodelLat,newdata = WapValB0))

  predicted_lat <- round(apply(result, 1, function(x) as.numeric(names(which.max(x)))),2)
  predicted_lat_b0 <- predicted_lat
  distance <- WapValB0$LATITUDE - predicted_lat
  plot(distance)
  my_distance <- sqrt(sum(distance*distance)/length(distance))
  my_distance #6.07
  distance_B0_knn <- distance
}
#### knn Building 1 ####
#Building 0
#Just in case
set.seed(123)
indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)
trainData <- WapSample[indexes,1:(ncol(WapSample))]
testData <- WapSample[-indexes,1:(ncol(WapSample))]

trainData$LATITUDE <- Dataset[indexes,]$LATITUDE
testData$LATITUDE <- Dataset[-indexes, ]$LATITUDE
WapVal$LATITUDE <- vDataset$LATITUDE - min(oDataset$LATITUDE)

trainDataB0 <- trainData[which(trainData$BUILDINGID == 1), ]
testDataB0 <- testData[which(testData$BUILDINGID == 1), ]

WapValB0 <- WapVal[which(WapVal$BUILDINGID == 1), ]
vDatasetB0 <- vDataset[which(WapVal$BUILDINGID == 1), ]

indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)

#create knn model
#knnmodelLat <- knn3(LATITUDE ~. - BUILDINGID, trainDataB0, k=3) # Error in latitude in Val is 11.65
#knnmodelLat <- knn3(LATITUDE ~. - BUILDINGID, trainDataB0, k=5) # Error in latitude in Val is 11.68
#knnmodelLat <- knn3(LATITUDE ~. - BUILDINGID, trainDataB0, k=6) # Error in latitude in Val is 11.72
knnmodelLat <- knn3(LATITUDE ~. - BUILDINGID, trainDataB0, k=7) # Error in latitude in Val is 11.52
#knnmodelLat <- knn3(LATITUDE ~. - BUILDINGID, trainDataB0, k=9) # Error in latitude in Val is 11.59

if(run_tests){
  result <- as.data.frame(predict(knnmodelLat,newdata = testDataB0))

  predicted_lat <- round(apply(result, 1, function(x) as.numeric(names(which.max(x)))),2)
  lat_b1_test_knn <- predicted_lat
  distance <- testDataB0$LATITUDE - predicted_lat
  plot(distance)
  my_distance <- sqrt(sum(distance*distance)/length(distance))
  my_distance
}

if(run_val){
  result <- as.data.frame(predict(knnmodelLat,newdata = WapValB0))

  predicted_lat <- round(apply(result, 1, function(x) as.numeric(names(which.max(x)))),2)
  predicted_lat
  distance <- WapValB0$LATITUDE - predicted_lat
  distance_B1_knn <- distance

  plot(distance)
  my_distance <- sqrt(sum(distance*distance)/length(distance))
  my_distance #6.07
}




#### knn Building 2 ####
#Building 0
#Just in case
set.seed(123)
indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)
trainData <- WapSample[indexes,1:(ncol(WapSample))]
testData <- WapSample[-indexes,1:(ncol(WapSample))]

trainData$LATITUDE <- Dataset[indexes,]$LATITUDE
testData$LATITUDE <- Dataset[-indexes, ]$LATITUDE
WapVal$LATITUDE <- vDataset$LATITUDE - min(oDataset$LATITUDE)

trainDataB0 <- trainData[which(trainData$BUILDINGID == 2), ]
testDataB0 <- testData[which(testData$BUILDINGID == 2), ]

WapValB0 <- WapVal[which(WapVal$BUILDINGID == 2), ]
vDatasetB0 <- vDataset[which(WapVal$BUILDINGID == 2), ]

indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)

#create knn model
#knnmodelLat <- knn3(LATITUDE ~. - BUILDINGID, trainDataB0, k=1) # Error in latitude in Val is 12.11
knnmodelLat <- knn3(LATITUDE ~. - BUILDINGID, trainDataB0, k=3) # Error in latitude in Val is 10.47
#knnmodelLat <- knn3(LATITUDE ~. - BUILDINGID, trainDataB0, k=5) # Error in latitude in Val is 10.79
#knnmodelLat <- knn3(LATITUDE ~. - BUILDINGID, trainDataB0, k=6) # Error in latitude in Val is 10.59
#knnmodelLat <- knn3(LATITUDE ~. - BUILDINGID, trainDataB0, k=7) # Error in latitude in Val is 10.84
#knnmodelLat <- knn3(LATITUDE ~. - BUILDINGID, trainDataB0, k=9) # Error in latitude in Val is 10.77
if(run_tests){
  result <- as.data.frame(predict(knnmodelLat,newdata = testDataB0))

  predicted_lat <- round(apply(result, 1, function(x) as.numeric(names(which.max(x)))),2)
  lat_b2_test_knn <- predicted_lat
  distance <- testDataB0$LATITUDE - predicted_lat
  plot(distance)
  my_distance <- sqrt(sum(distance*distance)/length(distance))
  my_distance
}

if(run_val){
  result <- as.data.frame(predict(knnmodelLat,newdata = WapValB0))

  predicted_lat <- round(apply(result, 1, function(x) as.numeric(names(which.max(x)))),2)
  predicted_lat
  distance <- WapValB0$LATITUDE - predicted_lat
  distance_B2_knn <- distance
  plot(distance)
  my_distance <- sqrt(sum(distance*distance)/length(distance))
  my_distance #6.07
}





###LONGITUDE####
#### knn Building 0 ####
#Building 0
#Just in case
set.seed(123)
indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)
trainData <- WapSample[indexes,1:(ncol(WapSample))]
testData <- WapSample[-indexes,1:(ncol(WapSample))]

trainData$LONGITUDE <- Dataset[indexes,]$LONGITUDE
testData$LONGITUDE <- Dataset[-indexes, ]$LONGITUDE
WapVal$LONGITUDE <- vDataset$LONGITUDE - min(oDataset$LONGITUDE)

trainDataB0 <- trainData[which(trainData$BUILDINGID == 0), ]
testDataB0 <- testData[which(testData$BUILDINGID == 0), ]

WapValB0 <- WapVal[which(WapVal$BUILDINGID == 0), ]
vDatasetB0 <- vDataset[which(WapVal$BUILDINGID == 0), ]

indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)

#create knn model


#knnmodelLat <- knn3(LONGITUDE ~. - BUILDINGID, trainDataB0, k=3) # Error in LONGITUDE in Val is 6.91
#knnmodelLat <- knn3(LONGITUDE ~. - BUILDINGID, trainDataB0, k=5) # Error in LONGITUDE in Val is 7.26
#knnmodelLat <- knn3(LONGITUDE ~. - BUILDINGID, trainDataB0, k=6) # Error in LONGITUDE in Val is 6.94
knnmodelLat <- knn3(LONGITUDE ~. - BUILDINGID, trainDataB0, k=7) # Error in LONGITUDE in Val is 6.83
#knnmodelLat <- knn3(LONGITUDE ~. - BUILDINGID, trainDataB0, k=9) # Error in LONGITUDE in Val is 7.24

if(run_tests){
  result <- as.data.frame(predict(knnmodelLat,newdata = testDataB0))

  predicted_lon <- round(apply(result, 1, function(x) as.numeric(names(which.max(x)))),2)
  lon_b0_test_knn <- predicted_lon
  distance <- testDataB0$LONGITUDE - predicted_lon
  #plot(distance)
  my_distance <- sqrt(sum(distance*distance)/length(distance))
  my_distance
}

if(run_val){
  result <- as.data.frame(predict(knnmodelLat,newdata = WapValB0))

  predicted_lat <- round(apply(result, 1, function(x) as.numeric(names(which.max(x)))),2)
  predicted_lat
  distance <- WapValB0$LONGITUDE - predicted_lat
  #plot(distance)
  my_distance <- sqrt(sum(distance*distance)/length(distance))
  my_distance #6.07
  distance_B0_knn <- distance
}



#### knn Building 1 ####
#Building 0
#Just in case
set.seed(123)
indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)
trainData <- WapSample[indexes,1:(ncol(WapSample))]
testData <- WapSample[-indexes,1:(ncol(WapSample))]

trainData$LONGITUDE <- Dataset[indexes,]$LONGITUDE
testData$LONGITUDE <- Dataset[-indexes, ]$LONGITUDE
WapVal$LONGITUDE <- vDataset$LONGITUDE - min(oDataset$LONGITUDE)

trainDataB0 <- trainData[which(trainData$BUILDINGID == 1), ]
testDataB0 <- testData[which(testData$BUILDINGID == 1), ]

WapValB0 <- WapVal[which(WapVal$BUILDINGID == 1), ]
vDatasetB0 <- vDataset[which(WapVal$BUILDINGID == 1), ]

indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)

#create knn model
#knnmodelLat <- knn3(LONGITUDE ~. - BUILDINGID, trainDataB0, k=3) # Error in LONGITUDE in Val is 9.84
knnmodelLat <- knn3(LONGITUDE ~. - BUILDINGID, trainDataB0, k=5) # Error in LONGITUDE in Val is 
#knnmodelLat <- knn3(LONGITUDE ~. - BUILDINGID, trainDataB0, k=6) # Error in LONGITUDE in Val is 
#knnmodelLat <- knn3(LONGITUDE ~. - BUILDINGID, trainDataB0, k=7) # Error in LONGITUDE in Val is 9.45
#knnmodelLat <- knn3(LONGITUDE ~. - BUILDINGID, trainDataB0, k=9) # Error in LONGITUDE in Val is 

if(run_tests){
  result <- as.data.frame(predict(knnmodelLat,newdata = testDataB0))

  predicted_lon <- round(apply(result, 1, function(x) as.numeric(names(which.max(x)))),2)
  lon_b1_test_knn <- predicted_lon
  distance <- testDataB0$LONGITUDE - predicted_lon
  plot(distance)
  my_distance <- sqrt(sum(distance*distance)/length(distance))
  my_distance
}

if(run_val){
  result <- as.data.frame(predict(knnmodelLat,newdata = WapValB0))

  predicted_lat <- round(apply(result, 1, function(x) as.numeric(names(which.max(x)))),2)
  predicted_lat
  distance <- WapValB0$LONGITUDE - predicted_lat
  distance_B1_knn <- distance

  plot(distance)
  my_distance <- sqrt(sum(distance*distance)/length(distance))
  my_distance #6.07
}


#### knn Building 2 ####
#Building 0
#Just in case
set.seed(123)
indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)
trainData <- WapSample[indexes,1:(ncol(WapSample))]
testData <- WapSample[-indexes,1:(ncol(WapSample))]

trainData$LONGITUDE <- Dataset[indexes,]$LONGITUDE
testData$LONGITUDE <- Dataset[-indexes, ]$LONGITUDE
WapVal$LONGITUDE <- vDataset$LONGITUDE - min(oDataset$LONGITUDE)

trainDataB0 <- trainData[which(trainData$BUILDINGID == 2), ]
testDataB0 <- testData[which(testData$BUILDINGID == 2), ]

WapValB0 <- WapVal[which(WapVal$BUILDINGID == 2), ]
vDatasetB0 <- vDataset[which(WapVal$BUILDINGID == 2), ]

indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)

#create knn model
#knnmodelLat <- knn3(LONGITUDE ~. - BUILDINGID, trainDataB0, k=3) # Error in LONGITUDE in Val is 11.65
#knnmodelLat <- knn3(LONGITUDE ~. - BUILDINGID, trainDataB0, k=5) # Error in LONGITUDE in Val is 11.68
#knnmodelLat <- knn3(LONGITUDE ~. - BUILDINGID, trainDataB0, k=6) # Error in LONGITUDE in Val is 11.72
knnmodelLat <- knn3(LONGITUDE ~. - BUILDINGID, trainDataB0, k=7) # Error in LONGITUDE in Val is 11.52
#knnmodelLat <- knn3(LONGITUDE ~. - BUILDINGID, trainDataB0, k=9) # Error in LONGITUDE in Val is 11.59


if(run_tests){
  result <- as.data.frame(predict(knnmodelLat,newdata = testDataB0))

  predicted_lon <- round(apply(result, 1, function(x) as.numeric(names(which.max(x)))),2)
  lon_b2_test_knn <- predicted_lon
  distance <- testDataB0$LONGITUDE - predicted_lat
  #plot(distance)
  my_distance <- sqrt(sum(distance*distance)/length(distance))
  my_distance
}

if(run_val){

  result <- as.data.frame(predict(knnmodelLat,newdata = WapValB0))

  predicted_lat <- round(apply(result, 1, function(x) as.numeric(names(which.max(x)))),2)
  predicted_lat
  distance <- WapValB0$LONGITUDE - predicted_lat
  distance_B2_knn <- distance

  #plot(distance)
  my_distance <- sqrt(sum(distance*distance)/length(distance))
  my_distance #6.07
}

####Putting Everything Together####

if(run_tests){
  
  
  #Creating vector of floor predictions
  floor_test <- c()
  floor_test[which(testData$BUILDINGID == 0)]<- myfloor_b0_test
  floor_test[which(testData$BUILDINGID == 1)]<- myfloor_b1_test
  floor_test[which(testData$BUILDINGID == 2)]<- myfloor_b2_test
  
  #Creating vector of latitude predictions
  lat_test <- c()
  lat_test[which(testData$BUILDINGID == 0)]<- lat_b0_test_knn
  lat_test[which(testData$BUILDINGID == 1)]<- lat_b1_test_knn
  lat_test[which(testData$BUILDINGID == 2)]<- lat_b2_test_knn
  
  #Creating vector of longitude predictions
  lon_test <- c()
  lon_test[which(testData$BUILDINGID == 0)]<- lon_b0_test_knn
  lon_test[which(testData$BUILDINGID == 1)]<- lon_b1_test_knn
  lon_test[which(testData$BUILDINGID == 2)]<- lon_b2_test_knn
  
  
  PredictedTest <- as.data.frame(floor_test)
  names(PredictedTest) <- "Floor_pred"
  PredictedTest$Floor_real <- Dataset[-indexes,"FLOOR"]
  PredictedTest$Building_real <- testData$BUILDINGID
  PredictedTest$Building_pred <- testData$BUILDINGID
  PredictedTest <- PredictedTest[,c(3,4,2,1)]
  PredictedTest$Latitude_real <- Dataset[-indexes,"LATITUDE"]
  PredictedTest$Latitude_pred <- lat_test  
  PredictedTest$Longitude_real <- Dataset[-indexes,"LONGITUDE"] 
  PredictedTest$Longitude_pred <- lon_test
  PredictedTest <- PredictedTest %>% mutate(Distance = sqrt((Latitude_real - Latitude_pred)^2+
                                                (Longitude_real - Longitude_pred)^2))
  colMeans(PredictedTest)
  

  PredictedTest <- PredictedTest %>% mutate(Distance = Distance + 4*abs(Floor_real - Floor_pred))
  mean(PredictedTest$Distance)                                            
  head(PredictedTest)
  
  
}

if(run_val){
  WapVal$FLOOR[which(WapVal$BUILDINGID == 0)]<-myfloor0
  WapVal$FLOOR[which(WapVal$BUILDINGID == 1)]<-myfloor1
  WapVal$FLOOR[which(WapVal$BUILDINGID == 2)]<-myfloor2
  
  wrong_floor <- which(WapVal$FLOOR != vDataset$FLOOR)
  wrong_b <- which(WapVal$BUILDINGID != vDataset$BUILDINGID)
  
  floor_accuracy <- round((1 - length(wrong_floor)/length(WapVal$FLOOR)) * 100,2)
  floor_accuracy
  
  write.csv(WapVal,file="~/Documents/Ubiqum/Uji/OnlyWapValDataBFloor.csv",row.names=FALSE)
  
  
  WrongCases <- vDataset[wrong_b,]
  ODataset <- read.csv("~/Documents/Ubiqum/Uji/trainingData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
  WrongCases$LATITUDE <- WrongCases$LATITUDE - min(ODataset$LATITUDE)
  WrongCases$LONGITUDE <- WrongCases$LONGITUDE - min(ODataset$LONGITUDE)
  
  ggplot(data=Dataset) + geom_point(aes(x=LONGITUDE,y=LATITUDE))+ xlab("Latitude (m)") + ylab("Longitude (m)") +
    geom_point(data=WrongCases,aes(x=LONGITUDE,y=LATITUDE,color="red")) + theme(legend.position="none") + xlim(0,400) + ylim(0,400)
  
  
  ggplot(data=vDataset) + geom_point(aes(x=LONGITUDE - min(ODataset$LONGITUDE),y=LATITUDE - min(ODataset$LATITUDE)))+ xlab("Latitude (m)") + ylab("Longitude (m)") + theme(legend.position="none") + xlim(0,400) + ylim(0,400)
  
  WrongCases <- vDataset[wrong_floor,]
  WrongCases$LATITUDE <- WrongCases$LATITUDE - min(ODataset$LATITUDE)
  WrongCases$LONGITUDE <- WrongCases$LONGITUDE - min(ODataset$LONGITUDE)
  ggplot(data=Dataset) + geom_point(aes(x=LONGITUDE,y=LATITUDE))+ xlab("Latitude (m)") + ylab("Longitude (m)") +
    geom_point(data=WrongCases,aes(x=LONGITUDE,y=LATITUDE,color="red")) + theme(legend.position="none") + xlim(0,400) + ylim(0,400)
  
  
  for (i in 0:4){
    floor0 <- Dataset[which(Dataset$FLOOR ==i),]
    WrongCases <- vDataset[wrong_floor,]
    WrongCases <- WrongCases[which(WrongCases$FLOOR ==i),]
    WrongCases$LATITUDE <- WrongCases$LATITUDE - min(ODataset$LATITUDE)
    WrongCases$LONGITUDE <- WrongCases$LONGITUDE - min(ODataset$LONGITUDE)
    
    p <-  ggplot(data=floor0) + geom_point(aes(x=LONGITUDE,y=LATITUDE),size=1)+ xlab("Latitude (m)") + ylab("Longitude (m)") +
      geom_point(data=WrongCases,aes(x=LONGITUDE,y=LATITUDE,color="red"),size=2) + theme(legend.position="none") + xlim(0,400) + ylim(0,400)
    print(p)
  }
  
  floor0 <- Dataset[which(Dataset$FLOOR ==4),]
  WrongCases <- vDataset[wrong_floor,]
  WrongCases <- WrongCases[which(WrongCases$FLOOR ==4),]
  WrongCases$LATITUDE <- WrongCases$LATITUDE - min(ODataset$LATITUDE)
  WrongCases$LONGITUDE <- WrongCases$LONGITUDE - min(ODataset$LONGITUDE)
  
  ggplot(data=floor0) + geom_point(aes(x=LONGITUDE,y=LATITUDE),size=1)+ xlab("Latitude (m)") + ylab("Longitude (m)") +
    geom_point(data=WrongCases,aes(x=LONGITUDE,y=LATITUDE,color="red"),size=2) + theme(legend.position="none") + xlim(0,400) + ylim(0,400)
  
}




####RANDOM FOREST####
RanFor <- FALSE
if(RanFor){
  #LATITUDE
  #### random forest Building 0 ####
#Building 0
#Just in case
set.seed(123)
indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)
trainData <- WapSample[indexes,1:(ncol(WapSample))]
testData <- WapSample[-indexes,1:(ncol(WapSample))]

trainData$LATITUDE <- Dataset[indexes,]$LATITUDE
testData$LATITUDE <- Dataset[-indexes, ]$LATITUDE
WapVal$LATITUDE <- vDataset$LATITUDE - min(oDataset$LATITUDE)

trainDataB0 <- trainData[which(trainData$BUILDINGID == 0), ]
testDataB0 <- testData[which(testData$BUILDINGID == 0), ]

WapValB0 <- WapVal[which(WapVal$BUILDINGID == 0), ]
vDatasetB0 <- vDataset[which(WapVal$BUILDINGID == 0), ]

indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)

#create knn model


rf_model <-  rpart(LATITUDE ~. - BUILDINGID,
                   data = trainDataB0,
                   control = rpart.control(maxdepth = 30,cp=0.0001))


pred <- as.data.frame(predict(rf_model,newdata= testDataB0)) #predict test dataset
pred <- pred %>% mutate(prediccion = pred[,1]) 
distance<- testDataB0$LATITUDE - pred$prediccion #Check if prediction is correct

my_distance <- sqrt(sum(distance*distance)/length(distance))
plot(distance)
my_distance



pred <- as.data.frame(predict(rf_model,newdata = WapValB0))
pred <- pred %>% mutate(prediccion = pred[,1]) 
distance <- WapValB0$LATITUDE - pred$prediccion #Check if prediction is correct
distance_B0_rf <- distance
my_distance <- sqrt(sum(distance*distance)/length(distance))
plot(distance)
my_distance
#Error in Latitude is 3.6 m
#Horrible


####Combinando errores de knn y rf #### Error Latitud: 4.10
mybesterror <- as.data.frame(distance_B0_knn)
mybesterror$distance_B0_rf <- distance_B0_rf
mybesterror <- mybesterror %>% mutate(best_distance = ifelse(abs(distance_B0_rf) > abs(distance_B0_knn)
                                                             , distance_B0_knn, distance_B0_rf)) 

my_distance <- sqrt(sum(mybesterror$best_distance*mybesterror$best_distance)/length(mybesterror$best_distance))
my_distance


####PREPROCESSS#######
####Probando preprocesado####
DatasetWaps <- preprocess_wifi(Dataset)
extrange_rows <- which(apply(DatasetWaps, 1, max) > -30)
DatasetWaps <- DatasetWaps[-extrange_rows,]
DatasetWaps <- normalize_rows(DatasetWaps)


#DatasetWaps <- normalize_rows(DatasetWaps)
View(DatasetWaps)

DatasetWaps2 <- preprocess_wifi(Dataset)
View(DatasetWaps2)



#### random forest Building 1 ####
#Building 1
#Just in case
set.seed(123)
indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)
trainData <- WapSample[indexes,1:(ncol(WapSample))]
testData <- WapSample[-indexes,1:(ncol(WapSample))]

trainData$LATITUDE <- Dataset[indexes,]$LATITUDE
testData$LATITUDE <- Dataset[-indexes, ]$LATITUDE
WapVal$LATITUDE <- vDataset$LATITUDE - min(oDataset$LATITUDE)

trainDataB0 <- trainData[which(trainData$BUILDINGID == 1), ]
testDataB0 <- testData[which(testData$BUILDINGID == 1), ]

WapValB0 <- WapVal[which(WapVal$BUILDINGID == 1), ]
vDatasetB0 <- vDataset[which(WapVal$BUILDINGID == 1), ]

indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)

#create knn model


rf_model <-  rpart(LATITUDE ~. - BUILDINGID,
                   data = trainDataB0,
                   control = rpart.control(maxdepth = 30,cp=0.0001))


pred <- as.data.frame(predict(rf_model,newdata= testDataB0)) #predict test dataset
pred <- pred %>% mutate(prediccion = pred[,1]) 
distance<- testDataB0$LATITUDE - pred$prediccion #Check if prediction is correct

my_distance <- sqrt(sum(distance*distance)/length(distance))
plot(distance)
my_distance



pred <- as.data.frame(predict(rf_model,newdata = WapValB0))
pred <- pred %>% mutate(prediccion = pred[,1]) 
distance <- WapValB0$LATITUDE - pred$prediccion #Check if prediction is correct
distance_B1_rf <- distance
my_distance <- sqrt(sum(distance*distance)/length(distance))
plot(distance)
my_distance
#Error in Latitude is 3.6 m
#Horrible


####Combinando errores de knn y rf #### Error Latitud: 4.10
mybesterror <- as.data.frame(distance_B1_knn)
mybesterror$distance_B1_rf <- distance_B1_rf
mybesterror <- mybesterror %>% mutate(best_distance = ifelse(abs(distance_B1_rf) > abs(distance_B1_knn)
                                                             , distance_B1_knn, distance_B1_rf)) 

my_distance <- sqrt(sum(mybesterror$best_distance*mybesterror$best_distance)/length(mybesterror$best_distance))
my_distance



  #### random forest Building 2 ####
#Building 2
#Just in case
set.seed(123)
indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)
trainData <- WapSample[indexes,1:(ncol(WapSample))]
testData <- WapSample[-indexes,1:(ncol(WapSample))]

trainData$LATITUDE <- Dataset[indexes,]$LATITUDE
testData$LATITUDE <- Dataset[-indexes, ]$LATITUDE
WapVal$LATITUDE <- vDataset$LATITUDE - min(oDataset$LATITUDE)

trainDataB0 <- trainData[which(trainData$BUILDINGID == 2), ]
testDataB0 <- testData[which(testData$BUILDINGID == 2), ]

WapValB0 <- WapVal[which(WapVal$BUILDINGID == 2), ]
vDatasetB0 <- vDataset[which(WapVal$BUILDINGID == 2), ]

indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)

#create knn model


rf_model <-  rpart(LATITUDE ~. - BUILDINGID,
                   data = trainDataB0,
                   control = rpart.control(maxdepth = 30,cp=0.0001))


pred <- as.data.frame(predict(rf_model,newdata= testDataB0)) #predict test dataset
pred <- pred %>% mutate(prediccion = pred[,1]) 
distance<- testDataB0$LATITUDE - pred$prediccion #Check if prediction is correct

my_distance <- sqrt(sum(distance*distance)/length(distance))
plot(distance)
my_distance



pred <- as.data.frame(predict(rf_model,newdata = WapValB0))
pred <- pred %>% mutate(prediccion = pred[,1]) 
distance <- WapValB0$LATITUDE - pred$prediccion #Check if prediction is correct
distance_B2_rf <- distance
my_distance <- sqrt(sum(distance*distance)/length(distance))
plot(distance)
my_distance
#Error in Latitude is 3.6 m
#Horrible




  #Longitude
  #### random forest Building 0 ####
#Building 0
#Just in case
set.seed(123)
indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)
trainData <- WapSample[indexes,1:(ncol(WapSample))]
testData <- WapSample[-indexes,1:(ncol(WapSample))]

trainData$LONGITUDE <- Dataset[indexes,]$LONGITUDE
testData$LONGITUDE <- Dataset[-indexes, ]$LONGITUDE
WapVal$LONGITUDE <- vDataset$LONGITUDE - min(oDataset$LONGITUDE)

trainDataB0 <- trainData[which(trainData$BUILDINGID == 0), ]
testDataB0 <- testData[which(testData$BUILDINGID == 0), ]

WapValB0 <- WapVal[which(WapVal$BUILDINGID == 0), ]
vDatasetB0 <- vDataset[which(WapVal$BUILDINGID == 0), ]

indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)

#create knn model


rf_model <-  rpart(LONGITUDE ~. - BUILDINGID,
                   data = trainDataB0,
                   control = rpart.control(maxdepth = 30,cp=0.0001))


pred <- as.data.frame(predict(rf_model,newdata= testDataB0)) #predict test dataset
pred <- pred %>% mutate(prediccion = pred[,1]) 
distance<- testDataB0$LONGITUDE - pred$prediccion #Check if prediction is correct

my_distance <- sqrt(sum(distance*distance)/length(distance))
plot(distance)
my_distance



pred <- as.data.frame(predict(rf_model,newdata = WapValB0))
pred <- pred %>% mutate(prediccion = pred[,1]) 
distance <- WapValB0$LONGITUDE - pred$prediccion #Check if prediction is correct
distance_B0_rf <- distance
my_distance <- sqrt(sum(distance*distance)/length(distance))
plot(distance)
my_distance
#Error in LONGITUDE is 3.6 m
#Horrible


####Combinando errores de knn y rf #### Error Latitud: 4.10
mybesterror <- as.data.frame(distance_B0_knn)
mybesterror$distance_B0_rf <- distance_B0_rf
mybesterror <- mybesterror %>% mutate(best_distance = ifelse(abs(distance_B0_rf) > abs(distance_B0_knn)
                                                             , distance_B0_knn, distance_B0_rf)) 

my_distance <- sqrt(sum(mybesterror$best_distance*mybesterror$best_distance)/length(mybesterror$best_distance))
my_distance




#### random forest Building 1 ####
#Building 1
#Just in case
set.seed(123)
indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)
trainData <- WapSample[indexes,1:(ncol(WapSample))]
testData <- WapSample[-indexes,1:(ncol(WapSample))]

trainData$LONGITUDE <- Dataset[indexes,]$LONGITUDE
testData$LONGITUDE <- Dataset[-indexes, ]$LONGITUDE
WapVal$LONGITUDE <- vDataset$LONGITUDE - min(oDataset$LONGITUDE)

trainDataB0 <- trainData[which(trainData$BUILDINGID == 1), ]
testDataB0 <- testData[which(testData$BUILDINGID == 1), ]

WapValB0 <- WapVal[which(WapVal$BUILDINGID == 1), ]
vDatasetB0 <- vDataset[which(WapVal$BUILDINGID == 1), ]

indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)

#create knn model


rf_model <-  rpart(LONGITUDE ~. - BUILDINGID,
                   data = trainDataB0,
                   control = rpart.control(maxdepth = 30,cp=0.0001))


pred <- as.data.frame(predict(rf_model,newdata= testDataB0)) #predict test dataset
pred <- pred %>% mutate(prediccion = pred[,1]) 
distance<- testDataB0$LONGITUDE - pred$prediccion #Check if prediction is correct

my_distance <- sqrt(sum(distance*distance)/length(distance))
plot(distance)
my_distance



pred <- as.data.frame(predict(rf_model,newdata = WapValB0))
pred <- pred %>% mutate(prediccion = pred[,1]) 
distance <- WapValB0$LONGITUDE - pred$prediccion #Check if prediction is correct
distance_B1_rf <- distance
my_distance <- sqrt(sum(distance*distance)/length(distance))
plot(distance)
my_distance
#Error in LONGITUDE is 3.6 m
#Horrible


####Combinando errores de knn y rf #### Error Latitud: 4.10
mybesterror <- as.data.frame(distance_B1_knn)
mybesterror$distance_B1_rf <- distance_B1_rf
mybesterror <- mybesterror %>% mutate(best_distance = ifelse(abs(distance_B1_rf) > abs(distance_B1_knn)
                                                             , distance_B1_knn, distance_B1_rf)) 

my_distance <- sqrt(sum(mybesterror$best_distance*mybesterror$best_distance)/length(mybesterror$best_distance))
my_distance





#### random forest Building 2 ####
#Building 1
#Just in case
set.seed(123)
indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)
trainData <- WapSample[indexes,1:(ncol(WapSample))]
testData <- WapSample[-indexes,1:(ncol(WapSample))]

trainData$LONGITUDE <- Dataset[indexes,]$LONGITUDE
testData$LONGITUDE <- Dataset[-indexes, ]$LONGITUDE
WapVal$LONGITUDE <- vDataset$LONGITUDE - min(oDataset$LONGITUDE)

trainDataB0 <- trainData[which(trainData$BUILDINGID == 2), ]
testDataB0 <- testData[which(testData$BUILDINGID == 2), ]

WapValB0 <- WapVal[which(WapVal$BUILDINGID == 2), ]
vDatasetB0 <- vDataset[which(WapVal$BUILDINGID == 2), ]

indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)

#create knn model


rf_model <-  rpart(LONGITUDE ~. - BUILDINGID,
                   data = trainDataB0,
                   control = rpart.control(maxdepth = 30,cp=0.0001))


pred <- as.data.frame(predict(rf_model,newdata= testDataB0)) #predict test dataset
pred <- pred %>% mutate(prediccion = pred[,1]) 
distance<- testDataB0$LONGITUDE - pred$prediccion #Check if prediction is correct

my_distance <- sqrt(sum(distance*distance)/length(distance))
plot(distance)
my_distance



pred <- as.data.frame(predict(rf_model,newdata = WapValB0))
pred <- pred %>% mutate(prediccion = pred[,1]) 
distance <- WapValB0$LONGITUDE - pred$prediccion #Check if prediction is correct
distance_B2_rf <- distance
my_distance <- sqrt(sum(distance*distance)/length(distance))
plot(distance)
my_distance
#Error in LONGITUDE is 3.6 m
#Horrible


####Combinando errores de knn y rf #### Error Latitud: 4.10
mybesterror <- as.data.frame(distance_B2_knn)
mybesterror$distance_B2_rf <- distance_B2_rf
mybesterror <- mybesterror %>% mutate(best_distance = ifelse(abs(distance_B2_rf) > abs(distance_B2_knn)
                                                             , distance_B2_knn, distance_B2_rf)) 

my_distance <- sqrt(sum(mybesterror$best_distance*mybesterror$best_distance)/length(mybesterror$best_distance))
my_distance


plot(WapVal$FLOOR - vDataset$FLOOR)
length(which(abs(WapVal$FLOOR - vDataset$FLOOR) < 2))/length(WapVal$FLOOR)




}
