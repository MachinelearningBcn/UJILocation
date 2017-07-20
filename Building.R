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
  
  
  ####Reading the tables####
  Dataset <- read.csv("trainingData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
  vDataset <- read.csv("validationData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
  
  Dataset$LATITUDE <- Dataset$LATITUDE - min(Dataset$LATITUDE)
  Dataset$LONGITUDE <- Dataset$LONGITUDE - min(Dataset$LONGITUDE)
  
  set.seed(123)
  indexes <- CreateDataPartition(Dataset$WAP006, p = .80, list = FALSE)
  
  trainData <- Dataset[-indexes,1:(ncol(Dataset)-9)]
  testData <- Dataset[indexes,1:(ncol(Dataset)-9)]
  validationData <- vDataset[,1:(ncol(Dataset)-9)]
  
  trainData$LATITUDE <- Dataset[-indexes,]$LATITUDE
  
  LatModel <-  rpart(LATITUDE ~. ,
                          data = trainData,
                          control = rpart.control(maxdepth = 24,cp=0.00001))
  
  
  prediction <- as.data.frame(predict(LatModel,newdata= testData))
  prediction <- prediction %>% mutate(latitude = prediction[,1])
  prediction <- prediction %>% mutate(resultado = Dataset[indexes,]$LATITUDE)
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
  validationData$LATITUDE <- prediction$latitude
  
  
  trainData$LONGITUDE <- Dataset[-indexes,]$LONGITUDE
  
  
  LonModel <-  rpart(LONGITUDE ~. ,
                     data = trainData,
                     control = rpart.control(maxdepth = 24,cp=0.00001))
  
  
  prediction <- as.data.frame(predict(LonModel,newdata= testData))
  prediction <- prediction %>% mutate(longitude = prediction[,1])
  prediction <- prediction %>% mutate(resultado = Dataset[indexes,]$LONGITUDE)
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
  
  validationData$LONGITUDE <- prediction$longitude
  
  
  
  ####Predicting the Building####
  trainData$BUILDINGID <- Dataset[-indexes,]$BUILDINGID
  testData$BUILDINGID <- Dataset[indexes,]$BUILDINGID
  validationData$BUILDINGID <- vDataset$BUILDINGID
  
  Bmodel <-  rpart( BUILDINGID~. ,
                    data = trainData,
                    control = rpart.control(maxdepth = 24,cp=0.00001))
  
  prediction <- as.data.frame(round(predict(Bmodel,newdata= testData)))
  prediction <- prediction %>% mutate(prediccion = round(prediction[,1]))
  prediction <- prediction %>% mutate(resultado = testData$BUILDINGID)
  prediction <- prediction %>% mutate(check = ifelse(resultado == prediccion, 1, 0)) #Check if prediction is correct
  success <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
  success
  
  
  #Assigning building IDs
  #testData$BUILDINGID <- prediction$prediccion
  
  prediction <- as.data.frame(round(predict(Bmodel,newdata= trainData)))
  prediction <- prediction %>% mutate(buildingid = round(prediction[,1]))
  prediction <- prediction %>% mutate(resultado = Dataset[-indexes,]$BUILDINGID)
  prediction <- prediction %>% mutate(check = ifelse(resultado == buildingid, 1, 0)) #Check if prediction is correct
  successTrain <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
  successTrain
  
  trainData$BUILDINGID <- prediction$buildingid
  
  
  prediction <- as.data.frame(round(predict(Bmodel,newdata= validationData)))
  prediction <- prediction %>% mutate(buildingid = round(prediction[,1]))
  #validationData$BUILDINGID <- prediction$buildingid
  
  
  
  ###Second iteration of Lat and Lon
  
  repeat{  
    GlobalSuccess = successTrain
    print(GlobalSuccess)
    LatModel <-  rpart(LATITUDE ~. ,
                     data = trainData,
                     control = rpart.control(maxdepth = 24,cp=0.00001))
  
  
    prediction <- as.data.frame(predict(LatModel,newdata= testData))
    prediction <- prediction %>% mutate(latitude = prediction[,1])
    testData$LATITUDE <- prediction$latitude
  
    prediction <- as.data.frame(predict(LatModel,newdata= trainData))
    prediction <- prediction %>% mutate(latitude = prediction[,1])
  
    trainData$LATITUDE <- prediction$latitude
    
    prediction <- as.data.frame(predict(LatModel,newdata= validationData))
    prediction <- prediction %>% mutate(latitude = prediction[,1])
    validationData$LATITUDE <- prediction$latitude
  
    LonModel <-  rpart(LONGITUDE ~. ,
                     data = trainData,
                     control = rpart.control(maxdepth = 24,cp=0.00001))
  
  
    prediction <- as.data.frame(predict(LonModel,newdata= testData))
    prediction <- prediction %>% mutate(longitude = prediction[,1])
    testData$LONGITUDE <- prediction$longitude
    
    prediction <- as.data.frame(predict(LonModel,newdata= trainData))
    prediction <- prediction %>% mutate(longitude = prediction[,1])
    trainData$LONGITUDE <- prediction$longitude
    
    prediction <- as.data.frame(predict(LonModel,newdata= validationData))
    prediction <- prediction %>% mutate(longitude = prediction[,1])
    validationData$LONGITUDE <- prediction$longitude
    
    Bmodel <-  rpart( BUILDINGID~. ,
                    data = trainData,
                    control = rpart.control(maxdepth = 24,cp=0.00001))
  
    prediction <- as.data.frame(round(predict(Bmodel,newdata= testData)))
    prediction <- prediction %>% mutate(prediccion = round(prediction[,1]))
    prediction <- prediction %>% mutate(resultado = testData$BUILDINGID)
    prediction <- prediction %>% mutate(check = ifelse(resultado == prediccion, 1, 0)) #Check if prediction is correct
    successTest <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
    successTest
  
 #   testData$BUILDINGID <- prediction$prediccion
    
    prediction <- as.data.frame(round(predict(Bmodel,newdata= trainData)))
    prediction <- prediction %>% mutate(prediccion = round(prediction[,1]))
    prediction <- prediction %>% mutate(resultado = Dataset[-indexes,]$BUILDINGID)
    prediction <- prediction %>% mutate(check = ifelse(resultado == prediccion, 1, 0)) #Check if prediction is correct
    successTrain <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
    successTrain
    
    trainData$BUILDINGID <- prediction$prediccion
    
    
    prediction <- as.data.frame(round(predict(Bmodel,newdata= validationData)))
    prediction <- prediction %>% mutate(prediccion = round(prediction[,1]))
    prediction <- prediction %>% mutate(resultado = validationData$BUILDINGID)
    prediction <- prediction %>% mutate(check = ifelse(resultado == prediccion, 1, 0)) #Check if prediction is correct
    successVal <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
    successVal
  
    if(GlobalSuccess >= successTrain){
      break
    }else if(successTrain == 100){
      break
    } 
  }
  
  
###100% success in Validation
    
  
###The floor is lava
  
  trainData$FLOOR <- Dataset[-indexes,]$FLOOR
  testData$FLOOR <- Dataset[indexes,]$FLOOR
  validationData$FLOOR <- vDataset$FLOOR
  
  
  Fmodel <-  rpart( FLOOR~. ,
                    data = trainData,
                    control = rpart.control(maxdepth = 24,cp=0.00001))
  
  prediction <- as.data.frame(round(predict(Fmodel,newdata= trainData)))
  prediction <- prediction %>% mutate(floor = round(prediction[,1]))
  prediction <- prediction %>% mutate(resultado = trainData$FLOOR)
  prediction <- prediction %>% mutate(check = ifelse(floor == resultado, 1, 0)) #Check if prediction is correct
  successTrain <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
  successTrain
  
 #trainData$FLOOR <- prediction$floor
  
  prediction <- as.data.frame(round(predict(Fmodel,newdata= testData)))
  prediction <- prediction %>% mutate(floor = round(prediction[,1]))
 #testData$FLOOR <- prediction$floor
  
  prediction <- as.data.frame(round(predict(Fmodel,newdata= validationData)))
  prediction <- prediction %>% mutate(floor = round(prediction[,1]))
  #validationData$FLOOR <- prediction$floor
  
  repeat{  
  
    
    GlobalSuccess = successTrain
    print(GlobalSuccess)
    LatModel <-  rpart(LATITUDE ~. ,
                       data = trainData,
                       control = rpart.control(maxdepth = 24,cp=0.00001))
    
    
    prediction <- as.data.frame(predict(LatModel,newdata= testData))
    prediction <- prediction %>% mutate(latitude = prediction[,1])
    testData$LATITUDE <- prediction$latitude
    
    prediction <- as.data.frame(predict(LatModel,newdata= trainData))
    prediction <- prediction %>% mutate(latitude = prediction[,1])
    prediction <- prediction %>% mutate(resultado = Dataset[-indexes,]$LATITUDE)
    prediction <- prediction %>% mutate(distance = abs(resultado - latitude)) #Calculate distance in coordinates
    
    
    
    trainData$LATITUDE <- prediction$latitude
    ErrorLat <- sum(prediction$distance)/nrow(prediction) #Average error
    
    prediction <- as.data.frame(predict(LatModel,newdata= validationData))
    prediction <- prediction %>% mutate(latitude = prediction[,1])
    validationData$LATITUDE <- prediction$latitude
    
    LonModel <-  rpart(LONGITUDE ~. ,
                       data = trainData,
                       control = rpart.control(maxdepth = 24,cp=0.00001))
    
    
    prediction <- as.data.frame(predict(LonModel,newdata= testData))
    prediction <- prediction %>% mutate(longitude = prediction[,1])
    testData$LONGITUDE <- prediction$longitude
    
    prediction <- as.data.frame(predict(LonModel,newdata= trainData))
    prediction <- prediction %>% mutate(longitude = prediction[,1])
    trainData$LONGITUDE <- prediction$longitude
    prediction <- prediction %>% mutate(resultado = Dataset[-indexes,]$LONGITUDE)
    prediction <- prediction %>% mutate(distance = abs(resultado - longitude)) #Calculate distance in coordinates
    ErrorLon <- sum(prediction$distance)/nrow(prediction) #Average error
    
    
    prediction <- as.data.frame(predict(LonModel,newdata= validationData))
    prediction <- prediction %>% mutate(longitude = prediction[,1])
    validationData$LONGITUDE <- prediction$longitude
    
    Fmodel <-  rpart( FLOOR~. ,
                      data = trainData,
                      control = rpart.control(maxdepth = 24,cp=0.00001))
    
    prediction <- as.data.frame(round(predict(Fmodel,newdata= testData)))
    prediction <- prediction %>% mutate(prediccion = round(prediction[,1]))
    prediction <- prediction %>% mutate(resultado = testData$FLOOR)
    prediction <- prediction %>% mutate(check = ifelse(resultado == prediccion, 1, 0)) #Check if prediction is correct
    successTest <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
    successTest
    
    testData$FLOOR <- prediction$prediccion
    
    prediction <- as.data.frame(round(predict(Fmodel,newdata= trainData)))
    prediction <- prediction %>% mutate(prediccion = round(prediction[,1]))
    prediction <- prediction %>% mutate(resultado = trainData$FLOOR)
    prediction <- prediction %>% mutate(check = ifelse(resultado == prediccion, 1, 0)) #Check if prediction is correct
    successTrain <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
    successTrain
    
    trainData$FLOOR <- prediction$prediccion
    
    
    prediction <- as.data.frame(round(predict(Fmodel,newdata= validationData)))
    prediction <- prediction %>% mutate(prediccion = round(prediction[,1]))
    prediction <- prediction %>% mutate(resultado = validationData$FLOOR)
    prediction <- prediction %>% mutate(check = ifelse(resultado == prediccion, 1, 0)) #Check if prediction is correct
    successVal <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
    successVal
    
    validationData$FLOOR <- prediction$prediccion
    
    
    if(GlobalSuccess >= successTrain){
      break
    } 
  }
  
  ###100% in Floor prediction
  
  ErrorDistance <- sqrt(ErrorLon^2 + ErrorLat^2)
  ErrorDistance
  
  trainData$LATITUDE <- Dataset[-indexes,]$LATITUDE
  trainData$LONGITUDE <- Dataset[-indexes,]$LONGITUDE
    
  ###As we know 100% sure the building we are gonna separate by building
  trainDataTrue <- Dataset[-indexes,]
  trainDataTrue1 <- trainDataTrue[which(trainDataTrue$BUILDINGID ==1),]
  trainData1 <- trainData[which(trainData$BUILDINGID == 1),]
  testData1 <- testData[which(testData$BUILDINGID == 1),]
  validationData1 <- validationData[which(validationData$BUILDINGID == 1),]
  
  #####Measuring the number of waps with signal
  waps_to_kill <- c()
   for(i in 1:520){
     if(length(which(trainData1[,i]!=100)) ==0){ 
       waps_to_kill <- c(waps_to_kill,i)    
     }
   }
   
  trainData1 <- trainData1[,-waps_to_kill]
  testData1 <- testData1[,-waps_to_kill]
  validationData1 <- validationData1[,-waps_to_kill]
  
  ####First Lat and Lon models
  
  trainData1$LATITUDE <- trainDataTrue1$LATITUDE
  
  LatModel <-  rpart(LATITUDE ~. ,
                     data = trainData1,
                     control = rpart.control(maxdepth = 24,cp=0.00001))
  
  
  prediction <- as.data.frame(predict(LatModel,newdata= testData1))
  prediction <- prediction %>% mutate(latitude = prediction[,1])
  prediction <- prediction %>% mutate(resultado = Dataset0[indexes,]$LATITUDE)
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
  validationData$LATITUDE <- prediction$latitude
  
  
  
  
  trainData$LONGITUDE <- Dataset[-indexes,]$LONGITUDE
  
  
  LonModel <-  rpart(LONGITUDE ~. ,
                     data = trainData,
                     control = rpart.control(maxdepth = 24,cp=0.00001))
  
  
  prediction <- as.data.frame(predict(LonModel,newdata= testData))
  prediction <- prediction %>% mutate(longitude = prediction[,1])
  prediction <- prediction %>% mutate(resultado = Dataset[indexes,]$LONGITUDE)
  prediction <- prediction %>% mutate(distance = abs(resultado - longitude)) #Calculate distance in coordinates
  ErrorLon <- sum(prediction$distance)/nrow(prediction) #Average error
  ErrorLon
  
  ErrorDistance <- sqrt(ErrorLon^2 + ErrorLat^2)
  ErrorDistance
  

  
  repeat{  
    
    GlobalSuccess = ErrorDistance
    print(GlobalSuccess)
    LatModel <-  rpart(LATITUDE ~. ,
                       data = trainData1,
                       control = rpart.control(maxdepth = 24,cp=0.00001))
    
    
    prediction <- as.data.frame(predict(LatModel,newdata= testData1))
    prediction <- prediction %>% mutate(latitude = prediction[,1])
    testData1$LATITUDE <- prediction$latitude
    
    prediction <- as.data.frame(predict(LatModel,newdata= trainData1))
    prediction <- prediction %>% mutate(latitude = prediction[,1])
    prediction <- prediction %>% mutate(resultado = trainDataTrue[which(trainData$BUILDING==1),]$LATITUDE)
    prediction <- prediction %>% mutate(distance = abs(resultado - latitude)) #Calculate distance in coordinates
    trainData1$LATITUDE <- prediction$latitude
    
    ErrorLat <- sum(prediction$distance)/nrow(prediction) #Average error
    
    prediction <- as.data.frame(predict(LatModel,newdata= validationData1))
    prediction <- prediction %>% mutate(latitude = prediction[,1])
    validationData1$LATITUDE <- prediction$latitude
    
    LonModel <-  rpart(LONGITUDE ~. ,
                       data = trainData1,
                       control = rpart.control(maxdepth = 24,cp=0.00001))
    
    
    prediction <- as.data.frame(predict(LonModel,newdata= testData1))
    prediction <- prediction %>% mutate(longitude = prediction[,1])
    testData1$LONGITUDE <- prediction$longitude
    
    prediction <- as.data.frame(predict(LonModel,newdata= trainData1))
    prediction <- prediction %>% mutate(longitude = prediction[,1])
    trainData1$LONGITUDE <- prediction$longitude
    prediction <- prediction %>% mutate(resultado = trainDataTrue[which(trainData$BUILDING==1),]$LONGITUDE)
    prediction <- prediction %>% mutate(distance = abs(resultado - longitude)) #Calculate distance in coordinates
    ErrorLon <- sum(prediction$distance)/nrow(prediction) #Average error
    
    
    prediction <- as.data.frame(predict(LonModel,newdata= validationData1))
    prediction <- prediction %>% mutate(longitude = prediction[,1])
    validationData1$LONGITUDE <- prediction$longitude
  
    ErrorDistance <- sqrt(ErrorLon^2 + ErrorLat^2)
    
    if(GlobalSuccess <= ErrorDistance){
      break
    } 
  }
  
  prediction <- as.data.frame(round(predict(Fmodel,newdata= validationData)))
  prediction <- prediction %>% mutate(prediccion = round(prediction[,1]))
  prediction <- prediction %>% mutate(resultado = validationData$FLOOR)
  prediction <- prediction %>% mutate(check = ifelse(resultado == prediccion, 1, 0)) #Check if prediction is correct
  success <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
  success
  
  
  
  
  
  
  
  
  waps_to_kill <- read.csv("B1waps.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
  
  
  vector1<- c()
  vector2<- c()
  vector3<- c()
  
  #Check important WAPS for each building
  
  for(j in 0:2){
    Building <- Dataset[which(Dataset$BUILDINGID==j),]
      counter <- 0
      vector <- c()
      for(i in 1:520){  
        if(length(which(Building[,i]!=100))!=0){
          counter = counter +1
          vector <- c(vector,i)
         }
        
      }
      if(j==0){
        vector1 <- vector
      }else if(j==1){
        vector2 <- vector
      }else{
        vector3 <- vector
      }
       print(counter)
  }
  matchvector1 <- c()
  for (i in vector1){
    matchvector1<- c(matchvector1,match(i,vector2))
  }  
  
  matchvector2 <- c()
  for(i in vector1){
    matchvector2<- c(matchvector2,match(i,vector3))
  }  
  
  #Dataset <- Dataset[,-waps_to_kill$x]
  #vDataset <- vDataset[,-waps_to_kill$x]
  
 
  
  NumberOfWaps <- c()
  
  OnlyWap <- trainData[,1:(ncol(trainData))]
  if(Create){
      for(i in 1:nrow(OnlyWap)){
          print(i)
          now <- length(which(OnlyWap[i,] != 100))
          NumberOfWaps <- c(NumberOfWaps,now)    
      }
  }
  noSignal <- which(NumberOfWaps <1)
  
  NumberOfWapsTest <- c()
  OnlyWap <- testData[,1:(ncol(testData))]
  for(i in 1:nrow(OnlyWap)){
    print(i)
    now <- length(which(OnlyWap[i,] != 100))
    NumberOfWapsTest <- c(NumberOfWapsTest,now)    
  }
  
  trainData <- trainData[-which(NumberOfWaps <1),]
  write.csv(trainData,file="trainclean.csv",row.names=FALSE)
  
  if(Create){
    for(j in 1:ncol(trainData)){
      print(j)
      for(i in 1:length(trainData$WAP006)){
        signal  =  trainData[i,j] 
        
        if(signal==100){
          signal = -100000
        }
        trainData[i,j] = signal
      }
    }  
    
    
  
    
    for(j in 1:ncol(testData)){
      for(i in 1:length(testData$WAP006)){
        
        signal  =  testData[i,j] 
        
        if(signal==100){
          signal = -100000
        }
        testData[i,j] = signal
      }
    }  
  }
  
  
  