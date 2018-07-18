###############################
###                         ###
###   WIFI LOCATION         ###
###                         ###
###                         ###
###                         ###
###############################


####Definition of Libraries####

library("data.table")
#library("tidyr")
library("dplyr")
library("taRifx")
library("rpart")
library("caret")
library("e1071")


####Creating my functions####
t0 <- proc.time()
t_model_total <- 0

#Rotates a pair of vectors and set the coordinates starting at (0,0)
rotate_matrix <- function(myangle, mydataframe){
  mydataframe$rot_x <- cos(myangle)*mydataframe[,1] - sin(myangle)*mydataframe[,2]
  mydataframe$rot_y <- sin(angle)*mydataframe[,1] + cos(myangle)*mydataframe[,2]
  mydataframe$rot_x <- mydataframe$rot_x - min(mydataframe$rot_x)
  mydataframe$rot_y <- mydataframe$rot_y - min(mydataframe$rot_y)
  return(mydataframe)
}


#Save NANs on normalizing by column
norm_vector <- function(myvector){
  vec_norm <- 0
  if(max(myvector) != min(myvector)){
    vec_norm <- (myvector - min(myvector))/(max(myvector) - min(myvector))
  }
  return(vec_norm)
}


#Normalizes a dataset by column
normalize_columns <- function(mydataframe){
  mydataframe <- lapply(mydataframe, function(x) norm_vector(x))
  return(as.data.frame(mydataframe))
}

#Normalizes a dataset by rows
normalize_rows <- function(mydataframe){
  mydataframe <- as.data.frame(t(apply(mydataframe, 1, function(x) (x - min(x))/(max(x)-min(x)))))
  return(as.data.frame(mydataframe))
}


#Selection columns containing waps information
preprocess_wifi <- function(mydataframe){
  mydataframe <- mydataframe[,1:520]
  mydataframe[mydataframe ==100] <- -110
  return(mydataframe)
}


find_wap <- function(myvector){
  wap_threshold <- 0.6 #Threshold 0.6 gets best result
  mywap <- which(myvector > wap_threshold & is.na(myvector) == FALSE)
  myb <- as.numeric(names(which.max(table(building_of_my_wap[mywap]))))
  return(myb)
}

#Predict building 
build_pred <- function(where_is_wap, mydataframe){
  predicted_build <- apply(mydataframe, 1, function (x) find_wap(x))
  return(as.numeric(predicted_build))
}



####Definition of Variables####

WD <- "/Users/sediaz/Documents/Ubiqum/Curso online Primavera 2017/R/Course3Task3"
setwd(WD)

#source("WifiFunctions.R")

run_tests <- FALSE
run_val  <- TRUE 
remove_odd_waps <- FALSE
norm_by_rows <- TRUE
verbose <- FALSE

####Reading the tables####
Dataset <- read.csv("~/Documents/Ubiqum/Uji/DatasetClean.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
vDataset <- read.csv("~/Documents/Ubiqum/Uji/validationData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
oDataset <- read.csv("~/Documents/Ubiqum/Uji/trainingData.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)

#Starting coordinates at 0
Dataset$LONGITUDE <- Dataset$LONGITUDE - min(Dataset$LONGITUDE)
Dataset$LATITUDE <- Dataset$LATITUDE - min(Dataset$LATITUDE)
vDataset$LONGITUDE <- vDataset$LONGITUDE - min(oDataset$LONGITUDE)
vDataset$LATITUDE <- vDataset$LATITUDE - min(oDataset$LATITUDE)

####Removing duplicated rows####
duplicated_rows <- which(duplicated(Dataset))  #3% of rows are copies of other rows
Dataset <- Dataset[-duplicated_rows,]

#Validation Dataset
duplicated_rows <- which(duplicated(vDataset))  #0% of rows are copies


####Preprocess####
DatasetWaps <- preprocess_wifi(Dataset)
extrange_rows <- which(apply(DatasetWaps, 1, max) > -30)
DatasetWaps <- DatasetWaps[-extrange_rows,] #Removing rows with data over -30 dB
Dataset <- Dataset[-extrange_rows,]

if(norm_by_rows){
  WapSample <- normalize_rows(DatasetWaps)
}else{
  WapSample <- normalize_columns(DatasetWaps)
}

if(run_val){
  vDatasetWaps <- preprocess_wifi(vDataset)
  extrange_rows <- which(apply(vDatasetWaps, 1, max) > -30)
  
  if(length(extrange_rows > 0)){
    vDatasetWaps <- vDatasetWaps[-extrange_rows,] #Removing rows with data over -30 dB
    vDataset <- vDataset[-extrange_rows,]
    
  }
  vWapSample <- normalize_rows(vDatasetWaps)
  
}
####Calculate my building via weighted waps####
#1 - Selecting where is any wap

building_of_my_wap <- c()

for(i in 1:520){
  
  if(length(which(WapSample[,i] > 0.2)) > 0){
    mymean <- as.numeric(names(which.max(table(Dataset$BUILDINGID[which(WapSample[,i] > 0)]))))
  }else{
    mymean <- NA
  }
  building_of_my_wap <- c(building_of_my_wap,mymean)
}


#2 - Predict Building
myBuilding <- c()

myBuilding <- build_pred(building_of_my_wap,WapSample)
successTrain <- (1 - length(which((as.numeric(myBuilding) - Dataset$BUILDINGID) != 0))/length(Dataset$BUILDINGID)) * 100
print(paste0("Building prediction accuracy is " , round(successTrain,2), "%"))
WapSample$BUILDINGID <- myBuilding


#99.8% for trainSet

if(run_val){
  
  myBuilding <- build_pred(building_of_my_wap,vWapSample)
  successTrainVal <- (1 - length(which((as.numeric(myBuilding) - vDataset$BUILDINGID) != 0))/length(vDataset$BUILDINGID)) * 100
  print(paste0("Building prediction accuracy is " , round(successTrainVal,2), "%"))
  vWapSample$BUILDINGID <- myBuilding
}



####Rotation of coordinates####
angle <- 0.50
Dataset_coor <- as.data.frame(Dataset$LONGITUDE)
Dataset_coor$LATITUDE <- Dataset$LATITUDE

Dataset_coor <- rotate_matrix(angle, Dataset_coor)
Dataset_coor$BUILDINGID <- Dataset$BUILDINGID
colnames(Dataset_coor)[1] <- "LONGITUDE"
WapSample$rot_x <- Dataset_coor$rot_x


plotting <- TRUE
if(plotting){
  
  ggplot() + geom_point(data = Dataset_coor, aes(x = rot_x, y = rot_y)) + xlim(0,400) + ylim(-150,250) +
    xlab("Longitude (m)") + ylab("Latitude (m)")
  ggplot() + geom_point(data = Dataset, aes(x = LONGITUDE, y = LATITUDE)) + xlim(0,400) + ylim(-50,350) +
    xlab("Longitude (m)") + ylab("Latitude (m)")
  
  ggplot() + geom_point(data = Dataset, aes(x = LONGITUDE, y = LATITUDE)) + geom_point(data = vDataset, aes(x = LONGITUDE, y = LATITUDE), color="red")
    + xlim(0,400) + ylim(-150,250) +
    xlab("Longitude (m)") + ylab("Latitude (m)")
  
}

if(run_val){
  vDataset_coor <- as.data.frame(vDataset$LONGITUDE)
  vDataset_coor$LATITUDE <- vDataset$LATITUDE
  
  vDataset_coor <- rotate_matrix(angle, vDataset_coor)
  vDataset_coor$BUILDINGID <- vDataset$BUILDINGID
  colnames(vDataset_coor)[1] <- "LONGITUDE"
  vWapSample$rot_x <- vDataset_coor$rot_x
  
  
}

####Creating train and test####
set.seed(123)
indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)
trainData <- WapSample[indexes,1:(ncol(WapSample))]
testData <- WapSample[-indexes,1:(ncol(WapSample))]

####Rot x Building 0####
threshold <- 0.35
building <- 0
neighbors <- 3

WapSample_b <- trainData[which(trainData$BUILDINGID == building),]
#first_position <- Position(function(x) x != 0, colMeans(WapSample_b))
testData_b <- testData[which(testData$BUILDINGID == building),]


testData_b$BUILDINGID <- NULL
WapSample_b$BUILDINGID <- NULL
WapSample_b[WapSample_b < threshold] <- 0

t_model_1 <- proc.time()

rf_model <-  rpart(rot_x ~.,
                   data = WapSample_b,
                   control = rpart.control(maxdepth = 30,cp=0.0001))

t_model_2 <- proc.time() - t_model_1
t_model_total <- t_model_total + t_model_2

if(run_tests){
  testData_b$BUILDINGID <- NULL
  testData_b[testData_b < threshold] <- 0
  result <- as.data.frame(predict(rf_model, newdata = testData_b))  
  rot_x_b0 <- result
}

if(run_val){
  vWapSample_b <- vWapSample[which(vWapSample$BUILDINGID == building),]
  vWapSample_b$BUILDINGID <- NULL
  vWapSample_b[vWapSample_b < threshold] <- 0
  result <- as.data.frame(predict(rf_model, newdata = vWapSample_b))  
  rot_x_b0_val <- result
  
}


####Rot x Building 1####
threshold <- 0.35
building <- 1
neighbors <- 3

WapSample_b <- trainData[which(trainData$BUILDINGID == building),]
first_position <- Position(function(x) x != 0, colMeans(WapSample_b))
testData_b <- testData[which(testData$BUILDINGID == building),]


testData_b$BUILDINGID <- NULL
WapSample_b$BUILDINGID <- NULL
WapSample_b[WapSample_b < threshold] <- 0

t_model_1 <- proc.time()
rf_model <-  rpart(rot_x ~.,
                   data = WapSample_b,
                   control = rpart.control(maxdepth = 30,cp=0.0001))
t_model_2 <- proc.time() - t_model_1
t_model_total <- t_model_total + t_model_2


if(run_tests){
  testData_b$BUILDINGID <- NULL
  testData_b[testData_b < threshold] <- 0
  result <- as.data.frame(predict(rf_model, newdata = testData_b))  
  rot_x_b1 <- result
}

if(run_val){
  vWapSample_b <- vWapSample[which(vWapSample$BUILDINGID == building),]
  vWapSample_b$BUILDINGID <- NULL
  vWapSample_b[vWapSample_b < threshold] <- 0
  result <- as.data.frame(predict(rf_model, newdata = vWapSample_b))  
  rot_x_b1_val <- result
  
}



####Rot x Building 2####
threshold <- 0.35
building <- 2
neighbors <- 3

WapSample_b <- trainData[which(trainData$BUILDINGID == building),]
first_position <- Position(function(x) x != 0, colMeans(WapSample_b))
testData_b <- testData[which(testData$BUILDINGID == building),]


testData_b$BUILDINGID <- NULL
WapSample_b$BUILDINGID <- NULL
WapSample_b[WapSample_b < threshold] <- 0

t_model_1 <- proc.time()

rf_model <-  rpart(rot_x ~.,
                   data = WapSample_b,
                   control = rpart.control(maxdepth = 30,cp=0.0001))

t_model_2 <- proc.time() - t_model_1
t_model_total <- t_model_total + t_model_2

if(run_tests){
  testData_b$BUILDINGID <- NULL
  testData_b[testData_b < threshold] <- 0
  result <- as.data.frame(predict(rf_model, newdata = testData_b))  
  rot_x_b2 <- result
}

if(run_val){
  vWapSample_b <- vWapSample[which(vWapSample$BUILDINGID == building),]
  vWapSample_b$BUILDINGID <- NULL
  vWapSample_b[vWapSample_b < threshold] <- 0
  result <- as.data.frame(predict(rf_model, newdata = vWapSample_b))  
  rot_x_b2_val <- result
  
}




####Putting Everything Together####

if(run_tests){
  
  #Creating vector of floor predictions
  rot_x_test <- c()
  rot_x_test[which(testData$BUILDINGID == 0)] <- rot_x_b0[,1]
  rot_x_test[which(testData$BUILDINGID == 1)] <- rot_x_b1[,1]
  rot_x_test[which(testData$BUILDINGID == 2)] <- rot_x_b2[,1]
  testData$rot_x <- rot_x_test
}



if(run_val){
  rot_x_val <- c()
  rot_x_val[which(vWapSample$BUILDINGID == 0)] <- rot_x_b0_val[,1]
  rot_x_val[which(vWapSample$BUILDINGID == 1)] <- rot_x_b1_val[,1]
  rot_x_val[which(vWapSample$BUILDINGID == 2)] <- rot_x_b2_val[,1]
  vWapSample$rot_x <- rot_x_val
}



####Building prediction using rot_x ####
trainBuilding <- as.data.frame(trainData$rot_x)
trainBuilding$BUILDINGID <- trainData$BUILDINGID
names(trainBuilding) <- c("rot_x","BUILDINGID")

testBuilding <- as.data.frame(testData$rot_x)
testBuilding$BUILDINGID <- testData$BUILDINGID
names(testBuilding) <- c("rot_x","BUILDINGID")

t_model_1 <- proc.time()
rf_model_b <- rpart(BUILDINGID ~ rot_x,
                   data = trainBuilding,
                   control = rpart.control(maxdepth = 30,cp=0.0001))

t_model_2 <- proc.time() - t_model_1
t_model_total <- t_model_total + t_model_2

if(run_tests){
  pred <- as.data.frame(predict(rf_model_b,newdata= testBuilding)) #predict test dataset
  pred <- pred %>% mutate(prediccion = round(pred[,1])) 
  Building_pred <- pred$prediccion
  
  testData$BUILDINGID <- pred$prediccion
  confusionMatrix(Dataset$BUILDINGID[-indexes], pred$prediccion)
  Building_accuracy <- round((1 - length(which(Dataset$BUILDINGID[-indexes] != pred$prediccion))/length(testData$BUILDINGID)) *100,2)
  Building_accuracy  
}

if(run_val){
  
  pred <- as.data.frame(predict(rf_model_b,newdata= vWapSample)) #predict test dataset
  pred <- pred %>% mutate(prediccion = round(pred[,1])) 
  Building_pred_val <- pred$prediccion
  
  vWapSample$BUILDINGID <- pred$prediccion
  confusionMatrix(vDataset$BUILDINGID, pred$prediccion)
  Building_accuracy_val <- round((1 - length(which(vDataset$BUILDINGID != pred$prediccion))/length(vDataset$BUILDINGID)) *100,2)
  Building_accuracy_val  
}


####Adding floor to the mix ####
trainData$FLOOR <- Dataset$FLOOR[indexes]
testData$FLOOR <- Dataset$FLOOR[-indexes]
trainData$rot_x <- NULL
testData$rot_x <- NULL

if(run_val){
  vWapSample$FLOOR <- vDataset$FLOOR
}
 
####Floor Building 0####
threshold <- 0.1
building <- 0
neighbors <- 3

WapSample_b <- trainData[which(trainData$BUILDINGID == building), ]
testBuilding <- testData[which(testData$BUILDINGID == building), ]

WapSample_b$BUILDINGID<- NULL
WapSample_b[WapSample_b < threshold] <- 0
knnmodel <- knn3(FLOOR ~. , WapSample_b, k = neighbors)
rf_model <- rpart(FLOOR ~.,
                  data = WapSample_b,
                  control = rpart.control(maxdepth = 30,cp=0.0001))
svm_model <- svm(FLOOR ~., data = WapSample_b)

WapSample_c5 <- WapSample_b
WapSample_c5$FLOOR <- NULL

c5_model <- C50::C5.0(x = WapSample_c5, y = factor(WapSample_b$FLOOR))


if(run_tests){
  testBuilding$BUILDINGID <- NULL
  testBuilding[testBuilding < threshold] <- 0
  
  result <- as.data.frame(round(predict(knnmodel,newdata = testBuilding)))
  result_rf0 <- as.data.frame(round(predict(rf_model, newdata = testBuilding)))
  result_svm0 <- as.data.frame(round(predict(svm_model, newdata = testBuilding)))
  result_c50 <- as.data.frame(predict(c5_model, newdata = testBuilding))
  
  
  myfloor<-c()
  for(i in 1:nrow(result)){
    if(length(which(result[i,] ==1))>0){
      myf <- which(result[i,] ==1) -1
    }else{
      myf <- NA
    }
    myfloor <- c(myfloor,myf )
  }
  myfloor_b0_test <- myfloor
  
  
  prediction <- as.data.frame(myfloor)
  prediction$floorid <- myfloor
  prediction <- prediction %>% mutate(resultado = testBuilding$FLOOR)
  prediction <- prediction %>% mutate(check = ifelse(resultado == floorid, 1, 0)) #Check if prediction is correct
  successTest <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
  successTest_b0 <- successTest
  successTest
  print(paste0("Building ",building, " Floor prediction accuracy is " , round(successTest,2), "%"))
  
}

if(run_val){
  vWapSample_b <- vWapSample[which(vWapSample$BUILDINGID == building), ]
  vWapSample_b[vWapSample_b < threshold] <- 0
  
  result <- as.data.frame(round(predict(knnmodel,newdata = vWapSample_b)))
  result_rf0_val <- as.data.frame(round(predict(rf_model, newdata = vWapSample_b)))
  result_svm0_val <- as.data.frame(round(predict(svm_model, newdata = vWapSample_b)))
  result_c50_val <- as.data.frame(predict(c5_model, newdata = vWapSample_b))
  
  myfloor<-c()
  for(i in 1:nrow(result)){
    if(length(which(result[i,] ==1))>0){
      myf <- which(result[i,] ==1) -1
    }else{
      myf <- NA
    }
    myfloor <- c(myfloor,myf )
  }
  myfloor_b0_val <- myfloor
  
  
  prediction <- as.data.frame(myfloor)
  prediction$floorid <- myfloor
  prediction <- prediction %>% mutate(resultado = vWapSample_b$FLOOR)
  prediction <- prediction %>% mutate(check = ifelse(resultado == floorid, 1, 0)) #Check if prediction is correct
  successVal <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
  successVal_b0 <- successVal
  successVal
  print(paste0("Building ", building, " Floor prediction accuracy is " , round(successVal,2), "%"))
}


####Floor Building 1####
threshold <- 0.8
building <- 1
neighbors <- 3

WapSample_b <- trainData[which(trainData$BUILDINGID == building), ]
testBuilding <- testData[which(testData$BUILDINGID == building), ]

WapSample_b$BUILDINGID<- NULL
WapSample_b[WapSample_b < threshold] <- 0
knnmodel <- knn3(FLOOR ~. , WapSample_b, k = neighbors)
rf_model <- rpart(FLOOR ~.,
                  data = WapSample_b,
                  control = rpart.control(maxdepth = 30,cp=0.0001))
svm_model <- svm(FLOOR ~., data = WapSample_b)

WapSample_c5 <- WapSample_b
WapSample_c5$FLOOR <- NULL

c5_model <- C50::C5.0(x = WapSample_c5, y = factor(WapSample_b$FLOOR))

if(run_tests){
  testBuilding$BUILDINGID <- NULL
  testBuilding[testBuilding < threshold] <- 0
  
  result <- as.data.frame(round(predict(knnmodel,newdata = testBuilding)))
  result_rf1 <- as.data.frame(round(predict(rf_model, newdata = testBuilding)))
  result_svm1 <- as.data.frame(round(predict(svm_model, newdata = testBuilding)))
  result_c51 <- as.data.frame(predict(c5_model, newdata = testBuilding))
  
  myfloor<-c()
  for(i in 1:nrow(result)){
    if(length(which(result[i,] ==1))>0){
      myf <- which(result[i,] ==1) -1
    }else{
      myf <- NA
    }
    myfloor <- c(myfloor,myf )
  }
  myfloor_b1_test <- myfloor
  
  
  prediction <- as.data.frame(myfloor)
  prediction$floorid <- myfloor
  prediction <- prediction %>% mutate(resultado = testBuilding$FLOOR)
  prediction <- prediction %>% mutate(check = ifelse(resultado == floorid, 1, 0)) #Check if prediction is correct
  successTest <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
  successTest_b1 <- successTest
  successTest
  print(paste0("Building ",building, " Floor prediction accuracy is " , round(successTest,2), "%"))
  
}

if(run_val){
  vWapSample_b <- vWapSample[which(vWapSample$BUILDINGID == building), ]
  vWapSample_b[vWapSample_b < threshold] <- 0
  
  result <- as.data.frame(round(predict(knnmodel,newdata = vWapSample_b)))
  result_rf1_val <- as.data.frame(round(predict(rf_model, newdata = vWapSample_b)))
  result_svm1_val <- as.data.frame(round(predict(svm_model, newdata = vWapSample_b)))
  result_c51_val <- as.data.frame(predict(c5_model, newdata = vWapSample_b))
  
  myfloor<-c()
  for(i in 1:nrow(result)){
    if(length(which(result[i,] ==1))>0){
      myf <- which(result[i,] ==1) -1
    }else{
      myf <- NA
    }
    myfloor <- c(myfloor,myf )
  }
  myfloor_b1_val <- myfloor
  
  
  prediction <- as.data.frame(myfloor)
  prediction$floorid <- myfloor
  prediction <- prediction %>% mutate(resultado = vWapSample_b$FLOOR)
  prediction <- prediction %>% mutate(check = ifelse(resultado == floorid, 1, 0)) #Check if prediction is correct
  successVal <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
  successVal_b1 <- successVal
  successVal
  print(paste0("Building ", building, " Floor prediction accuracy is " , round(successVal,2), "%"))
}


####Floor Building 2####
threshold <- 0.4
building <- 2
neighbors <- 3

WapSample_b <- trainData[which(trainData$BUILDINGID == building), ]
testBuilding <- testData[which(testData$BUILDINGID == building), ]

WapSample_b$BUILDINGID<- NULL
WapSample_b[WapSample_b < threshold] <- 0
knnmodel <- knn3(FLOOR ~. , WapSample_b, k = neighbors)
rf_model <- rpart(FLOOR ~.,
                  data = WapSample_b,
                  control = rpart.control(maxdepth = 30,cp=0.0001))
svm_model <- svm(FLOOR ~., data = WapSample_b)

WapSample_c5 <- WapSample_b
WapSample_c5$FLOOR <- NULL

c5_model <- C50::C5.0(x = WapSample_c5, y = factor(WapSample_b$FLOOR))

if(run_tests){
  testBuilding$BUILDINGID <- NULL
  testBuilding[testBuilding < threshold] <- 0
  
  result <- as.data.frame(round(predict(knnmodel,newdata = testBuilding)))
  result_rf2 <- as.data.frame(round(predict(rf_model, newdata = testBuilding)))
  result_svm2 <- as.data.frame(round(predict(svm_model, newdata = testBuilding)))
  result_c52 <- as.data.frame(predict(c5_model, newdata = testBuilding))
  
  
  myfloor<-c()
  for(i in 1:nrow(result)){
    if(length(which(result[i,] ==1))>0){
      myf <- which(result[i,] ==1) -1
    }else{
      myf <- NA
    }
    myfloor <- c(myfloor,myf )
  }
  myfloor_b2_test <- myfloor
  
  
  prediction <- as.data.frame(myfloor)
  prediction$floorid <- myfloor
  prediction <- prediction %>% mutate(resultado = testBuilding$FLOOR)
  prediction <- prediction %>% mutate(check = ifelse(resultado == floorid, 1, 0)) #Check if prediction is correct
  successTest <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
  successTest
  successTest_b2 <- successTest
  print(paste0("Building ",building, " Floor prediction accuracy is " , round(successTest,2), "%"))
  
}

if(run_val){
  vWapSample_b <- vWapSample[which(vWapSample$BUILDINGID == building), ]
  vWapSample_b[vWapSample_b < threshold] <- 0
  
  result <- as.data.frame(round(predict(knnmodel,newdata = vWapSample_b)))
  result_rf2_val <- as.data.frame(round(predict(rf_model, newdata = vWapSample_b)))
  result_svm2_val <- as.data.frame(round(predict(svm_model, newdata = vWapSample_b)))
  result_c52_val <- as.data.frame(predict(c5_model, newdata = vWapSample_b))
  
  
  myfloor<-c()
  for(i in 1:nrow(result)){
    if(length(which(result[i,] ==1))>0){
      myf <- which(result[i,] ==1) -1
    }else{
      myf <- NA
    }
    myfloor <- c(myfloor,myf )
  }
  myfloor_b2_val <- myfloor
  
  
  prediction <- as.data.frame(myfloor)
  prediction$floorid <- myfloor
  prediction <- prediction %>% mutate(resultado = vWapSample_b$FLOOR)
  prediction <- prediction %>% mutate(check = ifelse(resultado == floorid, 1, 0)) #Check if prediction is correct
  successVal <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
  successVal_b2 <- successVal
  successVal
  print(paste0("Building ", building, " Floor prediction accuracy is " , round(successVal,2), "%"))
}




####Putting Everything Together####

if(run_tests){
  
  
  #Creating vector of floor predictions
  floor_test <- c()
  floor_test[which(testData$BUILDINGID == 0)]<- myfloor_b0_test
  floor_test[which(testData$BUILDINGID == 1)]<- myfloor_b1_test
  floor_test[which(testData$BUILDINGID == 2)]<- myfloor_b2_test
  Floor_accuracy <- round((1 - length(which(floor_test != testData$FLOOR))/length(floor_test))*100 ,2)
  
  wrong_results<- c(which(Building_pred != Dataset$BUILDINGID[-indexes]),which(floor_test != testData$FLOOR))
  Total_accuracy_test <- round((1 - length(unique(wrong_results))/length(floor_test))*100,2)
  
  floor_test_rf <- c()
  floor_test_rf[which(testData$BUILDINGID == 0)]<- as.vector(result_rf0[,1])
  floor_test_rf[which(testData$BUILDINGID == 1)]<- as.vector(result_rf1[,1])
  floor_test_rf[which(testData$BUILDINGID == 2)]<- as.vector(result_rf2[,1])
  Floor_accuracy_rf <- round((1 - length(which(floor_test_rf != testData$FLOOR))/length(floor_test_rf))*100 ,2)
  
  floor_test_svm <- c()
  floor_test_svm[which(testData$BUILDINGID == 0)]<- as.vector(result_svm0[,1])
  floor_test_svm[which(testData$BUILDINGID == 1)]<- as.vector(result_svm1[,1])
  floor_test_svm[which(testData$BUILDINGID == 2)]<- as.vector(result_svm2[,1])
  Floor_accuracy_svm <- round((1 - length(which(floor_test_svm != testData$FLOOR))/length(floor_test_svm))*100 ,2)
  
  floor_test_c5 <- c()
  floor_test_c5[which(testData$BUILDINGID == 0)]<- as.vector(result_c50[,1])
  floor_test_c5[which(testData$BUILDINGID == 1)]<- as.vector(result_c51[,1])
  floor_test_c5[which(testData$BUILDINGID == 2)]<- as.vector(result_c52[,1])
  Floor_accuracy_c5 <- round((1 - length(which(floor_test_c5 != testData$FLOOR))/length(floor_test_c5))*100 ,2)
}

if(run_val){
  
  #Creating vector of floor predictions
  floor_val <- c()
  floor_val[which(vWapSample$BUILDINGID == 0)] <- myfloor_b0_val
  floor_val[which(vWapSample$BUILDINGID == 1)] <- myfloor_b1_val
  floor_val[which(vWapSample$BUILDINGID == 2)] <- myfloor_b2_val
  Floor_accuracy_val <- round((1 - length(which(floor_val != vWapSample$FLOOR))/length(floor_val))*100 ,2)
  
  wrong_results<- c(which(Building_pred_val != vDataset$BUILDINGID),which(floor_val != vDataset$FLOOR))
  Total_accuracy_val <- round((1 - length(unique(wrong_results))/length(floor_val))*100,2)
  
  floor_val_rf <- c()
  floor_val_rf[which(vWapSample$BUILDINGID == 0)]<- as.vector(result_rf0_val[,1])
  floor_val_rf[which(vWapSample$BUILDINGID == 1)]<- as.vector(result_rf1_val[,1])
  floor_val_rf[which(vWapSample$BUILDINGID == 2)]<- as.vector(result_rf2_val[,1])
  Floor_accuracy_rf_val <- round((1 - length(which(floor_val_rf != vWapSample$FLOOR))/length(floor_val_rf))*100 ,2)
  
  floor_val_svm <- c()
  floor_val_svm[which(vWapSample$BUILDINGID == 0)]<- as.vector(result_svm0_val[,1])
  floor_val_svm[which(vWapSample$BUILDINGID == 1)]<- as.vector(result_svm1_val[,1])
  floor_val_svm[which(vWapSample$BUILDINGID == 2)]<- as.vector(result_svm2_val[,1])
  Floor_accuracy_svm_val <- round((1 - length(which(floor_val_svm != vWapSample$FLOOR))/length(floor_val_svm))*100 ,2)
  
  floor_val_c5 <- c()
  floor_val_c5[which(vWapSample$BUILDINGID == 0)]<- as.vector(result_c50_val[,1])
  floor_val_c5[which(vWapSample$BUILDINGID == 1)]<- as.vector(result_c51_val[,1])
  floor_val_c5[which(vWapSample$BUILDINGID == 2)]<- as.vector(result_c52_val[,1])
  Floor_accuracy_c5_val <- round((1 - length(which(floor_val_c5 != vWapSample$FLOOR))/length(floor_val_c5))*100 ,2)
}


####Printing results####
if(run_tests){
  print(paste0("Test Building prediction accuracy is " , Building_accuracy, "%"))
  print(paste0("Test Building ",0, " Floor prediction accuracy is " , round(successTest_b0,2), "%"))
  print(paste0("Test Building ",1, " Floor prediction accuracy is " , round(successTest_b1,2), "%"))
  print(paste0("Test Building ",2, " Floor prediction accuracy is " , round(successTest_b2,2), "%"))
  print(paste0("Knn Test Floor prediction accuracy is " , Floor_accuracy, "%"))
  print(paste0("Random Forest Test Floor prediction accuracy is " , Floor_accuracy_rf, "%"))
  print(paste0("SVM Test Floor prediction accuracy is " , Floor_accuracy_svm, "%"))
  print(paste0("C5.0 Test Floor prediction accuracy is " , Floor_accuracy_c5, "%"))
  print(paste0("Test Building and Floor combined accuracy is " , Total_accuracy_test, "%"))
}

if(run_val){
  if(verbose){
    print(paste0("Validation Building prediction accuracy is " , Building_accuracy_val, "%"))
    print(paste0("Validation Building ",0, " Floor prediction accuracy is " , round(successVal_b0,2), "%"))
    print(paste0("Validation Building ",1, " Floor prediction accuracy is " , round(successVal_b1,2), "%"))
    print(paste0("Validation Building ",2, " Floor prediction accuracy is " , round(successVal_b2,2), "%"))
    print(paste0("Knn Validation Floor prediction accuracy is " , Floor_accuracy_val, "%"))
    print(paste0("Random Forest Validation Floor prediction accuracy is " , Floor_accuracy_rf_val, "%"))
    print(paste0("SVM Validation Floor prediction accuracy is " , Floor_accuracy_svm_val, "%"))
    print(paste0("C5.0 Validation Floor prediction accuracy is " , Floor_accuracy_c5_val, "%"))
    print(paste0("Validation Building and Floor combined accuracy is " , Total_accuracy_val, "%"))
  }
}

t1 <- proc.time() - t0
print(paste0("Time of execution is " , round(t1[3],2)))
print(paste0("Time of creating models is " , round(t_model_total[3],2)))

####Plots####
if(run_tests && run_val){
  df_accuracy <- as.data.frame(c(Floor_accuracy, Floor_accuracy_rf, Floor_accuracy_svm, Floor_accuracy_c5,
                                 Floor_accuracy_val, Floor_accuracy_rf_val, Floor_accuracy_svm_val, Floor_accuracy_c5_val))
  df_accuracy$model <- c("Knn", "Random Forest", "SVM", "C5.0", "Knn", "Random Forest", "SVM", "C5.0")
  df_accuracy$dataset <- c("Test", "Test", "Test", "Test", "Val", "Val", "Val", "Val")
  colnames(df_accuracy) <- c("Accuracy", "Model", "DataSet")
  
 
  
  ggplot(df_accuracy, aes(x = Model, y = Accuracy, fill = DataSet)) + geom_col(position = "dodge") + 
    xlab("Model") + ylab("Accuracy %") +
    ggtitle("Floor prediction accuracy ")+
    theme(plot.title = element_text(hjust = 0.5)) + coord_flip(ylim=c(80,100))
}
