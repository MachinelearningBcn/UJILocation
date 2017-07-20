###############################
###                         ###
###   WIFI LOCATION         ###
###                         ###
###                         ###
###                         ###
###############################


#####Model1 is using all the waps plus NumberOfWaps plus Top1

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
WapSample <- read.csv("data/OnlyWap.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)
vDataset <- read.csv("validationData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
WapVal <- read.csv("data/validationWap.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)

#Dataset$LATITUDE <- Dataset$LATITUDE - min(Dataset$LATITUDE)
#Dataset$LONGITUDE <- Dataset$LONGITUDE - min(Dataset$LONGITUDE)

###Top1 is a factor
WapSample$Top1 <- as.factor(WapSample$Top1)
WapVal$Top1 <- as.factor(WapVal$Top1)

set.seed(123)
indexes <- createDataPartition(WapSample$NumberOfWaps, p = .80, list = FALSE)
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




# waps_to_kill <- c()
# for(i in 1:520){
#   if(length(which(trainData[,i]!=-110)) ==0){ 
#     waps_to_kill <- c(waps_to_kill,i)    
#   }
# }
# 
# trainData <- trainData[-waps_to_kill,]
# trainData<- trainData[,which(apply(trainData, 2, var)< 1)]
# 
# ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
# knnFit <- train(BUILDINGID ~ , data = trainData, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
# 
# 
# prediction <- as.data.frame(round(predict(knnFit,newdata= testData)))
# prediction <- prediction %>% mutate(prediccion = round(prediction[,1]))
# prediction <- prediction %>% mutate(resultado = Dataset[-indexes,]$BUILDINGID)
# prediction <- prediction %>% mutate(check = ifelse(resultado == prediccion, 1, 0)) #Check if prediction is correct
# success <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
# success
