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


WD <- "/Users/sediaz/Documents/Ubiqum/Curso online Primavera 2017/R/Course3Task3"
setwd(WD)

source("Functions.R")

####Variables#####
Create = FALSE
cptree <- 0.000001
maxd <- 30


####Reading the tables####
Dataset <- read.csv("DatasetClean.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
WapSample <- read.csv("data/OnlyWapBuilding.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)
vDataset <- read.csv("validationData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
WapVal <- read.csv("data/validationDataBuilding.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)

Dataset$LATITUDE <- Dataset$LATITUDE - min(Dataset$LATITUDE)
Dataset$LONGITUDE <- Dataset$LONGITUDE - min(Dataset$LONGITUDE)

###Top1 is a factor
WapSample$Top1 <- NULL
WapVal$Top1 <- NULL
#WapSample$Top1 <- as.factor(WapSample$Top1)
#WapVal$Top1 <- as.factor(WapVal$Top1)

set.seed(123)
indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)
trainData <- WapSample[indexes,1:(ncol(WapSample))]
testData <- WapSample[-indexes,1:(ncol(WapSample))]

trainData$BUILDINGID <- Dataset[indexes,]$BUILDINGID

trainData$FLOOR <- Dataset[indexes,]$FLOOR

TrueTrain <- Dataset[indexes,]
TrueTest <- Dataset[-indexes,]
trainDataB1 <- trainData[which(trainData$BUILDINGID ==2),]
testDataB1 <- testData[which(testData$BUILDINGID ==2),]
WapValB1 <- WapVal[which(WapVal$BUILDINGID ==2),]

Fmodel <-  rpart( FLOOR~ .,
                  data = trainDataB1,
                  control = rpart.control(maxdepth = maxd,cp=cptree))


prediction <- as.data.frame(round(predict(Fmodel,newdata= testDataB1)))
prediction <- prediction %>% mutate(prediccion = round(prediction[,1]))
prediction <- prediction %>% mutate(resultado = TrueTest[which(testData$BUILDINGID==2),]$FLOOR)
prediction <- prediction %>% mutate(check = ifelse(resultado == prediccion, 1, 0)) #Check if prediction is correct
successTest <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
successTest

prediction <- as.data.frame(round(predict(Fmodel,newdata= trainDataB1)))
prediction <- prediction %>% mutate(buildingid = round(prediction[,1]))
prediction <- prediction %>% mutate(resultado = TrueTrain[which(TrueTrain$BUILDINGID==2),]$FLOOR)
prediction <- prediction %>% mutate(check = ifelse(resultado == buildingid, 1, 0)) #Check if prediction is correct
successTrain <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
successTrain

prediction <- as.data.frame(round(predict(Fmodel,newdata= WapValB1)))
prediction <- prediction %>% mutate(buildingid = round(prediction[,1]))
prediction <- prediction %>% mutate(resultado = vDataset[which(WapVal$BUILDINGID==2),]$FLOOR)
prediction <- prediction %>% mutate(check = ifelse(resultado == buildingid, 1, 0)) #Check if prediction is correct
successVal <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
successVal
