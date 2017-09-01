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
Dataset <- read.csv("~/Documents/Ubiqum/Uji/DatasetClean.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
WapSample <- read.csv("~/Documents/Ubiqum/Uji/OnlyWapBuilding.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)
vDataset <- read.csv("~/Documents/Ubiqum/Uji/validationData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
WapVal <- read.csv("~/Documents/Ubiqum/Uji/validationDataBuilding.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)


BuildingID <- read.csv("~/Documents/Ubiqum/Uji/DatasetBuildingID.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
valBuildingID <- read.csv("~/Documents/Ubiqum/Uji/validationBuildingID.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)

lat0 <- min(Dataset$LATITUDE)
lon0 <- min(Dataset$LONGITUDE)
Dataset$LATITUDE <- Dataset$LATITUDE - min(Dataset$LATITUDE)
Dataset$LONGITUDE <- Dataset$LONGITUDE - min(Dataset$LONGITUDE)

vDataset$LATITUDE <- vDataset$LATITUDE - lat0
vDataset$LONGITUDE <- vDataset$LONGITUDE - lon0

###Top1 is a factor
WapSample$Top1 <- NULL
WapVal$Top1 <- NULL

set.seed(123)
indexes <- createDataPartition(WapSample$WAP001, p = .80, list = FALSE)
trainData <- WapSample[indexes,1:(ncol(WapSample))]
testData <- WapSample[-indexes,1:(ncol(WapSample))]

trainData$BUILDINGID <- Dataset[indexes,]$BUILDINGID
WapVal$BUILDINGID <- as.numeric(WapVal$BUILDINGID)

trainData$FLOOR <- Dataset[indexes,]$FLOOR

TrueTrain <- Dataset[indexes,]
TrueTest <- Dataset[-indexes,]

Fmodel <-  rpart( FLOOR~ .,
                  data = trainData,
                  control = rpart.control(maxdepth = maxd,cp=cptree))


prediction <- as.data.frame(round(predict(Fmodel,newdata= testData)))
prediction <- prediction %>% mutate(prediccion = round(prediction[,1]))
prediction <- prediction %>% mutate(resultado = TrueTest$FLOOR)
prediction <- prediction %>% mutate(check = ifelse(resultado == prediccion, 1, 0)) #Check if prediction is correct
successTest <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
successTest

prediction <- as.data.frame(round(predict(Fmodel,newdata= trainData)))
prediction <- prediction %>% mutate(floorid = round(prediction[,1]))
prediction <- prediction %>% mutate(resultado = TrueTrain$FLOOR)
prediction <- prediction %>% mutate(check = ifelse(resultado == floorid, 1, 0)) #Check if prediction is correct
successTrain <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
successTrain

prediction <- as.data.frame(round(predict(Fmodel,newdata= WapVal)))
prediction <- prediction %>% mutate(floorid = round(prediction[,1]))
prediction <- prediction %>% mutate(resultado = vDataset$FLOOR)
prediction <- prediction %>% mutate(check = ifelse(resultado == floorid, 1, 0)) #Check if prediction is correct
successVal <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
successVal

WapVal$FLOOR <- prediction$floorid


