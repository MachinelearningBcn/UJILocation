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
Dataset <- read.csv("DatasetZone.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
WapSample <- read.csv("data/OnlyWapNormCut.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)
vDataset <- read.csv("validationData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
WapVal <- read.csv("data/ValidationWapNorm.csv", sep=",",header=TRUE,stringsAsFactors = FALSE)

Dataset$LATITUDE <- Dataset$LATITUDE - min(Dataset$LATITUDE)
Dataset$LONGITUDE <- Dataset$LONGITUDE - min(Dataset$LONGITUDE)
WapSample$Top1 <- NULL
WapSample$NumberOfWaps <- NULL



set.seed(123)
indexes <- createDataPartition(WapSample$WAP517, p = .80, list = FALSE)
trainData <- WapSample[indexes,1:(ncol(WapSample))]
testData <- WapSample[-indexes,1:(ncol(WapSample))]

Dataset$ZONE <- factor(Dataset$ZONE)
trainData$ZONE <- Dataset[indexes,]$ZONE
zone <- levels(Dataset$ZONE)


Zmodel <-  rpart( ZONE~ .,
                  data = trainData,
                  control = rpart.control(maxdepth = maxd,cp=cptree))


prediction <- as.data.frame(predict(Zmodel,newdata= testData))
prediction$zone <- apply(prediction,1,which.max)
prediction$zone <- zone[prediction$zone]
prediction <- prediction %>% mutate(resultado = Dataset[-indexes,]$ZONE)
prediction <- prediction %>% mutate(check = ifelse(resultado == zone, 1, 0)) #Check if prediction is correct
success <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
success


prediction <- as.data.frame(predict(Zmodel,newdata= trainData))
prediction$zone <- apply(prediction,1,which.max)
prediction$zone <- zone[prediction$zone]
prediction <- prediction %>% mutate(resultado = Dataset[indexes,]$ZONE)
prediction <- prediction %>% mutate(check = ifelse(resultado == zone, 1, 0)) #Check if prediction is correct
success <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
success



prediction <- as.data.frame(round(predict(Bmodel,newdata= WapVal)))
prediction <- prediction %>% mutate(ZONE = round(prediction[,1]))
prediction <- prediction %>% mutate(resultado = vDataset$ZONE)
prediction <- prediction %>% mutate(check = ifelse(resultado == ZONE, 1, 0)) #Check if prediction is correct
successVal <- sum(na.omit(prediction$check))/nrow(prediction)*100 #Percentage of success
successVal


wap1 <- Dataset[which(WapSample[,1] > 0),521:524]



