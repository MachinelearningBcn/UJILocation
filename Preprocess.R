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


Dataset <- read.csv("trainingData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
vDataset <- read.csv("validationData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)

Dataset$LATITUDE <- Dataset$LATITUDE - min(Dataset$LATITUDE)
Dataset$LONGITUDE <- Dataset$LONGITUDE - min(Dataset$LONGITUDE)


NumberOfWaps <- c()

OnlyWap <- Dataset[,1:520]
for(i in 1:nrow(OnlyWap)){
  now <- length(which(OnlyWap[i,] != 100))
  NumberOfWaps <- c(NumberOfWaps,now)    
}
noSignal <- which(NumberOfWaps <1)
Dataset<-Dataset[-noSignal,]
OnlyWap <- OnlyWap[-noSignal,]


write.csv(Dataset,file="DatasetClean.csv",row.names=FALSE)
write.csv(OnlyWap,file="Onlywap.csv",row.names=FALSE)


