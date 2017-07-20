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

WapsDatasetCut <- read.csv("data/OnlyWapNormCut.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
Dataset <- read.csv("DatasetClean.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
Dataset$ZONE <- paste0(Dataset$BUILDINGID,"_",Dataset$FLOOR)

write.csv(Dataset,file="DatasetZone.csv",row.names=FALSE)
#write.csv(WapsDatasetCut,file="data/OnlyWapNormCut.csv",row.names=FALSE)

