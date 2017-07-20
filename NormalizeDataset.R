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

WapsDatasetCut <- read.csv("data/OnlyWapCut.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
WapsDataset <- read.csv("data/OnlyWap.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)

set.seed(123)
OnlyWap <- WapsDataset[,1:520]
OnlyWapCut <- WapsDatasetCut[,1:520]

Normalized_Waps <- t(apply(OnlyWap, 1, function(x)(x-min(x))/(max(x)-min(x))))
Normalized_WapsCut <- t(apply(OnlyWapCut, 1, function(x)(x-min(x))/(max(x)-min(x))))

WapsDataset[,1:520] <- Normalized_Waps
WapsDatasetCut[,1:520] <- Normalized_WapsCut

write.csv(WapsDataset,file="data/OnlyWapNorm.csv",row.names=FALSE)
write.csv(WapsDatasetCut,file="data/OnlyWapNormCut.csv",row.names=FALSE)

