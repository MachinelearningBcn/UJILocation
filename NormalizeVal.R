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

WapsDataset <- read.csv("data/validationWap.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)

set.seed(123)
OnlyWap <- WapsDataset[,1:520]
Normalized_Waps <- t(apply(OnlyWap, 1, function(x)(x-min(x))/(max(x)-min(x))))


WapsDataset[,1:520] <- Normalized_Waps

write.csv(WapsDataset,file="data/ValidationWapNorm.csv",row.names=FALSE)
