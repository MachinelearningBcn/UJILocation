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


OnlyWap <- read.csv("validationData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
set.seed(123)

OnlyWap <- OnlyWap[,1:520]
OnlyWap[OnlyWap == 100] <- -110

NumberOfWaps <- c()
WhoIsMax <- c()
for(i in 1:nrow(OnlyWap)){
  now <- length(which(OnlyWap[i,] != 100))
  NumberOfWaps <- c(NumberOfWaps,now) 
  maxi <- which(OnlyWap[i,1:520] == max(OnlyWap[i,1:520]))[1]
  WhoIsMax <- c(WhoIsMax,maxi)
}

OnlyWap$NumberOfWaps <- NumberOfWaps
OnlyWap$Top1 <- WhoIsMax


write.csv(OnlyWap,file="data/ValidationWap.csv",row.names=FALSE)
