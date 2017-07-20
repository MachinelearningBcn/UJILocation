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
Verbose = TRUE


source("Functions.R")


Dataset <- read.csv("data/OnlyWapNormCut.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
set.seed(123)

OnlyWap <- Dataset[,1:520]
OnlyWap[OnlyWap < 0.8] <- 0

Dataset[,1:520] <- OnlyWap

###Find Max of each row
write.csv(OnlyWap,file="data/OnlyWapNormCut08.csv",row.names=FALSE)
#write.csv(OnlyWapCut,file="data/OnlyWapCut.csv",row.names=FALSE)



