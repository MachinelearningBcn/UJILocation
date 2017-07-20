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


OnlyWap <- read.csv("Onlywap.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
set.seed(123)
#indexes <- createDataPartition(OnlyWap$WAP006, p = .10, list = FALSE)

#OnlyWap <- OnlyWap[indexes,]

WhoIsMax <- c()
WhoIsScnd <-c()
WhoIsThird <- c()
NumberOfWaps <- c()

OnlyWap[OnlyWap == 100] <- -110
OnlyWapCut <- OnlyWap
OnlyWapCut[OnlyWapCut > - 20] <- -110


for(i in 1:nrow(OnlyWap)){
  if(Verbose)  print(i)
  now <- length(which(OnlyWap[i,] != -110))
  NumberOfWaps <- c(NumberOfWaps,now) 
  maxi <- which(OnlyWap[i,1:520] == max(OnlyWap[i,1:520]))[1]
  WhoIsMax <- c(WhoIsMax,maxi)

  
  #minrow <- min(OnlyWapCut)[1]
  #maxrow <- max(OnlyWapCut)[1]
  
  #OnlyWapCut[i,] <- (OnlyWapCut[i,]-minrow)/maxrow
  
}





OnlyWap$NumberOfWaps <- NumberOfWaps
OnlyWap$Top1 <- WhoIsMax
#OnlyWap$Top2 <- WhoIsScnd
#OnlyWap$Top3 <- WhoIsThird



###Find Max of each row
write.csv(OnlyWap,file="data/OnlyWap.csv",row.names=FALSE)
write.csv(OnlyWapCut,file="data/OnlyWapCut.csv",row.names=FALSE)
