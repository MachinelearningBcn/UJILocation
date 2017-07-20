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


OnlyWap <- read.csv("data/Onlywap.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
set.seed(123)
OnlyWap[OnlyWap == 100] <- -110


rowstokill <- c()
for(i in 1:nrow(OnlyWap)){
  print(i)
  now <- length(which(OnlyWap[i,1:520] > -20))
  rowstokill <- c(rowstokill,now)
}
  rowstokill <- which(rowstokill > 0)

OnlyWap <- OnlyWap[-rowstokill,]

###Find Max of each row
write.csv(OnlyWap,file="data/OnlyWapOut.csv",row.names=FALSE)

