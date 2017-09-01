#Plotting
library("tidyr")
library("dplyr")
library("taRifx")
library("lubridate")
library("rpart")
library("caret")


WD <- "/Users/sediaz/Documents/GitHub/UJILocation"

Dataset <- read.csv("~/Documents/Ubiqum/Uji/DatasetClean.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
vDataset <- read.csv("~/Documents/Ubiqum/Uji/validationData.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)


ggplot(data=Dataset) + geom_point(aes(x=LATITUDE,y=LONGITUDE))+ xlab("Latitude (m)") + ylab("Longitude (m)") 


