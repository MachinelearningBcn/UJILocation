CreateBuilding <- function(BuildingNumber){
  
  Building <- Dataset[which(Dataset$BUILDINGID==BuildingNumber),]

  Building <- Building %>% mutate(LONGITUDE = LONGITUDE - min(Building$LONGITUDE))
  Building <- Building %>% mutate(LATITUDE = LATITUDE - min(Building$LATITUDE))
  
  ctk <- c()
  
  for(i in 1:520){
    
    if(length(which(Building[,i]!=100)) ==0){ 
      ctk <- c(ctk,i)    
    }
  }
  
  write.csv(ctk,file="B1waps.csv",row.names=FALSE)
  Building[,ctk] <- NULL
   
  for(j in 1:ncol(Building-9)){
    for(i in 1:length(Building$WAP006)){
      
      signal  =  Building[i,j] 
      
      if(signal==100){
        signal = -100000
      }
      
      Building[i,j] = signal
    }
  }  
    return(Building)
  
}

PredictLongitude <- function(){
  
  set.seed(123)
  inTraining <- createDataPartition(BuildingWAP$WAP006, p = .80, list = FALSE)
  
  
  B1Train <- BuildingWAP[inTraining,]
  B1Test <-  BuildingWAP[-inTraining,]
  
  LonModel <-  rpart(LONGITUDE ~. ,
                     data = B1Train,
                     control = rpart.control(maxdepth = 24,cp=0.00001))
  
  
  prediction <- as.data.frame(predict(LonModel,newdata= B1Test))
  prediction <- prediction %>% mutate(prediccion = prediction[,1])
  prediction <- prediction %>% mutate(resultado = Building[-inTraining,]$LONGITUDE)
  prediction <- prediction %>% mutate(distance = abs(resultado - prediccion)) #Calculate distance in coordinates
  ErrorLon <- sum(prediction$distance)/nrow(prediction) #Average error
  ErrorLon
  
  
 return(ErrorLon) 
}

PredictLatitude <- function(){
  
  set.seed(123)
  inTraining <- createDataPartition(BuildingWAP$WAP006, p = .80, list = FALSE)
  
  B1Train <- BuildingWAP[inTraining,]
  B1Test <-  BuildingWAP[-inTraining,]
  
  LatModel <-  rpart(LATITUDE ~. ,
                     data = B1Train,
                     control = rpart.control(maxdepth = 24,cp=0.00001))
  
  
  prediction <- as.data.frame(predict(LatModel,newdata= B1Test))
  prediction <- prediction %>% mutate(prediccion = prediction[,1])
  prediction <- prediction %>% mutate(resultado = Building[-inTraining,]$LATITUDE)
  prediction <- prediction %>% mutate(distance = abs(resultado - prediccion)) #Calculate distance in coordinates
  ErrorLat <- sum(prediction$distance)/nrow(prediction) #Average error
  ErrorLat
  
  return(ErrorLat) 
}