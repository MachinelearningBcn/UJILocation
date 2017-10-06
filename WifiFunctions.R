rotate_matrix <- function(myangle, mydataframe){
  mydataframe$rot_x <- cos(myangle)*mydataframe[,1] - sin(myangle)*mydataframe[,2]
  mydataframe$rot_y <- sin(angle)*mydataframe[,1] + cos(myangle)*mydataframe[,2]
  mydataframe$rot_x <- mydataframe$rot_x - min(mydataframe$rot_x)
  mydataframe$rot_y <- mydataframe$rot_y - min(mydataframe$rot_y)
  return(mydataframe)
}

#Save NANs on normalizing by column
norm_vector <- function(myvector){
  vec_norm <- 0
  if(max(myvector) != min(myvector)){
    vec_norm <- (myvector - min(myvector))/(max(myvector) - min(myvector))
  }
  return(vec_norm)
}

#Normalizes a dataset by column
normalize_columns <- function(mydataframe){
  mydataframe <- lapply(mydataframe, function(x) norm_vector(x))
  return(as.data.frame(mydataframe))
}

#Normalizes a dataset by rows
normalize_rows <- function(mydataframe){
  mydataframe <- as.data.frame(t(apply(mydataframe, 1, function(x) (x - min(x))/(max(x)-min(x)))))
  return(as.data.frame(mydataframe))
}

#Selection columns containing waps information
preprocess_wifi <- function(mydataframe){
  mydataframe <- mydataframe[,1:520]
  mydataframe[mydataframe ==100] <- -110
  return(mydataframe)
}

find_wap <- function(myvector){
  mywap <- which(myvector > 0.5 & is.na(myvector) == FALSE)
  myb <- as.numeric(names(which.max(table(building_of_my_wap[mywap]))))
  return(myb)
}

build_pred <- function(where_is_wap, mydataframe){
  predicted_build <- apply(mydataframe, 1, function (x) find_wap(x))
  return(as.numeric(predicted_build))
}