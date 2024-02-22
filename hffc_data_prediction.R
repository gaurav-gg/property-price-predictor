library(dplyr)
library(xgboost)
library(caret)
library(geosphere)
df <- read.csv("New Folder/GeoLiteCity-Location.csv")
data <- read.csv("New Folder/Property data.csv")
data$Project.Pin.Code <- as.character(data$Project.Pin.Code)
data <- filter(data,!is.na(data$Standard.Rate.Per.Sq..Ft...at.Inception.))
df$country <- as.character(df$country)
df <- filter(df,df$country=='IN')
df$postalCode <- as.character(df$postalCode)
count <- 0
lat <- data.frame()
lon <- data.frame()
for(i in 1:nrow(data)){
  pincode <- data$Project.Pin.Code[i]
  index <- grep(pincode,df$postalCode)
  if(length(index)==0){
    rlat <- NA
    rlon <- NA
  }else{
    rlat <- df$latitude[index]
    rlon <- df$longitude[index]
  }
  lat <- rbind(lat,rlat)
  lon <- rbind(lon,rlon)
}
data$lat <- lat
data$lon <- lon
data <- select(data,lat,lon,Standard.Rate.Per.Sq..Ft...at.Inception.)
data$lat <- lat$X21.15
data$lon <- lon$X79.1
colnames(data) <- c("lat","lon","rate")


predictValues <- function(lat,lon){
  pois <- amenities[which.min(abs(lat-amenities$lat)^2+abs(lon-amenities$lon)^2),]
  if(distGeo(c(lat,lon),c(pois$lat,pois$lon)) > 2000){
    predict <- NA
  }else{
    type <- 2
    test <- cbind(lat,lon,type,pois[,(3:66)])
    predict <- predict(model,as.matrix(test))
  }
  return(predict)
}

data <- filter(data,!is.na(data$lat))
amenities <- read.csv("amenities.csv")
model <- xgb.load("model")
predicted <- data.frame()
for(i in 1:nrow(data)){
  temp <- predictValues(data$lat[i],data$lon[i])
  predicted <- rbind(predicted,temp)
}
compare <- cbind(predicted,data$rate,abs(predicted-data$rate))
