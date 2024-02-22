library(rvest)
library(plyr)
library(jsonlite)
library(dplyr)
cities <- read.csv('cities.csv')
url_1 <- 'https://www.proptiger.com/app/v1/amenity?'
url_2 <- '&distance=50&start=0&rows=2147483647'
for(i in 1:nrow(cities)){
  city <- cities$label[i]
  print(paste(city,": "))
  latlong <- paste('latitude=',cities$centerLatitude[i],'&longitude=',cities$centerLongitude[i],sep = '')
  url <- paste(url_1,latlong,url_2,sep = '')
  download_html(url,'test.json')
  data <- fromJSON(txt = 'test.json',simplifyDataFrame = T)
  data <- data[["data"]]
  if(length(data)==0){
    next
  }
  data2 <- select(data,-localityAmenityTypes)
  data3 <- data$localityAmenityTypes
  data3 <- as.data.frame(data3)
  data <- cbind(data2,data3)
  data <- as.data.frame(lapply(data[,],as.character))
  data <- subset(data,!duplicated(id))
  #data <- subset(data,!duplicated(name,vicinity))
  write.csv(data,paste("Amenities/",city,".csv",sep = ''))
}
