library(rvest)
library(dplyr)
library(stringr)
library(rjson)
library(tidyverse)
url <- 'https://www.proptiger.com/data/v1/entity/city?selector=%7B%22sort%22:[%7B%22field%22:%22displayPriority%22,%22sortOrder%22:%22ASC%22%7D,%7B%22field%22:%22displayOrder%22,%22sortOrder%22:%22ASC%22%7D,%7B%22field%22:%22label%22,%22sortOrder%22:%22ASC%22%7D],%22paging%22:%7B%22start%22:0,%22rows%22:10000%7D,%22fields%22:[%22id%22,%22displayOrder%22,%22displayPriority%22,%22label%22,%22centerLatitude%22,%22centerLongitude%22,%22minZoomLevel%22,%22maxZoomLevel%22,%22amenities%22,%22latitude%22,%22longitude%22,%22localityAmenityTypes%22,%22name%22,%22isCouponAvailable%22,%22maxDiscount%22,%22authorized%22,%22cityPropertyCount%22,%22url%22]%7D'
download_html(url,'test.json')
data <- fromJSON(file = 'test.json')
data <- data[["data"]]
cities <- data.frame()
fields <- list("id","label","centerLatitude","centerLongitude","cityPropertyCount")
for(i in 1:length(data)){
  temp <- lapply(fields, function(x){data[[i]][[x]]})
  temp[lapply(temp, length)==0] <- NA
  names(temp) <- fields
  cities <- rbind(cities,as.data.frame(temp))
}
write.csv(cities,'cities.csv',row.names = F)
