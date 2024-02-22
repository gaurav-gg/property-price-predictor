library(rvest)
library(plyr)
library(jsonlite)
library(dplyr)
url_1 <- 'https://www.proptiger.com/app/v2/project-listing?selector={%22filters%22:{%22and%22:[{%22geoDistance%22:{%22geo%22:{%22distance%22:50,%22'
url_2 <- '}}}]},%22paging%22:{%22start%22:'
url_3 <- ',%22rows%22:1500},%22fields%22:[%22name%22,%22avgPriceRisePercentage%22,%22avgPriceRiseMonths%22,%22imageURL%22,%22projectStatus%22,%22address%22,%22distinctBedrooms%22,%22propertyUnitTypes%22,%22minPrice%22,%22maxPrice%22,%22offers%22,%22offer%22,%22offerHeading%22,%22offerDesc%22,%22minResalePrice%22,%22maxResalePrice%22,%22possessionDate%22,%22minPricePerUnitArea%22,%22propertySizeMeasure%22,%22minSize%22,%22maxSize%22,%22latitude%22,%22longitude%22,%22projectId%22,%22localityId%22,%22locality%22,%22cityId%22,%22builder%22,%22URL%22,%22authorized%22,%22avgPricePerUnitArea%22,%22derivedAvailability%22,%22totalUnits%22,%22launchDate%22,%22label%22,%22dominantUnitType%22,%22resalePricePerUnitArea%22,%22minResaleOrPrimaryPrice%22,%22maxResaleOrPrimaryPrice%22,%22safetyScore%22,%22livabilityScore%22,%22resaleEnquiry%22,%22isResale%22,%22amenities%22]}'
cities <- read.csv('cities.csv')
n_proj <- 0
for(i in 28:nrow(cities)){
  city <- cities$label[i]
  print(paste(city,": "))
  latlong <- paste("lat%22:",cities$centerLatitude[i],",%22lon%22:",cities$centerLongitude[i],sep = '')
  temp_url <- paste(url_1,latlong,url_2,"0",url_3,sep = '')
  download_html(temp_url,'one.json')
  data <- fromJSON(txt = 'one.json')
  total <- data[["totalCount"]]
  count <- 0
  df <- data.frame()
  while(count<total){
    url <- paste(url_1,latlong,url_2,count,url_3,sep = '')
    download_html(url,'two.json')
    data <- fromJSON(txt = 'two.json')
    data <- data[["data"]][["items"]]
    data2 <- select(data,-locality,-builder)
    data3 <- data$locality %>% as.data.frame()
    data4 <- select(data3,-suburb)
    names(data4) <- list("localityId","authorized","cityId","locality_label")
    data5 <- data3$suburb %>% as.data.frame()
    data6 <- select(data5,-city)
    names(data6) <- list("suburbId","cityId","suburb_label")
    data7 <- data5$city %>% as.data.frame()
    names(data7) <- list("cityId","authorized","city_label")
    data8 <- data$builder %>% as.data.frame()
    if(ncol(data8)==3){
      names(data8) <- list("builderId","builder_names","builder_url")
    }else if(ncol(data8)==2){
      names(data8) <- list("builderId","builder_names")
    }
    data <- cbind(data2,data4,data6,data7,data8)
    df <- rbind.fill(df,data)
    count <- nrow(df)
    print(paste(count," - ",total))
  }
  df <- as.data.frame(lapply(df[,], as.character))
  if(!is.null(df$launchDate) && !is.null(df$possessionDate)){
    launchDate <- anytime(as.numeric(as.character(df$launchDate))/1000) %>% as.data.frame()
    names(launchDate) <- list("launchDate")
    possessionDate <- anytime(as.numeric(as.character(df$possessionDate))/1000) %>% as.data.frame()
    names(possessionDate) <- list("possessionDate")
    df <- select(df,-launchDate,-possessionDate)
    df <- cbind(df,launchDate,possessionDate)
  }
  write.csv(df,paste("Properties/",city,".csv",sep = ''))
  n_proj <- n_proj + nrow(df)
}
print(paste("Total No. of Projects are",n_proj))
