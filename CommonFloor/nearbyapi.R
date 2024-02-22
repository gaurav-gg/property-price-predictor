library(httr)
library(jsonlite)
library(dplyr)
urloauth <- 'https://outpost.mapmyindia.com/api/security/oauth/token'
gt<-"client_credentials"
cid<-"92UWRtWgOX6Va_QMC4jG62LO09ZG0-Q1FuStTmpnpmk="
csecret<-"6G0FALtSaYAAIRcm3eLeo5oHDGEr4lLmx_lZeeUNH30="
CT<-"application/x-www-form-urlencoded"
response <- POST(urloauth,query = list(grant_type=gt,client_id=cid,client_secret=csecret),add_headers(Content_type=CT))
parsed<-fromJSON(content(response,"text"),simplifyVector = FALSE)
urlnear<-"https://atlas.mapmyindia.com/api/places/nearby/json?"
latlong <- read.csv('cities_latlong.csv')
city_list <- list("Bangalore","Mumbai","Delhi","Pune","Chennai","Hyderabad","Kolkata","Ahmedabad")
pois <- list("airport","atm","bank","bus stop","hospital","restaurant","school","shopping mall","station")
removeIrregular <- function(x){
  latlong1 <- filter(latlong,latlong$cities==city)
  count <- 0
  for(i in 1:nrow(x)){
    if(abs(x[i-count,1]-latlong1$lat)>2 || abs(x[i-count,2]-latlong1$long)>2){
      x <- x[-(i-count),]
      count <- count+1
    }
  }
  return(x)
}
for(poi in pois){
  key <- poi
  for(city in city_list){
    df <- read.csv(paste(city,'.csv',sep = ''))
    df <- df[,-1]
    df <- df[order(df$Latitude, df$Longitude),]
    df <- cbind(df,as.data.frame(round(df$Latitude,digits = 4)))
    df <- cbind(df,as.data.frame(round(df$Longitude,digits = 4)))
    names(df)[12] <- "latitude"
    names(df)[13] <- "longitude"
    df <- unique(df[c('latitude','longitude')])
    if(df[1,1]==0){
      df = df[-1,]
    }
    df <- filter(df,!is.na(df$latitude),!is.na(df$longitude))
    df <- cbind(df,as.data.frame(paste(df$latitude,',',df$longitude,sep = '')))
    names(df)[3] = "location"
    df <- removeIrregular(df)
    lat_df <- data.frame()
    long_df <- data.frame()
    count <- 0
    for(i in 1:nrow(df)){
      final<-GET(urlnear,query=list(keywords=key,refLocation=df$location[i-count]),add_headers(Authorization=paste(parsed$token_type,parsed$access_token)))
      if(final[["status_code"]]!=200){
        df <- df[-(i-count),]
        count <- count+1
        next
      }
      parsedfinal<-fromJSON(content(final,"text"),simplifyVector = TRUE)
      parsedfinal <- as.data.frame(parsedfinal)
      while(nrow(parsedfinal)<10){
        parsedfinal <- rbind(parsedfinal,NA)
      }
      lat_df <- rbind(lat_df,parsedfinal$suggestedLocations.latitude)
      long_df <- rbind(long_df,parsedfinal$suggestedLocations.longitude)
    }
    df <- cbind(df,lat_df,long_df)
    write.csv(df,paste(key,'/',city,'.csv',sep = ''))
  }
}