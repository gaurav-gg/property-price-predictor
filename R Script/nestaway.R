library(rvest)
library(dplyr)
library(stringr)
library(rjson)
city_list <- list("mumbai","bangalore","delhi","gurgaon","noida","ghaziabad","Greater%20noida","faridabad","hyderabad","pune","navi%20mumbai","thane")
fields <- list("bhk_details","house_type","id","title","lat_double","long_double","bed_available_count","min_rent","nestaway_id","min_room_rent","min_room_advance","shared","rent","advance","gender","locality","booking_type","available_from","area","bathroom_count","furnishing_type","business_model","last_booking_date","list_view_photos")
for(city in city_list){
  url_i <- 'https://www.nestaway.com/search_new.json?bed_count=1&bed_type=bed&bedroom=0&city='
  url_i2 <- '&distance=5000&engine=v3&furnishing=&gender=&group=&groupId=&house_type=&isgrid=false&latitude=&locality=&longitude=&max_bed_count=20&max_movein_date=&max_price=250000&min_price=2000&order=&order_by=&page='
  url_i3 <- '&per_page=100&prime=0&pvt_from=&pvt_to=&roomtype=&soldout=true'
  count <- 0
  page <- 1
  df <- data.frame()
  while(count==((page-1)*100)){
    url <- paste(url_i,city,url_i2,page,url_i3,sep = '')
    download_html(url,'test.json')
    data <- fromJSON(file = 'test.json')
    data <- data[["houses"]]
    for(i in 1:length(data)){
      temp <- sapply(fields, function(x){data[[i]][[x]]}) %>% as.list()
      image_url <- temp[[24]][1]
      if(!is.null(image_url)){
        for(j in 2:length(temp[[24]])){
          image_url <- paste(image_url,temp[[24]][j],sep = '  ,  ')
        }
        temp <- temp[-24]
        temp[24] <- image_url
      }
      temp[lapply(temp, length)==0] <- NA
      temp <- as.data.frame(temp)
      names(temp) <- fields
      df <- rbind(df,temp)
    }
    count <- nrow(df)
    page <- page + 1
  }
  name_csv <- paste("Data/nestaway/",city,".csv",sep = '')
  write.csv(df,name_csv)
}
