library(rvest)
city_list <- read.csv('cities_latlong.csv')
city_list <- city_list[,-1]
url1 <- 'http://property.sulekha.com/PropertyAd/LoadListingsAjaxNew?parameters=&url=%2Fapartments-flats-in-'
url2 <- '-for-sale_page-'
url3<- '&FetchCount=0&callFrom=&listingsCount=0&NextAdId='
# seperate <- function(x){
#   odd <- seq_along(x)%%2==1
#   o <- x[odd]
#   e <- x[!odd]
#   return(data.frame(locality=o))
# }
times <- nrow(city_list)
i<- 1
while(nrow(city_list)!=0){
  #city <- city_list[1,2]
  temp_url <- paste(url1,city_list[1,2],url2,1,url3,sep = '')
  webpage_temp <- read_html(temp_url)
  total <- html_nodes(webpage_temp,'h1')
  total <- html_text(total)
  index <- regexpr("\\+",total)
  if(index==-1){
    index = regexpr(" ",total)
  }
  total <- substr(total,0,index-1)
  total <- paste(total,"\r\n",sep = '')
  total <- gsub(',','',total,fixed = T)
  total <- as.numeric(total)
  num <- total/25
  num <- as.integer(num)
  num <- num+1
  if(is.na(num)){
    city_list <- city_list[-1,]
    next
  }
  #num <- 4
  name_csv <- paste(city_list[1,2],".csv",sep = '')
  df <- data.frame(Name=factor(),AreaType=factor(),Area=factor(),Price=factor(),Project_Name=factor(),Latitude=factor(),Longitude=factor(),City=factor())
  write.csv(df,paste("New folder/",name_csv,sep=''),row.names = F)
  #final_df <- data.frame()
  j <- 1
  while(j<=num){
    url_cur <- paste(url1,city_list[1,2],url2,j,url3,sep = '') 
    webpage <- read_html(url_cur)
    name <- html_nodes(webpage,'.project-heading a')
    name <- html_text(name)
    name <- as.data.frame(name)
    price <- html_nodes(webpage,'.list-price b')
    price <- html_text(price)
    price <- as.data.frame(price)
    areaType <- html_nodes(webpage,'span:nth-child(3) em')
    areaType <- html_text(areaType)
    areaType <- as.data.frame(areaType)
    area <- html_nodes(webpage,'span:nth-child(3) b')
    area <- html_text(area)
    area <- as.data.frame(area)
    project <- html_nodes(webpage,'.left.truncate')
    project <- html_text(project)
    project <- as.data.frame(project)
    project[["project"]] <- gsub("   ","",project[["project"]],fixed=T)
    latitude <- html_nodes(webpage,xpath = '//meta[@itemprop="latitude"]')%>% html_attr('content')
    #latitude <- html_text(latitude)
    latitude <- as.data.frame(latitude)
    longitude <- html_nodes(webpage,xpath = '//meta[@itemprop="longitude"]')%>% html_attr('content')
    #longitude <- html_text(longitude)
    longitude <- as.data.frame(longitude)
    n_row = nrow(name)
    if(nrow(area)!=n_row || nrow(price)!=n_row || nrow(areaType)!=n_row || nrow(project)!=n_row || nrow(latitude)!=n_row || nrow(longitude)!=n_row){
      j <- j+1
      next
    }
    df <- cbind(name,areaType,area,price,project,latitude,longitude,city_list[1,2])
    df <- lapply(df[,], trimws)
    write.table(df,paste("New folder/",name_csv,sep = ''),append = T,sep = ',',col.names = F,row.names = F)
    r <- j
    print(paste(city_list[1,2],"-",r,"(",num,")"))
    j <- j+1
    #final_df <- rbind(final_df,df)
  }
  city_list <- city_list[-1,]
  write.csv(city_list,"cities.csv")
}

