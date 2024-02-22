library(rvest)
library(dplyr)
library(stringr)
########## TO store the ID of most recent property scrapped################
df_save <- read.csv("Data/Magicbricks_rent.csv")
id_recent <- df_save$ID[1] 
####################
url_i <- 'https://www.magicbricks.com/mbsearch/propertySearch.html?editSearch=Y&category=R&propertyType=10002,10003,10021,10022,10020,10001,10017&searchType=1&page=1&tab1=&sortBy=postRecency'
web_temp <- read_html(url_i)
total <- html_nodes(web_temp,'.SRHeading') %>% html_text()
index <- regexpr(" ",total)
total <- as.numeric(substr(total,0,index-1))
count <- 0
page <- 1
url1 <- 'https://www.magicbricks.com/mbsearch/propertySearch.html?editSearch=Y&category=R&propertyType=10002,10003,10021,10022,10020,10001,10017&searchType=1&page='
url2 <- '&tab1=&sortBy=postRecency'
df <- data.frame(ID=factor(),Locality=factor(),City=factor(),BHK=factor(),Latitude=factor(),Longitude=factor(),ListedBy=factor(),Link=factor(),PostedOn=factor(),Rent=factor(),Fields=factor())
write.csv(df,"Data/Magicbricks_rent.csv")
while(count<total){
  url <- paste(url1,page,url2,sep = '')
  webpage <- read_html(url)
  posted <- html_nodes(webpage,'.m-srp-card__post-date span') %>% html_text() %>% as.data.frame()
  price <- html_nodes(webpage,'.m-srp-card__price') %>% html_text() %>% as.data.frame()
  if(nrow(price)==0){
    next
  }
  odd <- seq(1,nrow(price),2)
  price <- price[odd,] %>% as.data.frame()
  Name <- html_nodes(webpage,'.m-srp-card__title') %>% html_text() %>% as.data.frame()
  matrix <- str_split_fixed(Name$.,"\nfor ",2)
  locality <- matrix[,2] %>% as.data.frame()
  BHK <- matrix[,1] %>% as.data.frame()
  fields <- html_nodes(webpage,'.js-collapse__content') %>% html_text() %>% as.data.frame()
  listed_by <- html_nodes(webpage,'.m-srp-card__advertiser__name') %>% html_text() %>% as.data.frame()
  link_t <- html_nodes(webpage,'.m-srp-card__heading a') %>% xml_attrs('href')
  link <- data.frame()
  city <- data.frame()
  for(i in 1:length(link_t)){
    ct <- link_t[[i]][["href"]]
    index1 <- regexpr("-in-",ct)
    index2 <- regexpr('&',ct)
    city <- rbind(city,as.data.frame(substr(ct,index1+4,index2-1)))
    link <- rbind(link,as.data.frame(ct))
  }
  id <- html_nodes(webpage,'.m-srp-card') %>% html_attr('data-id') %>% as.data.frame()
  lat <- data.frame()
  long <- data.frame()
  photo <- data.frame()
  x_path1 <- '//*[@id="pmtLat'
  x_path2 <- '//*[@id="pmtLong'
  x_path3 <- '//*[@id="resultBlockWrapper'
  for(i in 1:nrow(id)){
    temp1 <- html_nodes(webpage,xpath = paste(x_path1,id$.[i],'"]',sep = '')) %>% html_attr('value')
    temp2 <- html_nodes(webpage,xpath = paste(x_path2,id$.[i],'"]',sep = '')) %>% html_attr('value')
    temp3 <- html_nodes(webpage,xpath = paste(x_path3,id$.[i],'"]/div[2]/div[1]/div[2]/div[1]/img',sep = '')) %>% html_attr('data-src')
    photo <- rbind(photo,as.data.frame(temp3))
    lat <- rbind(lat,as.data.frame(temp1))
    long <- rbind(long,as.data.frame(temp2))
  }
  n_row<- nrow(id)
  if(nrow(BHK)!=n_row||nrow(city)!=n_row||nrow(fields)!=n_row||nrow(lat)!=n_row||nrow(long)!=n_row||nrow(link)!=n_row||nrow(listed_by)!=n_row||nrow(locality)!=n_row||nrow(price)!=n_row||nrow(posted)!=n_row){
    page <- page+1
    next
  }
  df <- cbind(id,locality,city,BHK,lat,long,listed_by,link,posted,price,fields)
  if(id_recent %in% id$.){
    end <- grep(id_recent,id$.)
    df <- df[1:end,] %>% as.data.frame()
    print("END---Properties after this are already stored!")
    write.table(df,"Data/Magicbricks_rent.csv",append=T,col.names=F,sep=',')
    break
  }
  print(paste(count,"-",total))
  write.table(df,"Data/Magicbricks_rent.csv",append=T,col.names=F,sep=',')
  count <- count + nrow(id)
  page <- page+1
}
write.table(df_save,"Data/Magicbricks_rent.csv",append = T,col.names = F,sep = ',',row.names = F)
