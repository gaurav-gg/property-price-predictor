library(rvest)
library(dplyr)
library(stringr)

url_i <- 'https://www.magicbricks.com/property-for-sale/residential-real-estate?proptype=Multistorey-Apartment,Builder-Floor-Apartment,Penthouse,Studio-Apartment,Residential-House,Villa,Residential-Plot'
web_ori <- read_html(url_i)
n_prop <- html_nodes(web_ori,'.active span') %>% html_text()
n_prop <- n_prop[1]
n_prop <- gsub("\\(",'',n_prop)
n_prop <- gsub("\\)",'',n_prop) %>% as.numeric()
groupstart <- seq(30,n_prop,20)
groupstart <- append(groupstart,0,0)
url1 <- 'https://www.magicbricks.com/mbsearch/propertySearch.html?propertyType_new=10002_10003_10021_10022,10001_10017,10000&searchType=1&propertyType=10002,10003,10021,10022,10001,10017,10000&category=S&groupstart='
url2 <- '&offset=0&maxOffset=183287&attractiveIds=&page='
url3 <- '&ltrIds=&sortBy=postRecency'
j <- 1
df <- data.frame(ID=factor(),Locality=factor(),City=factor(),BHK=factor(),Latitude=factor(),Longitude=factor(),ListedBy=factor(),Link=factor(),PostedOn=factor(),Price=factor(),Fields=factor(),ListingType=factor())
write.csv(df,"Data/Magicbricks.csv")
while(length(groupstart)!=0){
  url <- paste(url1,groupstart[1],url2,j,url3,sep = '')
  webpage <- read_html(url)
  listing_type <- html_nodes(webpage,'.m-srp-card__listing-type') %>% html_text() %>% as.data.frame()
  posted <- html_nodes(webpage,'.m-srp-card__post-date span') %>% html_text() %>% as.data.frame()
  price <- html_nodes(webpage,'.m-srp-card__price') %>% html_text() %>% as.data.frame()
  if(nrow(price)==0){
    next
  }
  odd <- seq(1,nrow(price),2)
  price <- price[odd,] %>% as.data.frame()
  Name <- html_nodes(webpage,'.m-srp-card__title') %>% html_text() %>% as.data.frame()
  matrix <- str_split_fixed(Name$.,"\nfor Sale in\n",2)
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
  if(nrow(BHK)!=n_row||nrow(city)!=n_row||nrow(fields)!=n_row||nrow(lat)!=n_row||nrow(long)!=n_row||nrow(link)!=n_row||nrow(listed_by)!=n_row||nrow(listing_type)!=n_row||nrow(locality)!=n_row||nrow(price)!=n_row||nrow(posted)!=n_row){
    groupstart <- groupstart[-1]
    j <- j+1
    next
  }
  df <- cbind(id,locality,city,BHK,lat,long,listed_by,link,posted,price,fields,listing_type)
  print(paste(groupstart[1],j))
  write.table(df,"Data/Magicbricks.csv",append=T,col.names=F,sep=',')
  j <- j+1
  groupstart <- groupstart[-1]
}
