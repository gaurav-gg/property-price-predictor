library(rvest)
library(plyr)
library(jsonlite)
library(dplyr)
url_1 <- 'https://www.proptiger.com/data/v1/entity/city/'
cities <- read.csv('cities.csv')
df <- data.frame()
for(i in 1:nrow(cities)){
  url <- paste(url_1,cities$id,sep = '')
  download_html(url,'tt.json')
  data <- fromJSON(txt = 'tt.json')
  data <- data[["data"]]
  count <- 0
  for(j in 1:length(data)){
    temp <- data[[j-count]]
    if(is.data.frame(temp)){
      data <- data[-j+count]
      count <- count + 1
    }
  }
  data <- lapply(data,as.character)
  temp <- t(as.data.frame(as.matrix(data,nrow=1,ncol=length(data))))
  df <- rbind.fill(df,as.data.frame(temp))
}
df <- as.data.frame(lapply(df[,],as.character))
write.csv(df,"cities_data.csv")
