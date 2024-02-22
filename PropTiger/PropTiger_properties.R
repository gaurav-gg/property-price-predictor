library(rvest)
library(plyr)
library(jsonlite)
library(dplyr)
library(anytime)
url_1 <- 'https://www.proptiger.com/data/v1/entity/project/'
url_2 <- '/phase'
cities <- read.csv('cities.csv')
for(i in 1:nrow(cities)){
  city <- cities$label[i]
  projects <- read.csv(paste("Projects/",city,".csv",sep = ''))
  df <- data.frame()
  for(j in 1:nrow(projects)){
    id <- projects$projectId[j]
    url <- paste(url_1,id,url_2,sep = '')
    download_html(url,'test.json')
    data <- fromJSON(txt = 'test.json')
    data <- data[["data"]][["properties"]] 
    for(k in 1:length(data)){
      if(length(data[[k]])==0){
        next
      }
      temp <- cbind(data[[k]],projects$latitude[j],projects$longitude[j])
      if(!is.null(temp$imageTypeCount)){
        temp <- select(temp,-imageTypeCount) 
      }
      if(!is.null(temp$reraProject)){
        temp <- select(temp,-reraProject)
      }
      df <- rbind.fill(df,as.data.frame(temp))
    }
    print(paste(j," - ",nrow(projects),"in",city))
  }
  df <- as.data.frame(lapply(df[,], as.character))
  if(!is.null(df$createdAt) && !is.null(df$updatedAt)){
    createdAt <- anytime(as.numeric(as.character(df$createdAt))/1000) %>% as.data.frame()
    names(createdAt) <- list("createdAt")
    updatedAt <- anytime(as.numeric(as.character(df$updatedAt))/1000) %>% as.data.frame()
    names(updatedAt) <- list("updatedAt")
    df <- select(df,-createdAt,-updatedAt)
    df <- cbind(df,createdAt,updatedAt)
  }
  write.csv(df,paste("Properties/",city,".csv",sep = ''))
}
