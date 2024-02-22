library(osmdata)
library(plyr)
library(dplyr)
library(geosphere)
library(osmar)
india_box <- c(97.7186095715,7.9016472589,68.0115783215,35.6971946834)
india_data <- opq(bbox = india_box) %>% add_osm_feature(key='highway')%>%osmdata_sf()
df <- read.csv('Data/combinedpropertyAll.csv')
df <- df[order(df$Lat,df$Long),]
dff <- round(df,2)
df <- cbind(df,dff)
names(df) <- c("Lat","Long","latt","longg")
dist <- data.frame()
i<- 1
19.113305
72.863173
while(i<nrow(df)){
  box <- c(72.863173+0.05,19.113305-0.05,72.863173-0.05,19.113305+0.05)
  dd <- opq(bbox = box) %>% add_osm_feature(key = 'highway') %>% osmdata_sf()
  if(nrow(dd$osm_lines)==0){
    distance_hwy <- NA
    distance_mnrd <- NA
    distance <- cbind(distance_hwy,distance_mnrd)
    dist <- rbind(dist,distance)
    i <- i+1
    next
  }
  de <- as.data.frame(dd$osm_lines)
  de_hwy <- filter(de,(de$highway=='trunk'|de$highway=='trunk_link'))
  de_mnrd <- filter(de,(de$highway=='primary'|de$highway=='primary_link'))
  j <- i
  while(df$latt[i]==df$latt[j]&&df$longg[i]==df$longg[j]){
    distance_hwy <- lapply(de_hwy$geometry, function(x){
      d <- dist2Line(p = c(72.863173,19.113305),line = x,distfun = distHaversine)
      return(d[1,1])
    })
    if(length(distance_hwy)==0){
      distance_hwy = 6000
    }
    distance_hwy <- min(unlist(distance_hwy))
    distance_mnrd <- lapply(de_mnrd$geometry, function(x){
      d <- dist2Line(p = c(19.113305,72.863173),line = de_mnrd$geometry[2],distfun = distHaversine)
      return(d)
    })
    if(length(distance_mnrd)==0){
      distance_mnrd=6000
    }
    distance_mnrd <- min(unlist(distance_mnrd))
    distance <- cbind(distance_hwy,distance_mnrd)
    dist <- rbind(dist,distance)
    j <- j+1
    if(j>nrow(df)){
      break
    }
  }
  i <- j
  print(paste(i,"out of",nrow(df)))
}
