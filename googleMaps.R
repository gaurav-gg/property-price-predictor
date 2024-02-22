library(mapsapi)
library(xml2)
df <- read.csv('combinedpropertyAll.csv')
distance_pt <- data.frame()
for(i in 800:nrow(df)){
  dd <- mp_directions(origin = c(df$Long[i],df$Lat[i]),destination = c(df$Long[i]-0.05,df$Lat[i]-0.05),mode = "transit")
  if(is.na(dd%>%xml_child('route'))){
    dd <- mp_directions(origin = c(df$Long[i],df$Lat[i]),destination = c(df$Long[i]+0.05,df$Lat[i]-0.05),mode = "transit")
  }
  if(is.na(dd%>%xml_child('route'))){
    dd <- mp_directions(origin = c(df$Long[i],df$Lat[i]),destination = c(df$Long[i]-0.05,df$Lat[i]+0.05),mode = "transit")
  }
  if(is.na(dd%>%xml_child('route'))){
    dd <- mp_directions(origin = c(df$Long[i],df$Lat[i]),destination = c(df$Long[i]+0.05,df$Lat[i]+0.05),mode = "transit")
  }
  if(is.na(dd%>%xml_child('route'))){
    distance_pt <- rbind(distance_pt,NA)
    next
  }
  seg <- mp_get_segments(dd)
  j <- 1
  dis <- 0
  while(seg$travel_mode[j]!="transit"){
    dis <- dis+seg$distance_m[j]
    j <- j+1
    if(j>nrow(seg)){
      dis<-NA
      break
    }
  }
  distance_pt <- rbind(distance_pt,as.data.frame(dis))
  print(paste(i,"out of",nrow(df)))
}
