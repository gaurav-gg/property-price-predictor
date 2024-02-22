library(osmdata)
library(plyr)
library(dplyr)
library(geosphere)
library(osmar)
properties <- read.csv('combined_prop_all.csv')
properties <- properties[(order(properties$rlat,properties$rlon)),]
# north_box <- c(70.5713927746,28.183118439,88.4131896496,35.7061163143);
# west_box <- c(68.2862365246,20.6684649041,86.6553771496,27.9892669456);
# east_box <- c(86.7432677746,20.8328433566,97.2901427746,29.4919264612);
# south_box <- c(72.9444396496,8.096981884,86.6553771496,20.5862088191);
latp <- round(properties$lat,digits = 1)
lonp <- round(properties$lon,digits = 1)
properties <- cbind(properties,latp,lonp)
i<-1
dist_hwy <- data.frame()
dist_pri <- data.frame()
while(i<=nrow(properties)){
  lat <- properties$latp[i]
  lon <- properties$lonp[i]
  boxx <- c(lon-0.2,lat-0.2,lon+0.2,lat+0.2)
  dd <- opq(bbox = boxx,timeout = 100) %>% add_osm_feature(key = 'highway') %>% osmdata_sf()
  dl <- dd$osm_lines ########
  de_hwy <- filter(dl,dl$highway=='trunk'|dl$highway=='trunk_link')
  de_pri <- filter(dl,dl$highway=='primary'|dl$highway=='primary_link')
  j <- i
  while(lat==properties$latp[j]&&lon==properties$lonp[j]){
    distance_hwy <- lapply(de_hwy$geometry, function(x){
      d <- dist2Line(p=c(properties$lon[j],properties$lat[j]),line=x,distfun = distHaversine)
      return(d[1,1])
    })
    distance_hwy <- min(unlist(distance_hwy))
    distance_pri <- lapply(de_pri$geometry, function(x){
      d <- dist2Line(c(properties$lon[j],properties$lat[j]),x,distfun = distHaversine)
      return(d[1,1])
    })
    distance_pri <- min(unlist(distance_pri))
    dist_hwy <- rbind(dist_hwy,distance_hwy)
    dist_pri <- rbind(dist_pri,distance_pri)
    print(paste(j,"out of",nrow(properties)))
    j <- j+1
    if(j>nrow(properties)){
      break;
    }
    write.table(distance_hwy,"distance_hwy.csv",append = T,col.names = F,sep=",")
    write.table(distance_pri,"distance_pri.csv",append = T,col.names = F,sep=",")
  }
  i <- j
}
    write.csv(dist_hwy,"distance_hwy.csv")
write.csv(dist_pri,"distance_pri.csv")
