df <- read.csv("CommonFloor/commonFloor_final.csv")
df$Area <- as.character(df$Area)
for(i in 1:nrow(df)){
  index <- regexpr(" s",df$Area[i])
  df$Area[i] <- substr(df$Area[i],0,index-1)
}
df$Area <- as.numeric(df$Area)
df1 <- filter(df,!(df$Type=='Plot'))
df1$Area <- df1$Area/0.7
df1$Rate <- df1$Rate*0.7
df2 <- filter(df,(df$Type=='Plot'))
write.csv(df,"commonFloor1.csv",row.names = F)

d1 <- read.csv("CommonFloor/commonfloor.csv")
