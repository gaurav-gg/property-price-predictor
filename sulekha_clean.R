library(dplyr)
df <- read.csv("sulekha1.csv")
df <- select(df,Latitude,Longitude,BHK.BHK,Price,Area,City,Type,Rate)
df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)
df <- cbind(df,"sulekha")
df$rlat <- round(df$Latitude,digits = 3)
df$rlon <- round(df$Longitude,digits = 3)
names(df)[9] <- "source"
df$BHK.BHK <- as.character(df$BHK.BHK)
for(i in 1:nrow(df)){
  index <- regexpr(" ",df$BHK.BHK[i])
  df$BHK.BHK[i] <- substr(df$BHK.BHK[i],0,index-1)
}

df$City <- as.character(df$City)
df$City <- gsub(" ","",df$City)
df$City <- gsub("-","",df$City)
df$City <- tolower(df$City)

names(df) <- c("lat","lon","bhk","price","area","city","type","rate","source","rlat","rlon")
df1 <- read.csv("combined_prop_all1.csv")
df2 <- rbind(df,df1)
df2 <- unique(df2)
write.csv(df2,"combined_prop_all1.csv",row.names = F)
