library(stringr)
library(stringi)
library(tidyr)
library(dplyr)
data <- read.csv("Data/Buy/commonFloor/commonfloor.csv")
mat <- data %>% separate(col = "Area",into = c("Area","Rate"),sep = '@')
mat <- filter(mat,!is.na(mat$Rate))
mat <- filter(mat,!mat$BHK=="648 Sq. ft Apartment for sale")
str <- stri_reverse(mat$BHK)
str <- str_split_fixed(str,stri_reverse("Furnished"),2)
str <- as.data.frame(str)
str <- str$V1 %>% as.data.frame()
str <- str_split_fixed(str$.," K",2)
str <- as.data.frame(str)
BHK <- stri_reverse(str$V2) %>% as.data.frame()
names(BHK) = "BHK"
Type <- stri_reverse(str$V1)%>% as.data.frame()
Type <- str_split_fixed(Type$.," for",2)
Type <- as.data.frame(Type)
names(Type) <- "Type"
mat <- select(mat,-mat$`Type$Type`)
mat <- cbind(mat,Type$Type)
mat <- mat[,-13] %>% as.data.frame()
names(mat)[13] <- "Type"
data <- filter(data,!(Type=="1480 Sq. ft Apartment"|Type==""))
data_f <- select(data,Latitude,Longitude,BHK,Type,City,Price,Area,Rate)
convertPrice <- function(x){
  index1 <- regexpr("L",x)
  index2 <- regexpr("Cr",x)
  if(index1!=-1){
    x <- substr(x,0,index1-1)  %>% as.numeric()
    return(x*100000)
  }else if(index2!=-1){
    x <- substr(x,0,index2-1) %>% as.numeric()
    return(x*10000000)
  }else{
    return(x)
  }
}
Price <- lapply(as.character(data$Price), convertPrice)
Price <- as.data.frame(unlist(Price))
names(Price) <- "Price"
data_f <- select(data_f,-Price)
data_f <- cbind(data_f,Price)

mat$Name <- with(mat,paste(BHK,"in",Project_Name))
mat$Address <- with(mat,paste(Location,City,sep = ','))
mat <- select(mat,-Bathrooms,-Broker,-Possesion_Date)
mat$Area <- trimws(mat$Area)
write.csv(mat,"Data/Buy/commonfloor/commonFloor_final2.csv",row.names = F)




df <- read.csv("Data/Buy/commonfloor/commonFloor_final2.csv")
df$Area <- trimws(df$Area)
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
write.csv(df,"Data/commonFloor.csv",row.names = F)

