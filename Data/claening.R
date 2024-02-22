library(stringi)
library(stringr)
library(tidyr)
# df <- read.csv('99_acres_prop.csv')
# str <- stri_reverse(str = df$ADDRESS)
# str1 <- stri_split_fixed(str,",",2) %>% as.data.frame() 
# str1 <- t(str1)
# str1 <- as.data.frame(str1)
# str1 <- stri_reverse(str1$V1)
# str1 <- as.data.frame(str1)
# str2 <- str1%>%separate(col=str1,into=c("City","garbage"),sep = '\\(',extra = "merge")
# df <- select(df,-ADDRESS)
# City <- str2$City
# df <- cbind(df,City)
# Rate <- df$PRICE_RANGE.min/df$AREA
# Rate <- as.data.frame(Rate)
# df <- select(df,-Area_Unit)
# df <- cbind(df,Rate)
# write.csv(df,'99_acres_prop.csv',row.names = F)

sulekha <- read.csv('sulekha.csv')
sulekha$AreaType <- as.character(sulekha$AreaType)
df <- filter(sulekha,(sulekha$AreaType=='Built-up Area'|sulekha$AreaType=='Carpet Area'|sulekha$AreaType=='Super Built-up Area'))
df$Area <- as.numeric(df$Area)
df1 <- filter(df,df$AreaType=='Built-up Area')
df1$Area <- df1$Area/0.8
df2 <- filter(df,df$AreaType=='Carpet Area')
df2$Area <- df2$Area/0.65
df3 <- filter(df,df$AreaType=='Super Built-up Area')
df <- rbind(df1,df2,df3)
df <- select(df,-AreaType)
str <- stri_split_fixed(df$Name," in ",2) %>% as.data.frame() %>% t() %>% as.data.frame()
str1 <- stri_reverse(str$V1) %>% as.data.frame()
str2 <- separate(data = str1,col=.,into=c("Type","BHK"),sep = "K",extra = "merge")
BHK <- stri_reverse(str2$BHK) %>% as.data.frame()
BHK <- separate(BHK,.,into=c("BHK","Garbage"),sep = "K ")
df1 <- cbind(df,BHK$BHK,Type)
df2 <- filter(df1,BHK$BHK=="1 BH"|BHK$BHK=="1 R"|BHK$BHK=="2 BH"|BHK$BHK=="2 R"|BHK$BHK=="3 BH"|BHK$BHK=="4 BH"|BHK$BHK=="4+ BH")
df3 <- filter(df1,is.na(df1$`BHK$BHK`))
df1 <- filter(df1,!(BHK$BHK==""))
df2 <- filter(df1,!df1$`BHK$BHK`==""|df1$`BHK$BHK`==" ")
df3$City <- as.character(df3$City)
#df3 <- filter(df3,!df3$City=="Chennai")
df3 <- filter(df3,!df3$Latitude==0)
df2 <- filter(df2,!df2$Latitude==0)
Type <- stri_reverse(str2$Type) %>% as.data.frame()
Type <- filter(Type,BHK$BHK=="1 BH"|BHK$BHK=="1 R"|BHK$BHK=="2 BH"|BHK$BHK=="2 R"|BHK$BHK=="3 BH"|BHK$BHK=="4 BH"|BHK$BHK=="4+ BH")
df4 <- separate(df2,col = .,into = c("Type","garbage"),sep = " for ")
df_final <- filter(df_final,(Type==""|Type==" Affordable Flat"|Type==" Affordable Independent House"|Type==" Affordable Independent Villa"|Type==" Builder Floor"|Type==" Flat"|Type==" Low Budget Flat"|Type==" Luxury Flat"|Type==" Residential Flat"))
df6 <- select(df5,-Name,-garbage)
df7 <- select(df3,-Name)
df7 <- select(df7,-.)
df7$Type <- "Plot"
df_final <- rbind(df6,df7)
tt <- str_replace_all(df_final$perSqrt,",","") %>% as.data.frame()
tt <- as.character(tt$.) %>% as.data.frame()
df_final$perSqrt <- tt$.
df_final$perSqrt <- as.character(tt$.)
df_final1 <- filter(df_final,!perSqrt=="N/A")
df_final1$perSqrt <- as.numeric(df_final1$perSqrt)
df_final1 <- filter(df_final1,!is.na(perSqrt))
df1$Price <- as.character(df1$Price)
df1 <- filter(df1,!Price=="Contact for Price")
convertPrice <- function(x){
  index1 <- regexpr("L",x)
  index2 <- regexpr("C",x)
  if(index1!=-1){
    x <- substr(x,0,index1-1) %>% as.numeric()
    return(x*100000)
  }else if(index2!=-1){
    x <- substr(x,0,index2-1) %>% as.numeric()
    return(x*10000000)
  }else{
    return(NA)
  }
}
Price <- lapply(df1$Price, convertPrice) %>% as.data.frame()
Price <- t(Price) %>% as.data.frame()
names(Price) = "Price"
df1 <- select(df1,-Price)
df1 <- cbind(df1,Price)
df_final <- df1
df_final$Rate <- df_final$Price/df_final$Area
write.csv(df_final,"sulekha12.csv",row.names = F)
df_final <- read.csv("sulekha12.csv")
df <- select(df_final,Latitude,Longitude,BHK.BHK,Price,Area,City,Type,Rate)
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
#####################

data <- read.csv("CommonFloor/commonfloor.csv")
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
write.csv(mat,"commonFloor_final2.csv",row.names = F)




df <- read.csv("commonFloor_final2.csv")
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
write.csv(df,"commonFloor1.csv",row.names = F)

