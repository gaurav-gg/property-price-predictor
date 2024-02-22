library(stringr)
library(ggplot2)
library(caTools)
library(plyr)
library(dplyr)
library(xgboost)
library(data.table)
library(caret)
library(GGally)
library(corrplot)
data <- read.csv('combined_prop_all1.csv')
data <- filter(data,!(data$source=='sulekha'))
#data1 <- filter(data,!(data$source=='quikr'|data$source=='99acres'|data$source=='sulekha'))
#data <- filter(data,(data$type=='apartment'|data$type=='flat'|data$type=='house'|data$type=='builderfloor'))
amenities <- read.csv('Data/propertywiseamen.csv')
#amenities1 <- read.csv('combined_data.csv')
#amenities <- rbind(amenities,amenities1)
#amenities <- unique(amenities)
# data <- data[order(data$rlat,data$rlon),]
# amenities <- amenities[order(amenities$lat,amenities$lon),]
names(amenities)[1:2] <- c("rlat","rlon")
merge <- merge(data,amenities,by = c("rlat","rlon"))
merge$bhk[is.na(merge$bhk)] <- 0
# merge$airport1[merge$airport1>1] <- 1
# merge$airport5[merge$airport2>3] <- 3
# merge$airport3[merge$airport3>2] <- 2
merge$commercial1 <- merge$commercial1 + merge$commercialComplex1
merge$commercial3 <- merge$commercial3 + merge$commercialComplex3
merge$commercial5 <- merge$commercial5 + merge$commercialComplex5
merge$higher_education1 <- merge$higher_education1 + merge$university1
merge$higher_education3 <- merge$higher_education3 + merge$university3
merge$higher_education5 <- merge$higher_education5 + merge$university5
merge$school1 <- merge$school1 + merge$play_school1
merge$school3 <- merge$school3 + merge$play_school3
merge$school5 <- merge$school5 + merge$play_school5
merge <- select(merge,-commercialComplex1,-commercialComplex3,-commercialComplex5)
merge <- select(merge,-university1,-university3,-university5)
merge <- select(merge,-play_school1,-play_school3,-play_school5)
merge$area <- as.character(merge$area) %>% as.numeric()

tier1 <- list("Ahmedabad", "Bangalore", "Chennai", "Delhi", "Hyderabad", "Kolkata", "Mumbai", "Pune","New-Delhi","Gurgaon","Delhi-ncr")
tier2 <- list("Ambala","Panchkula","Greater-Noida","Navi-Mumbai","Thane","Agra", "Ajmer", "Aligarh", "Allahabad", "Amravati", "Amritsar", "Asansol", "Aurangabad", "Bareilly", "Belgaum", "Bhavnagar", "Bhiwandi", "Bhubaneswar", "Bikaner", "Bokaro", "Chandigarh", "Coimbatore", "Cuttack", "Dehradun", "Dhanbad", "Durg-Bhilai Nagar", "Durgapur", "Erode", "Faridabad", "Firozabad","Gandhinagar", "Ghaziabad", "Gorakhpur", "Gulbarga", "Guntur", "Guwahati", "Gwalior", "Hubli-Dharwad", "Indore", "Jabalpur", "Jaipur", "Jalandhar", "Jammu", "Jamnagar", "Jamshedpur", "Jhansi", "Jodhpur", "Kannur", "Kanpur", "Kakinada", "Kochi", "Kottayam", "Kolhapur", "Kollam", "Kota", "Kozhikode", "Kurnool", "Lucknow", "Ludhiana", "Madurai", "Malappuram", "Mathura", "Goa", "Mangalore", "Meerut", "Moradabad","Mohali", "Mysore", "Nagpur", "Nanded", "Nashik", "Nellore", "Noida", "Patna", "Pondicherry", "Raipur", "Rajkot", "Rajahmundry", "Ranchi", "Rourkela", "Salem", "Sangli", "Siliguri", "Solapur", "Srinagar", "Sultanpur", "Surat", "Thiruvananthapuram", "Palakkad", "Thrissur", "Tiruchirappalli", "Tirunelveli", "Tiruppur", "Ujjain", "Vijayapura", "Vadodara", "Varanasi", "Vasai-Virar City", "Vijayawada", "Visakhapatnam", "Warangal", "Bhopal")
tier1 <- tolower(tier1)
tier2 <- tolower(tier2)
tier1 <- gsub(" ","",tier1)
tier2 <- gsub(" ","",tier2)
tier1 <- gsub("-","",tier1)
tier2 <- gsub("-","",tier2)
Tier <- lapply(merge$city, function(x){
  if(x %in%  tier1){
    return(1)
  }else if(x %in% tier2){
    return(2)
  }else{
    return(3)
  }
})
merge$Tier <- unlist(Tier)
# apartment <- lapply(merge$type, function(x){
#   if(x=='apartment'){
#     return(1)
#   }else{
#     return(0)
#   }
# })
# builderfloor <- lapply(merge$type, function(x){
#   if(x=='builderfloor'){
#     return(1)
#   }else{
#     return(0)
#   }
# }) 
# flat <- lapply(merge$type, function(x){
#   if(x=='flat'){
#     return(1)
#   }else{
#     return(0)
#   }
# }) 
# house <- lapply(merge$type, function(x){
#   if(x=='house'){
#     return(1)
#   }else{
#     return(0)
#   }
# }) 
# merge$apartment <- unlist(apartment)
# merge$house <- unlist(house)
# merge$builderfloor <- unlist(builderfloor)
# merge$flat <- unlist(flat)
# Apartments - Flats - Builderfloor - house
#merge <- filter(merge,(merge$type=='apartment'|merge$type=='flat'|merge$type=='house'|merge$type=='builderfloor'))
#merge <- filter(merge,!(merge$bhk==0))
#ggplot(merge11,aes(x=type,y=rate)) + geom_boxplot()
#ggplot(merge11,aes(x=lon,y=rate)) + geom_boxplot(aes(group = cut_width(lon,5)))
 type <- unique(merge$type) %>% as.list()
 merge11 <- data.frame()
 for(single in type){
  temp <- filter(merge,merge$type==single)
  lower <- boxplot.stats(temp$area)$stats[1]
  upper <- boxplot.stats(temp$area)$stats[5]
  temp <- filter(temp,!(temp$area<lower|temp$area>upper))
  merge11 <- rbind(merge11,temp)
 }
 bhk <- unique(merge11$bhk) %>% as.list()
 merge12 <- data.frame()
 for(single in bhk){
   temp <- filter(merge11,merge11$bhk==single)
   lower <- boxplot.stats(temp$area)$stats[1]
   upper <- boxplot.stats(temp$area)$stats[5]
   temp <- filter(temp,!(temp$area<lower|temp$area>upper))
   merge12 <- rbind(merge12,temp)
 }
#ggplot(merge12,aes(x=bhk,y=area)) + geom_boxplot(aes(fill=bhk))
#ggplot(merge12,aes(x=source,y=rate)) + geom_boxplot(aes(fill=source))
#merge12 <- filter(merge12,!(merge12$rate>100000))
merge12 <- filter(merge12,!(merge12$rate<100))
#ggplot(filter(merge12,!(merge11$type=='farmhouse')),aes(x=type,y=area)) + geom_boxplot(aes(fill=type))
merge12$bhk <- as.character(merge12$bhk)
merge12$bhk[merge12$bhk=='4+'] <- '11'
merge12$bhk[merge12$bhk=='10+'] <- '11'
merge12$bhk <- as.numeric(merge12$bhk)

#merge12 <- filter(merge,merge$source=="sulekha")
#merge12 <- select(merge11,unlist(important_features),rate,Tier1,Tier2,Tier3,apartment,builderfloor,house,flat)
merge12 <- select(merge12,-city,-source,-area,-price,-rlat,-rlon,-bhk)
#merge11 <- select(merge11,-Residential.Land1,-Residential.Land3,-Residential.Land5,-Institutional1,-Institutional3,-Institutional5)
cols <- names(merge12) %>% as.list()
merge12$type <- as.numeric(merge12$type)
set.seed(999)
merge12 <- merge12[sample(nrow(merge12)),]
#write.csv(merge12,"final_data.csv",row.names = F)
# smp_size <- floor(0.8*nrow(merge12))
# train_ind <- sample(seq_len(nrow(merge12)),size = smp_size)
# train <- merge12[train_ind,]
# test <- merge12[-train_ind,]
# train1 <- train$rate
# train2 <- select(train,-rate) %>% as.matrix()
# test1 <- select(test,-rate) %>% as.matrix()
train2 <- select(merge12,-rate) %>% as.matrix()
boost <- xgboost(data = train2,label = merge12$rate,nrounds = 6000,early_stopping_rounds=10,min_child_weight=3)
#boost <- xgb.load("model")
predict <- predict(boost,test1)
compare <- cbind(predict,test$rate)
compare <- cbind(compare,abs(predict-compare))
compare <- compare[,-3]%>% as.data.frame
compare <- cbind(test$lat,test$lon,test$type,compare)
error <- RMSE(pred = predict,obs = test$rate)
mae <- MAE(pred = predict,test$rate)
r2 <- R2(predict,test$rate)
relative <- summary(abs(test$rate-predict)/test$rate)
xgb.save(boost,"model")
#####
ggplot(merge11,aes(x=Tier,y=rate)) + geom_boxplot()

#ggpairs(data = merge,columns = c(6,10))
M <- cor(merge11[,(3:77)] )
# corr_rate <- M[,"rate"]
# corr_rate <- as.data.frame(corr_rate)
# corr_rate <- cbind(rownames(corr_rate),corr_rate$corr_rate) %>% as.data.frame()
# ggplot(corr_rate,aes(x=V1,y=V2))+
#   geom_point() + geom_line()
# 
# 
# corrplot(M,method = "square")
corre <- findCorrelation(M,cutoff = 0.75)
merge_corr <- select(merge11,-corre)


compare <- cbind(test[,(1:6)],compare[,4])

importance <- xgb.importance(feature_names = colnames(train2), model = boost)

xgb.plot.importance(importance_matrix = importance)

important_features <- importance$Feature[1:30]

###### IMPORTANT ##############
data1 <- filter(merge,!(merge$rate>20000))
##############################

cooksd <- cooks.distance(model = boost)



