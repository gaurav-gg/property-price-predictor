library(caTools)
library(dplyr)
library(plyr)
library(ggplot2)
library(class)
library(caret)
data <- read.csv('combined_prop_all.csv')
data2 <- filter(data,(data$type=='apartment'|data$type=='house'|data$type=='flat'|data$type=='builderfloor'))
data1 <- select(data2,lat,lon,rate,type)
third_quant <- summary(data1$rate)[5]
data1$Preminum <- ifelse(data1$rate>third_quant,T,F)
data1 <- select(data1,-rate)
data1$type <- factor(data1$type)
ggplot(data1,aes(x=lon,y=lat)) + geom_point(aes(color = Preminum))
sample <- sample.split(data1$Preminum,SplitRatio = 0.75)
train <- subset(data1,sample==T)
test <- subset(data1,sample==F)
y_pred <- knn(train = train[,-4],
              test = test[,-4],
              cl = train[,4],
              k = 5)
conmat <- table(y_pred,test$Preminum)
compare <- cbind(y_pred,test$Preminum) %>% as.data.frame()
compare$y_pred <- compare$y_pred -1 
  