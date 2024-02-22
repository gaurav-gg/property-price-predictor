library(dplyr)
cities <- list.files(pattern = '*.csv')
final <- data.frame()
for(city in cities){
  df <- read.csv(city)
  if(ncol(df)!=8){
    print(city)
    next
  }
  final <- rbind(final,df)
}
write.csv(final,"sulekha.csv")
