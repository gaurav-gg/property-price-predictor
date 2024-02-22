library(dplyr)
cities <- list("Ahmedabad","Bangalore","Chennai","Delhi","Hyderabad","Kolkata","Mumbai","Pune")
final <- data.frame()
for(city in cities){
  df <- read.csv(paste(city,".csv",sep = ''))
  final <- rbind(final,df)
}
final <- final[,-2]
final <- as.data.frame(final)
write.csv(final,"commonfloor.csv")
