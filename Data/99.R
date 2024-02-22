df <- read.csv('99acres_properties.csv')
test <- filter(df,df$PRICE_RANGE.min!=df$PRICE_RANGE.max)
data <- filter(df,(df$PRICE_RANGE.min==df$PRICE_RANGE.max))
data <- select(data,PROP_TYPE,LAT,LNG,BEDROOM_NUM,PRICE_RANGE.min,AREA,ADDRESS)
data1 <- filter(data,!AREA==" ")
data1 <- data1 %>% separate(col=AREA,into = c("AREA","Area_Unit"),sep = " ",extra = "merge")
test <- filter(data1,is.na(data1$Area_Unit))
data2 <- filter(data1,(Area_Unit=="Acres"|Area_Unit=="Cents"|Area_Unit=="Kottah"|Area_Unit=="Sq. Meter"|Area_Unit=="Sq. Yards"|Area_Unit=="Sq.Ft."))
data_acres <- filter(data2,Area_Unit=="Acres")
data_acres$AREA <- as.numeric(data_acres$AREA)*43560
data_acres$Area_Unit <- "Sq.Ft."
data_cents <- filter(data2,Area_Unit=="Cents")
data_cents$AREA <- as.numeric(data_cents$AREA)*435.62
data_cents$Area_Unit <- "Sq.Ft."
data_kottah <- filter(data2,Area_Unit=="Kottah")
data_kottah$AREA <- as.numeric(data_kottah$AREA)*720
data_kottah$Area_Unit <- "Sq.Ft."
data_sqm <- filter(data2,Area_Unit=="Sq. Meter")
data_sqm$AREA <- as.numeric(data_sqm$AREA)*10.7639
data_sqm$Area_Unit <- "Sq.Ft."
data_yrd <- filter(data2,Area_Unit=="Sq. Yards")
data_yrd$AREA <- as.numeric(data_yrd$AREA)*9
data_yrd$Area_Unit <- "Sq.Ft."
data_ft <- filter(data2,Area_Unit=="Sq.Ft.")
data3 <- rbind(data_acres,data_cents,data_ft,data_kottah,data_sqm,data_yrd)



write.csv(data3,"99_acres_prop.csv",row.names = F)


