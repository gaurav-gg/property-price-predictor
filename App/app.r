library(shiny)
library(xgboost)
library(leaflet)
library(geosphere)
library(httr)
library(jsonlite)
library(dplyr)
library(rvest)

#JavaScript Code, used to stimulate button code on Pressing Enter
jscode <- '
$(function() {
  var $els = $("[data-proxy-click]");
  $.each(
    $els,
    function(idx, el) {
      var $el = $(el);
      var $proxy = $("#" + $el.data("proxyClick"));
      $el.keydown(function (e) {
        if (e.keyCode == 13) {
          $proxy.click();
        }
      });
    }
  );
});
'
hide1 <- '$(document).ready(function(){
$("#clickSide1").hide();});'
hide2 <- '$(document).ready(function(){
$("#clickSide").hide();});'
hide3 <- '$(document).ready(function(){
$("#clickMain").hide();});'
# Function to get the geolocation for provided address parameter
getCoordinates <- function(address) {
  address <- gsub(" ","%20",address)
  url <- paste("http://maps.googleapis.com/maps/api/geocode/json?address=",address,"&output=json&components=country:IN",sep="")
  map_data <- fromJSON(paste(readLines(url),collapse=""))
  coord <- map_data[["results"]][["geometry"]][["location"]]
  return(coord)
}

#List of Icons to be added to leaflet Map
iconList <- iconList(
  bank <- makeIcon("https://searchosm.com/icons/117.png"),
  school <- makeIcon("https://searchosm.com/icons/101.png"),
  hospital <- makeIcon("https://searchosm.com/icons/160.png"),
  bus <- makeIcon("https://searchosm.com/icons/193.png"),
  train <- makeIcon("https://searchosm.com/icons/192.png"),
  hotel <- makeIcon("https://searchosm.com/icons/2.png"),
  restaurant <- makeIcon("https://searchosm.com/icons/30.png"),
  airport <- makeIcon("https://searchosm.com/icons/191.png"),
  home <- makeIcon("http://www.tematy.info/library/h/home-icon-png-transparent/home-icon-png-transparent-05.png",iconWidth = 25,iconHeight = 25),
  sales <- makeIcon("https://www.nepal.gov.np/splash/images/ICONS/Home.png",iconWidth = 25,iconHeight = 25)
)
#Load Global Environment required for the App
load("data.RData",.GlobalEnv)

# #model trained on the Scrapped Data
# #properties data used to display nearby properties
# #amenities data to add to the model as test data with lat lon
# #salesforce dara to add to the map
##Ploygons of localities with average Rates

##### Run only To Updata Data for the App #########
##### Comment it out before deploying the app ############
#model <- xgb.load("Data/model_final")
#properties <- read.csv('Data/combined.csv',stringsAsFactors = F)
#amenities <- read.csv('Data/amenities.csv',stringsAsFactors = F)
#salesforce <- read.csv("Data/salesforce.csv",stringsAsFactors = F)
#polygons <- read.csv("Data/polygons.csv",stringsAsFactors = F)
#save.image("App/data.RData")
########################################
polygons <- polygons %>% mutate(X=paste("<a href='https://www.magicbricks.com",detailLink,"' target='_blank'>",'<strong>Locality Name: </strong>',
                                        localityName,
                                        '<br><strong>Average Rate: </strong>',
                                        prSqFt,
                                        '<br><strong>Status: </strong>',
                                        locStatus,
                                        "</a>",sep = ''))
# ui and server, defined functions for shiny
ui <- fluidPage(
  tags$head(tags$script(HTML(jscode))),
  tags$head(tags$script(HTML(hide1))),
  tags$head(tags$script(HTML(hide2))),
  tags$head(tags$script(HTML(hide3))),#Run JS script present in jscode
    sidebarPanel(
    titlePanel("Rate Predictor"),
    tags$head(tags$style("#map{height:100vh !important;}")),
    tags$head(tags$style("#trends{height:100vh !important;}")),
    tagAppendAttributes(
      numericInput(inputId = 'lat',label = "Latitude",value = 13.1563889),
      `data-proxy-click` = "clickSide" #which button's code to stimulate on pressing Enter
    ),
    tagAppendAttributes(
      numericInput(inputId = 'lon',label = 'Longitude',value = 80.0347222),
      `data-proxy-click` = "clickSide"
    ),
    tagAppendAttributes(
      numericInput(inputId = 'area',label = "Area in sqft.",value = 0),
      `data-proxy-click` = "clickSide1"
    ),
    actionButton(label="",inputId="clickSide1"),
    actionButton(label = "Update",inputId = "clickSide"),
    h3("Rates Predicted"),
    tableOutput("rates"),
    h3("NearBy Properties"),
    tableOutput("nearby")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Predictor",
               leafletOutput("map"),
               absolutePanel(top = 50,left = 70, #absolutePanel is used to Overlay search bar over map
                             tagAppendAttributes(
                               textInput("search", NULL, ""),
                               `data-proxy-click` = "clickMain"
                             )),
               absolutePanel(top = 50,left = 370,actionButton(label = "Search",inputId = "clickMain"))
               ),
      tabPanel("Locality Trends",
               leafletOutput("trends"),
               absolutePanel(top = 50,left = 70, #absolutePanel is used to Overlay search bar over map
                             tagAppendAttributes(
                               textInput("search2", NULL, ""),
                               `data-proxy-click` = "clickMain2"
                             )),
               absolutePanel(top = 50,left = 370,actionButton(label = "Search",inputId = "clickMain2"))
               )
    )
  )
)
server <- function(input,output,session){
  latitude=19.0760 #global variables
  longitude=72.8777
  sideLatitude=19.0760 #global variables
  sideLongitude=72.8777
  prop <- data.frame()
  new_zoom = 16
  dataTable <- reactiveValues()
  observeEvent(input$clickSide1,{
    if(input$lat!=sideLatitude | input$lon!=sideLongitude){
      latitude <<- input$lat
      longitude <<- input$lon
      sideLongitude <<- input$lon
      sideLatitude <<- input$lat
      dataTable$data <- predictValues(input$lat,input$lon,input$area)
    }else{
      dataTable$data <- predictValues(latitude,longitude,input$area)
    }
  })
  observeEvent(input$clickSide,{
    sideLatitude <<- input$lat
    sideLongitude <<- input$lon
    latitude <<- sideLatitude
    longitude <<- sideLongitude
    dataTable$data <- predictValues(sideLatitude,sideLongitude,input$area)
  })
  observeEvent(input$clickMain,{
    tt <- getCoordinates(input$search)
    if(!is.null(tt)){
      if(nrow(tt)>1){
        tt <- tt[1,]
      }
      latitude <<- tt$lat
      longitude <<- tt$lng
    }
    dataTable$data <- predictValues(latitude,longitude,input$area)
  })
  observeEvent(input$map_click,{
    latitude <<- input$map_click$lat
    longitude<<- input$map_click$lon
    dataTable$data <- predictValues(input$map_click$lat,input$map_click$lng,input$area)
  })
   output$rates <- renderTable({
     dataTable$data
   })
   dataMap <- reactiveValues()
   observeEvent(input$clickSide1,{
     dataMap$data <- getMapsData(latitude,longitude,16)
   })
   observeEvent(input$map_click,{
     dataMap$data <- getMapsData(input$map_click$lat,input$map_click$lng,input$map_zoom)
   })
   observeEvent(input$clickSide,{
     dataMap$data <- getMapsData(sideLatitude,sideLongitude,16)
   })
   observeEvent(input$clickMain,{
     dataMap$data <- getMapsData(latitude,longitude,16)
   })
  output$map <- renderLeaflet({
    map <- leaflet() %>% addTiles("http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google')
    dataMap$data
    map
  })
  dataProp <- reactiveValues()
  observeEvent(input$clickSide1,{
    dataProp$data <- getProp(latitude,longitude)
  })
  observeEvent(input$clickMain,{
    dataProp$data <- getProp(latitude,longitude)
  })
  observeEvent(input$clickSide,{
    dataProp$data<- getProp(sideLatitude,sideLongitude)
  })
  observeEvent(input$map_click,{
    dataProp$data <- getProp(input$map_click$lat,input$map_click$lng)
  })
  output$nearby <- renderTable({
    dataProp$data
  })
  dataTrend <- reactiveValues()
  observeEvent(input$clickSide1,{
    dataTrend$data <- getTrend(latitude,longitude,16)
  })
  observeEvent(input$clickMain2,{
    tt <- getCoordinates(input$search2)
    if(!is.null(tt)){
      if(nrow(tt)>1){
        tt <- tt[1,]
      }
      latitude <<- tt$lat
      longitude <<- tt$lng
    }
    dataTrend$data <- getTrend(latitude,longitude,16)
  })
  observeEvent(input$clickSide,{
    dataTrend$data<- getTrend(latitude,longitude,16)
  })
  observeEvent(input$trends_click,{
    dataTrend$data <- getTrend(input$trends_click$lat,input$trends_click$lng,input$trends_zoom)
  })
  output$trends <- renderLeaflet({
    trend <- leaflet() %>% addTiles("http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = 'Google') 
    dataTrend$data
    trend
  })
}

#Function to apply model on the entered lat and lon, and return table with predicted rates and prices
predictValues <- function(lat,lon,area){
  lat <- log(lat)
  lon <- log(lon)
  pois <- amenities[which.min(abs(lat-amenities$lat)^2+abs(lon-amenities$lon)^2),] #poi data of nearest point present in our Data
  if(distGeo(c(lat,lon),c(pois$lat,pois$lon)) > 2000){ # If no property is present in our data within 2Km Range
    return(as.data.frame("DATA NOT AVAILABLE"))
  }
  result <- data.frame()
  type_num <- c(2,5,8)
  types <- c("Apartment","House","Plot")
  city <- pois[,3]
  for(type in c(2,5,8)){
    test <- cbind(lat,lon,city,type,pois[,(5:68)])
    predict <- predict(model,as.matrix(test))
    temp <- cbind(types[type_num==type],predict,predict*area)
    result <- rbind(result,temp)
  }
  names(result) <- c("Type","Rate (rs/sqft)","Price")
  result[,2] <- as.numeric(as.character(result[,2]))
  result[,2] <- round(result[,2],digits = 3)
  result[,2] <- exp(result[,2])
  result[,3] <- as.numeric(as.character(result[,3]))
  result[,3] <- result[,2]*area
  result[,3] <- round(result[,3],digits = 3)
  return(result)
}

#Function to output leaflet map with markers and popups
getMapsData <- function(lat,lon,zoom){
  limit = 0.01
  url <- paste("https://searchosm.com/query.php/?","&north=",lat+limit,"&south=",lat-limit,"&west=",lon-limit,"&east=",lon+limit,"&lat=",lat,"&lon=",lon,"&output=json",sep = '')
  download_html(url,"pp.json")
  osmdf <- data.frame(name=character(),dist=numeric(),lon=numeric(),lat=numeric(),type=numeric())
  if(!file.size("pp.json")==0){
    file <- read.delim2("pp.json")
    cont <- T
    while(nrow(file)<=1){
      limit <- limit - 0.0005
      if(limit<0){
        lat = 19.113305
        lon = 72.863173
      }
      url <- url <- paste("https://searchosm.com/query.php/?","&north=",lat+limit,"&south=",lat-limit,"&west=",lon-limit,"&east=",lon+limit,"&lat=",lat,"&lon=",lon,"&output=json",sep = '')
      download_html(url,'pp.json')
      file <- read.delim2("pp.json")
    }
    osmdf1 <- fromJSON("pp.json")
    osmdf <- plyr::rbind.fill(osmdf,osmdf1)
  }
  osmdf <- select(osmdf,name,dist,lon,lat,type)
  osmdf$lat <- as.numeric(osmdf$lat)
  osmdf$lon <- as.numeric(osmdf$lon)
  if(file.exists("pp.json")){file.remove("pp.json")}
  banks <- filter(osmdf,osmdf$type=="117") %>% mutate(X=paste('<strong>Name: </strong>',
                                                              name,
                                                              '<br><strong>Distance: </strong>',
                                                              dist))
  schools <- filter(osmdf,osmdf$type=="101")%>% mutate(X=paste('<strong>Name: </strong>',
                                                               name,
                                                               '<br><strong>Distance: </strong>',
                                                               dist))
  hospitals <- filter(osmdf,osmdf$type=="160")%>% mutate(X=paste('<strong>Name: </strong>',
                                                                 name,
                                                                 '<br><strong>Distance: </strong>',
                                                                 dist))
  buses <- filter(osmdf,osmdf$type=="193")%>% mutate(X=paste('<strong>Name: </strong>',
                                                             name,
                                                             '<br><strong>Distance: </strong>',
                                                             dist))
  trains <- filter(osmdf,osmdf$type=="192")%>% mutate(X=paste('<strong>Name: </strong>',
                                                              name,
                                                              '<br><strong>Distance: </strong>',
                                                              dist))
  airports <- filter(osmdf,osmdf$type=="191")%>% mutate(X=paste('<strong>Name: </strong>',
                                                                name,
                                                                '<br><strong>Distance: </strong>',
                                                                dist))
  restaurants <- filter(osmdf,osmdf$type=="30")%>% mutate(X=paste('<strong>Name: </strong>',
                                                                  name,
                                                                  '<br><strong>Distance: </strong>',
                                                                  dist))
  hotels <- filter(osmdf,osmdf$type=="2")%>% mutate(X=paste('<strong>Name: </strong>',
                                                            name,
                                                            '<br><strong>Distance: </strong>',
                                                            dist))
  html_legend <- paste("<img src='https://searchosm.com/icons/117.png'style='width:20px;height:20px;'>Banks/ATMs - ",nrow(banks),"<br/>
      <img src='https://searchosm.com/icons/101.png'style='width:20px;height:20px;'>Schools - ",nrow(schools),"<br/>
                         <img src='https://searchosm.com/icons/160.png'style='width:20px;height:20px;'>Hospitals - ",nrow(hospitals),"<br/>
                         <img src='https://searchosm.com/icons/193.png'style='width:20px;height:20px;'>Bus Stations - ",nrow(buses),"<br/>
                         <img src='https://searchosm.com/icons/192.png'style='width:20px;height:20px;'>Train Stations - ",nrow(trains),"<br/>
                         <img src='https://searchosm.com/icons/2.png'style='width:20px;height:20px;'>Hotels - ",nrow(hotels),"<br/>
                         <img src='https://searchosm.com/icons/30.png'style='width:20px;height:20px;'>Restaurants - ",nrow(restaurants),"<br/>
                         <img src='https://searchosm.com/icons/191.png'style='width:20px;height:20px;'>Airport - ",nrow(airports))
  #Mark nearby Properties present in the Data
  temp <- properties[abs(properties$lat-lat)<0.02&abs(properties$lon-lon)<0.02,]
  temp <- subset(temp,!duplicated(temp[c("lat","lon")]))
  if(nrow(temp) > 0){
    dist <- distm(c(lon,lat),select(temp,lon,lat))
    temp <- temp[order(dist),]
  }
  nearby <- temp[(1:10),] %>% mutate(X=paste("<a href='",url,"' target='_blank'>",'<strong>Name: </strong>',
                                                              name,
                                                              '<br><strong>Address: </strong>',
                                                              address,
                                                              '<br><strong>Area: </strong>',
                                                              area,
                                                              '<br><strong>Price: </strong>',
                                                              price,"</a>"))
  prop <<- temp[(1:min(5,nrow(temp))),c(3,7,9)]
  temp1 <- salesforce[abs(salesforce$Property.Geo.Location..Latitude.-lat)<0.05&abs(salesforce$Property.Geo.Location..Longitude.-lon)<0.05,]
  temp1 <- filter(temp1,!is.na(Property.Geo.Location..Latitude.))
  temp1 <- temp1 %>%  mutate(X=paste('<strong>Salesforce </strong>',
                                     '<br><strong>Name: </strong>',
                                     Property..Property..Name,
                                     '<br><strong>Builder: </strong>',
                                     Builder.Name,
                                     '<br><strong>Rate: </strong>',
                                     Standard.Rate.Per.Sq..Ft...at.Inception.))
  new_zoom <<- 16
  if(!is.na(zoom)) new_zoom <<- zoom
  m <- leafletProxy("map",deferUntilFlush = T) %>% 
    setView(lng=lon, lat=lat , zoom=new_zoom) %>%
    clearMarkers() %>%
    addMarkers(lng = lon,lat = lat) %>%
    addMarkers(lng = banks$lon,lat = banks$lat,icon = iconList[[1]],label = if(length(lapply(banks$X,HTML))==0){""}else{lapply(banks$X,HTML)}) %>%
    addMarkers(lng = schools$lon,lat = schools$lat,icon = iconList[[2]],label = if(length(lapply(schools$X,HTML))==0){""}else{lapply(schools$X,HTML)}) %>%
    addMarkers(lng = hospitals$lon,lat = hospitals$lat,icon = iconList[[3]],label = if(length(lapply(hospitals$X,HTML))==0){""}else{lapply(hospitals$X,HTML)}) %>%
    addMarkers(lng = buses$lon,lat = buses$lat,icon = iconList[[4]],label = if(length(lapply(buses$X,HTML))==0){""}else{lapply(buses$X,HTML)}) %>%
    addMarkers(lng = trains$lon,lat = trains$lat,icon = iconList[[5]],label = if(length(lapply(trains$X,HTML))==0){""}else{lapply(trains$X,HTML)}) %>%
    addMarkers(lng = hotels$lon,lat = hotels$lat,icon = iconList[[6]],label = if(length(lapply(hotels$X,HTML))==0){""}else{lapply(hotels$X,HTML)}) %>%
    addMarkers(lng = restaurants$lon,lat = restaurants$lat,icon = iconList[[7]],label = if(length(lapply(restaurants$X,HTML))==0){""}else{lapply(restaurants$X,HTML)}) %>%
    addMarkers(lng = airports$lon,lat = airports$lat,icon = iconList[[8]],label = if(length(lapply(airports$X,HTML))==0){""}else{lapply(airports$X,HTML)}) %>%
    addMarkers(lng = nearby$lon,lat=nearby$lat,icon = iconList[[9]],popup = nearby$X) %>%
    addMarkers(lng = temp1$Property.Geo.Location..Longitude., lat = temp1$Property.Geo.Location..Latitude.,icon = iconList[[10]],label = if(length(lapply(temp1$X,HTML))==0){""}else{lapply(temp1$X,HTML)}) %>%
    addControl(html = html_legend,position = "bottomright")
  return(m)
}

#Function to return table with nearby properties
getProp <- function(lat,lon){
  prop$name <- stringr::str_remove_all(prop$name,"<U+00A0>")
  prop$source <- stringr::str_remove_all(prop$source,"<U+00A0>")
  prop$name <- as.factor(prop$name)
  prop$source <- as.factor(prop$source)
  return(prop)
}

#Functino to return map with locality polygons
getTrend <- function(lat,lon,zoom){
  new_zoom <<- 16
  if(!is.na(zoom)) new_zoom <<- zoom
  index <- which.min(abs(lat-polygons$latitude)^2+abs(lon-polygons$longitude)^2)
  mm <- leafletProxy("trends",deferUntilFlush = T) %>% 
    setView(lng=lon, lat=lat , zoom=new_zoom) %>%
    clearMarkers() %>%
    addPolygons(lng = cordi[[index]][[2]],lat = cordi[[index]][[1]],popup = polygons$X[index], weight = 1,
                color =  "#666",
                smoothFactor ="0.5",
                fillOpacity = 0) %>%
    addMarkers(lng = lon,lat = lat)
  return(mm)
}
shinyApp(ui = ui,server = server)
#shinyApp(ui = ui,server = server,options = list(host='192.168.1.8',port=5050))
