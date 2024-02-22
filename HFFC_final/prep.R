
#Makaan Buy Data Scrapeing
try(
  r<-  source('Data/Buy/Makaan/Prep1.R',echo=T),
  r<-  source('Data/Buy/Makaan/rerun.R', echo=TRUE),
  r<-  source('Data/Buy/Makaan/numprice.R', echo=TRUE),
  r<-  source('Data/Buy/Makaan/combine.R', echo=TRUE),
  r<-  source('Data/Buy/Makaan/trim.R', echo=TRUE),
  r<-  source('Data/Buy/Makaan/type bhk seperation.R', echo=TRUE)
  #Makaan_combined.csv
  )

#99Acres Buy Data Scrapeing
try(
  r<-  source('Data/Buy/99Acres/Prep1.R',echo=T),
  r<-  source('Data/Buy/99Acres/rerun.R',echo=T),
  r<-  source('Data/Buy/99Acres/combine.R',echo=T)
  #99Acres_combined.csv
)

#Quikr  Buy Data Scrapeing
try(
  write(0,"Data/page.txt"),
  r<-  source('Data/Buy/quikr homes/citiesconstants.R', echo=TRUE),
  r<-  source('Data/Buy/quikr homes/rerun with proxies.R', echo=TRUE),
  r<-  source('Data/Buy/quikr homes/combine.R',echo=T)
  #quikr_combined.csv
)

#Proptiger Buy Data Scrapeing
try(
  write(0,"Data/page.txt"),
  r<-  source('Data/Buy/PropTiger/Proptiger_projects.R', echo=TRUE),
  r<-  source('Data/Buy/PropTiger/PropTiger_properties.R', echo=TRUE),
  r<-  source('Data/Buy/PropTiger/combine.R',echo=T)
  #proptiger.csv
)
#Sulekha Buy Data Scrapeing
try(
  write(0,"Data/page.txt"),
  r<-  source('Data/Buy/sulekha/Sulekha.R', echo=TRUE),
  r<-  source('Data/Buy/sulekha/combine.R',echo=T)
  #sulekha.csv
)
#Magic Bricks Buy Data Scrapeing
try(
  write(0,"Data/page.txt"),
  r<-  source('Data/Buy/magicbricks/MagicBricks.R', echo=TRUE)
  #MagicBricks.csv
)
#Commonfloor Buy Data Scrapeing
try(
  write(0,"Data/page.txt"),
  r<-  source('Data/Buy/commonfloor/commonfloor.R', echo=TRUE),
  r<-  source('Data/Buy/commonfloor/combine.R', echo=TRUE),
  r<-  source('Data/Buy/commonfloor/final3.R',echo=T)
  #commonFloor1.csv
)



#Makaan Rent Properties

try(
  r<-  source('Data/Rent/prep1.R', echo=TRUE),
  r<-  source('Data/Rent/rerun Makaan Rent.R', echo=TRUE)
  
)


#Nestaway Rent Properties
try(
  r<-  source('Data/Rent/nestaway/nestaway.R', echo=TRUE)
  #combined.csv
)

# Magic bricks Rent Properties
try(
  r<-  source('Data/Rent/magicbricks/prep1.R', echo=TRUE)
  #Magicbricks_rent.csv
)

#combine Buy Properties

try(
  r<-  source('Data/combiningproperties2.R', echo=TRUE)
  #combined_prop_all1.csv
)



#Amenities Data Scrapeing
try(
  r<-  source('Data/POI/prep.R',echo=T),
  r<-  source('Data/POI/rerun.R', echo=TRUE)
#propertywiseamen.csv  
)  

#Model
try(
  r<-  source('Data/model_final.R', echo=TRUE)
)
#model_final
#amenities.csv

#combine properties for App

try(
  r<-  source('Data/combiningproperties3.R', echo=TRUE),
  r<-  source('Data/combined.R', echo=TRUE)
#combined.csv
)


#Amenities data for App
#will come from model file


