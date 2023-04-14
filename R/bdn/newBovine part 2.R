w#Libraries
library(rgdal)
library(sf)
library(dplyr)
library(sp)
library(raster)
setwd("~/R/bdn")
#Data frames
load("weights_bdn.Rdata")
load("dataMerge.rdata")


#BOVINI E SUINI INSIEME

#Calcolo delle intersezioni che cambiano nel tempo (spazio/tempo)
# Loop over each unique location
unique_days <- unique(dataMerge$Date)

for (day in unique_days) {
  subset_df <- dataMerge[dataMerge$Date == day, c("PRO_COM", "Date", "DensityBovine","DensitySwines")]

  merge <- merge(weights_bdn, subset_df, by.x = "PRO_COM", 
                 by.y = "PRO_COM", all.x= TRUE)
  #Fare una somma utilizzando la chiave idstations
  
  merge$wdBovini <- merge$weight*merge$DensityBovine
  merge$wdSuini <-  merge$weight*merge$DensitySwines
  
  partial_resultB <- aggregate( wdBovini ~ IDStations, data = merge, sum)
  partial_resultS <- aggregate( wdSuini ~ IDStations, data = merge, sum)
  
  partial_result <- merge(partial_resultB, partial_resultS, by=c("IDStations"))
  
  partial_result <- partial_result %>% mutate(Date = day)
  
  partial_result$Date <- day
  
  print(day)
  
  if(day == "2015-12-31") {
    result <- partial_result
  } else {
    result <-rbind(result, partial_result)
  }
}


write.csv(result, file="newDensity.csv", row.names = FALSE)
save(result, file="newDensity.Rdata" )


