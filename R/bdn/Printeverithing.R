#Libraries
library(rgdal)
library(sf)
library(dplyr)
library(sp)
library(raster)
library(leaflet)
library(sf)

setwd("~/R/bdn/SerieStorica")
source("funzioneSerieStorica.R")

#Loading the shapefile
setwd("~/R/bdn")
shapename <- read_sf('Com01012021_g_WGS84.shp')

#Loading the datasets
setwd("~/R/bdn")
my_shapefile <- st_read(dsn = "Com01012021_g_WGS84.shp")

setwd("~/R/bdn/Plot Differenze")
load("top20.rdata")

setwd("~/R/bdn")
csv <- read.csv("Agrimonia_Dataset_v_2_0_1.csv")


coordinates <- unique(csv[,1:3])
coordinates(coordinates) <- c("Longitude", "Latitude")

cord_sf <- st_as_sf(coordinates)
#cord_sf <- st_as_sf(coordinates, coords =  c("Longitude", "Latitude"), crs = 4326)
save(coordinates, file = "coordinates.Rdata")
st_buffer <- st_buffer(cord_sf, nQuadSegs = 4,  endCapStyle = 'SQUARE', dist = 0.05)


st_buffer_big <- st_buffer(cord_sf, nQuadSegs = 4,  endCapStyle = 'SQUARE', dist = 0.2)


setwd("~/R/bdn")

load("newDensity.rdata")

positions <- csv[, c("IDStations", "Latitude", "Longitude")]

#merge <-merge(positions, top_20_stazioni, on = "IDStations")
merge <-positions

plot(my_shapefile$geometry)


#Old time series
#select_util_old <- merge(csv, top_20_stazioni, on = "IDStation")
select_util_old <- csv
select_util_old <- select_util_old[, c("IDStations", "Time","LI_pigs", "LI_bovine")]

#New time series
#merge_new <- merge(top_20_stazioni, result, on= "IDStations")
merge_new <- result
merge_new <- merge_new[, c("IDStations", "Date", "wdSuini", "wdBovini")]

merge_new <- rename(merge_new, Time = Date, LI_pigs = wdSuini, LI_bovine = wdBovini)
setwd("~/R/bdn/PlotTool")
save(merge_new, file= "Merge_new.rdata")
save(select_util_old, file="old_utils.rdata")
#Merge

load("dataMerge.rdata")
dataMerge <- dataMerge[dataMerge$Date == "2020-12-31",]
mergeA <- merge(my_shapefile, dataMerge, by= "PRO_COM" )

#Plot dei suini
setwd("~/R/bdn/SerieStorica")
pdf("best_stations_Swine.pdf")

# Create a vector of unique station IDs
stations <- unique(merge_new$IDStations)

mergeA <- st_transform(mergeA, 4236)
save(mergeA, file ="mergeA.Rdata")
st_crs(cord_sf) <- st_crs(mergeA)

setwd("~/R/bdn/PlotTool")
save(cord_sf, file= "cord_sf.rdata")
library(ggplot2)
library(units)

# Create a PDF file device
pdf("FinalComparison.pdf", width = 11, height = 8.5,  compress = 9)

# Loop through the stations
for (i in seq_along(stations)) {
  #print(stations[i])
  
  print(plot_bdn(stations[i]))
}

# Close the PDF file device
dev.off()

