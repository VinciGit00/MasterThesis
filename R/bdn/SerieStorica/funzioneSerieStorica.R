#Library
library(ggplot2)
library(sf)
library(cowplot)

setwd("~/R/bdn/PlotTool")
#File che servono alla funzione ma che non variano (tipo MergeA) -> servono 2/3 load prima della funzione
load("mergeA.rdata")

plot_bdn <- function(IDStations){
 
  setwd("~/R/bdn/SerieStorica")
  load("st_buffer.Rdata")
  
  station_data <- merge_new[merge_new$IDStations == IDStations, ]
  station_data <- station_data[order(station_data$Time), ]
  
  #Old
  station_data2 <- select_util_old[select_util_old$IDStations == IDStations, ]
  station_data2 <- station_data2[order(station_data2$Time), ]
  
  # Convert the Time variable to POSIXct format
  station_data$Time  <- as.POSIXct(station_data$Time)
  station_data2$Time <- as.POSIXct(station_data2$Time)
  
  tspigs <- ggplot() +
    geom_line(data = station_data, aes(x = Time, y = as.numeric(LI_pigs), color = "New")) +
    geom_line(data = station_data2, aes(x = Time, y = as.numeric(LI_pigs), color = "Old")) +
    labs(title = paste("Station ID", IDStations),
         x = "Time",
         color = "Data type")+
    scale_y_continuous(name = "n/km2")
  
  tsbov <- ggplot() +
    geom_line(data = station_data, aes(x = Time, y = as.numeric(LI_bovine), color = "New")) +
    geom_line(data = station_data2, aes(x = Time, y = as.numeric(LI_bovine), color = "Old")) +
    labs(title = paste("Station ID", IDStations),
         x = "Time",
         color = "Data type")+
    scale_y_continuous(name = "n/km2")
  
  
  square <- st_buffer[st_buffer$IDStations==IDStations,]
  st_crs(square) <- st_crs(mergeA)
  
  mapPigs <- ggplot(mergeA) +
    geom_sf(aes(fill = DensitySwines)) +
    scale_fill_continuous(type = "viridis")+
    geom_sf(data = cord_sf[cord_sf$IDStations==IDStations, ], col= "red")+
    geom_sf(data = square, col = "red", fill = NA)+
    coord_sf(xlim = c(st_coordinates(cord_sf[cord_sf$IDStations==IDStations, ])[1]-0.3, st_coordinates(cord_sf[cord_sf$IDStations==IDStations, ])[1]+0.3), 
             ylim = c(st_coordinates(cord_sf[cord_sf$IDStations==IDStations, ])[2]-0.3, st_coordinates(cord_sf[cord_sf$IDStations==IDStations, ])[2]+0.3)
    )
   
  mapBovine <- ggplot(mergeA) +
    geom_sf(aes(fill = DensityBovine)) +
    scale_fill_continuous(type = "viridis")+
    geom_sf(data = cord_sf[cord_sf$IDStations==IDStations, ], col= "red")+
    geom_sf(data = square, col = "red", fill = NA)+
    coord_sf(xlim = c(st_coordinates(cord_sf[cord_sf$IDStations==IDStations, ])[1]-0.3, st_coordinates(cord_sf[cord_sf$IDStations==IDStations, ])[1]+0.3), 
             ylim = c(st_coordinates(cord_sf[cord_sf$IDStations==IDStations, ])[2]-0.3, st_coordinates(cord_sf[cord_sf$IDStations==IDStations, ])[2]+0.3)
    )
  
  plot_grid(tspigs, mapPigs, tsbov, mapBovine, nrow = 2)
} 
