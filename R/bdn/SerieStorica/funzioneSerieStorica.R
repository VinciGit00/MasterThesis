library(ggplot2)
library(sf)
library(cowplot)

plot_bdn <- function(IDStations){
  setwd("~/R/bdn/SerieStorica")
  load ("coordinates.Rdata")
  
  station_data <- merge_new[merge_new$IDStations == IDStations, ]
  station_data <- station_data[order(station_data$Time), ]
  
  #Old
  station_data2 <- select_util_old[select_util_old$IDStations == IDStations, ]
  station_data2 <- station_data2[order(station_data2$Time), ]
  
  # Convert the Time variable to POSIXct format
  station_data$Time  <- as.POSIXct(station_data$Time)
  station_data2$Time <- as.POSIXct(station_data2$Time)
  
  tspigs <- ggplot() +
    geom_line(data = station_data, aes(x = Time, y = LI_pigs, color = "New")) +
    geom_line(data = station_data2, aes(x = Time, y = LI_pigs, color = "Old")) +
    labs(title = paste("Station ID", IDStations),
         x = "Time",
         y = "Density swines",
         color = "Data type")
  
  tsbov <- ggplot() +
    geom_line(data = station_data, aes(x = Time, y = LI_bovine, color = "New")) +
    geom_line(data = station_data2, aes(x = Time, y = LI_bovine, color = "Old")) +
    labs(title = paste("Station ID", IDStations),
         x = "Time",
         y = "Density bovines",
         color = "Data type")
  
  mergeA <- st_transform(mergeA, st_crs(coordinates))
  
  st_buffer <- st_buffer(coordinates[coordinates$IDStations==IDStations, ], nQuadSegs = 4,  endCapStyle = 'SQUARE', dist = 0.05)
  st_buffer <- st_as_sf(st_buffer, coords =  c("Longitude", "Latitude"), crs = 4326)

  
  mapPigs <- ggplot(mergeA) +
    geom_sf(aes(fill = DensitySwines)) +
    scale_fill_continuous(type = "viridis")+
    geom_sf(data = coordinates[coordinates$IDStations==IDStations, ], col= "red")+
    geom_sf(data = st_buffer[st_buffer$IDStations==IDStations], col = "red")+
    coord_sf(xlim = c(sp_coord$Longitude[sp_coord$IDStations==IDStations]-0.3, sp_coord$Longitude[sp_coord$IDStations==IDStations]+0.3), 
                    ylim = c(sp_coord$Latitude[sp_coord$IDStations==IDStations]-0.3, sp_coord$Latitude[sp_coord$IDStations==IDStations]+0.3)
                    )
   
    
   
  mapBovine <- ggplot(mergeA) +
    geom_sf(aes(fill = DensityBovine)) +
    scale_fill_continuous(type = "viridis")+
    geom_sf(data = coordinates[coordinates$IDStations==IDStations, ], col= "red")+
    coord_sf(xlim = c(sp_coord$Longitude[sp_coord$IDStations==IDStations]-0.3, sp_coord$Longitude[sp_coord$IDStations==IDStations]+0.3), 
             ylim = c(sp_coord$Latitude[sp_coord$IDStations==IDStations]-0.3, sp_coord$Latitude[sp_coord$IDStations==IDStations]+0.3)
    )
  plot_grid(tspigs, mapPigs, tsbov, mapBovine, nrow = 2)
} 