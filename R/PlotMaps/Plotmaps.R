library(ggplot2)
library(sf)
library(sp)
library(rgdal)

setwd("~/R/PlotMaps")

#Opening the dataframe
load("AGC_Dataset_v_3_0_0.rdata")
load("agrimonia_v2.0.3_intern.rdata")

##Opening the shapefiles
regions <- st_read(dsn = ".", layer = "Reg01012021_g_WGS84")
provinces <- st_read(dsn = ".", layer = "ProvCM01012021_g_WGS84")


regions <- regions[regions$COD_REG=="3", ]
provinces <- provinces[provinces$COD_REG=="3",]

#AGC
AGC_coordinates <- unique(AGC_Dataset[, c('Latitude', 'Longitude')])

AGC_coordinates <- st_as_sf(AGC_coordinates, coords = c("Longitude", "Latitude"), 
                            crs = 4326, agr = "constant")
#Agrimonia
Agrimonia_coordinates <- unique(agrimonia_v2.2.3_intern[, c('Latitude', 'Longitude')])

Agrimonia_coordinates <- st_as_sf(Agrimonia_coordinates, coords = c("Longitude", "Latitude"), 
                                  crs = 4326, agr = "constant")

##Add coordinates system
regions <- st_transform(regions,st_crs(AGC_coordinates))
provinces <- st_transform(provinces, st_crs(AGC_coordinates)) 

load("buf.rdata")

#AGC
AGC_coordinates <- unique(AGC_Dataset[, c('Latitude', 'Longitude')])

ggplot() +
  geom_sf(data = regions, color = "black", linewidth = 1) +
  geom_sf(data = provinces,) +
  geom_sf(data = buf, fill = NA, color = "red", size = 20) +
  geom_tile(data = AGC_coordinates, aes(x = Longitude, y = Latitude), fill = NA, col = "black") +
  theme_minimal()



#Agrimonia
Agrimonia_coordinates <- unique(agrimonia_v2.2.3_intern[, c('Latitude', 'Longitude')])

ggplot() +
  geom_sf(data = regions, color = "black", linewidth = 1) +
  geom_sf(data= provinces)+
  geom_sf(data = buf, fill = NA, color = "red", size = 20) +
  geom_point(data = Agrimonia_coordinates, aes(x = Longitude, y = Latitude), fill = "white", col = "blue") +
  theme_minimal()

#ERA5
#Single level
load("Daily Single Levels 2016_2021 Lombardy.rdata")

SingleLevels <- unique(SingleLevels[, 1:2])

ggplot() +
  geom_sf(data = regions, color = "black", linewidth = 1) +
  geom_sf(data= provinces)+
  geom_tile(data= SingleLevels, aes(x = Lon, y = Lat),fill=NA, color="blue")+
  geom_sf(data = buf, fill = NA, color = "red", size = 20) +
  ggtitle("ERA5-Land grid")+
  theme_minimal()
  
#Daily
load("Daily Land 2016_2021 Lombardy.rdata")
Land <- unique(Land[, 1:2])

ggplot() +
  geom_sf(data = regions, color = "black", linewidth = 1) +
  geom_sf(data= provinces)+
  geom_tile(data= Land, aes(x = Lon, y = Lat), fill=NA, color="red")+
  geom_sf(data = buf, fill = NA, color = "red", size = 20) +
  ggtitle("ERA5 single level grid")+
  theme_minimal()

#Mized
ggplot() +
  #geom_sf(data = regions, color = "black", linewidth = 1) +
  #geom_sf(data= provinces)+
  geom_point(data= Land, aes(x = round(Lon,2), y = round(Lat,2)),  fill=NA, color="red")+
  geom_point(data= SingleLevels, aes(x = round(Lon,2), y = round(Lat,2)),  fill=NA, color="blue")+
  #geom_sf(data = buf, fill = NA, color = "red", size = 20) +
  theme_minimal()

#Plot 
comunis <- st_read(dsn = ".", layer = "Com01012016_g_WGS84")
AGC_coordinates <- st_as_sf(AGC_coordinates, coords = c("Longitude", "Latitude"), 
                            crs = 4326, agr = "constant")

comunis <- st_transform(comunis, st_crs(4326)) 
ggplot() +
  geom_sf(data= comunis)+
  geom_sf(data = regions, color = "black", fill=NA, linewidth = 1) +
  geom_sf(data = buf, fill = NA, color = "red", size = 20)+
  coord_sf(xlim=c(8, 11.9), ylim=c(44.3, 47))+
  theme_minimal()

grid_data <- expand.grid(x = seq(9.985, 10.205, 0.01), y = seq(45.445, 45.715, 0.01))
grid_data <- st_as_sf(grid_data, coords = c("x", "y"), 
                      crs = 4326, agr = "constant")

comunis <- subset(comunis, COD_REG==3)
comunis <- subset(comunis, COD_PROV==17)
grid_data_2 <- st_intersection(grid_data, comunis)

# Plot con ggplot
ggplot() +
  geom_sf(data = comunis, aes(fill= as.factor(PRO_COM)), alpha = 0.2) +
  geom_sf(data = grid_data_2, aes(col=as.factor(PRO_COM))) +
  geom_sf(data=provinces, col = "orange", fill=NA)+
  geom_tile(data = Land, aes(x = Lon, y = Lat), fill = NA, color = "blue", linewidth=1) +
  geom_sf(data = regions, color = "black", fill = NA, linewidth = 1) +
  geom_sf(data = buf, fill = NA, color = "red", size = 20) +
  coord_sf(xlim = c(10, 10.1), ylim = c(45.5, 45.6)) +
  theme_minimal()+
  theme(legend.position = "none")
