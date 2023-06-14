library(ggplot2)
library(sf)
library(sp)
library(rgdal)
library(viridis)

setwd("~/R/PlotMaps")

#Read the data frames
load("AGC_Dataset_v_3_0_0.rdata")
load("agrimonia_v2.0.3_intern.rdata")
load("buf.rdata")

regions <- st_read(dsn = ".", layer = "Reg01012021_g_WGS84")

#Subset

AGC_Dataset <- subset(AGC_Dataset, format(Time, "%Y") == "2020")
agrimonia_v2.2.3_intern <- subset(agrimonia_v2.2.3_intern, format(Time, "%Y") == "2020")

regions <- st_read(dsn = ".", layer = "Reg01012021_g_WGS84")
provinces <- st_read(dsn = ".", layer = "ProvCM01012021_g_WGS84")

regions <- regions[regions$COD_REG=="3", ]
provinces <- provinces[provinces$COD_REG=="3",]


#Plot
##Temperature
meanAGC <- aggregate(WE_temp_2m ~ Latitude + Longitude, data = AGC_Dataset, FUN = mean)
meanAGC <- st_as_sf(meanAGC, coords = c("Longitude", "Latitude"), 
                            crs = 4326, agr = "constant")

regions <- st_transform(regions,st_crs(meanAGC))
meanAGC <- aggregate(WE_temp_2m ~ Latitude + Longitude, data = AGC_Dataset, FUN = mean)

png("temp.png")
ggplot() +
  geom_tile(data = meanAGC, aes(x = Longitude, y = Latitude, fill = WE_temp_2m), color="black") +
  geom_sf(data = regions, col = "black", linewidth = 1, fill = NA) +
  theme_minimal() +
  geom_sf(data = provinces, col="black", fill = NA) +
  geom_sf(data = buf, fill = NA, col = "black", size = 20) +
  scale_fill_gradient(low = "lightblue", high = "red", name="Temperature (°c)")+
  ggtitle("Mean of daily temperature of 2020")
dev.off()

##Precipitations
meanAGC <- aggregate(WE_tot_precipitation ~ Latitude + Longitude, data = AGC_Dataset, FUN = mean)
png("precipitations.png")
ggplot() +
  geom_tile(data = meanAGC, aes(x = Longitude, y = Latitude, fill = WE_tot_precipitation),  color="black") +
  geom_sf(data = regions, col = "black", linewidth = 1, fill = NA) +
  theme_minimal() +
  geom_sf(data = provinces,fill = NA, col="black") +
  geom_sf(data = buf, fill = NA, color = "black", size = 20) +
  scale_fill_gradient(low = "white", high = "blue", name="Precipitations (m)")+
  ggtitle("Mean of daily precipitations of 2020")
dev.off()


##bl
meanAGC <- aggregate(WE_blh_layer_max ~ Latitude + Longitude, data = AGC_Dataset, FUN = mean)

png("bl.png")
ggplot() +
  geom_tile(data = meanAGC, aes(x = Longitude, y = Latitude, fill = WE_blh_layer_max), color="black") +
  geom_sf(data = regions, color = "black", linewidth = 1, fill = NA) +
  theme_minimal() +
  geom_sf(data = provinces,color = "black", fill = NA) +
  geom_sf(data = buf, fill = NA, color = "black", size = 20) +
  scale_fill_gradient(low = "white", high = "orange", name="Boundary layer (m)")+
  ggtitle("Mean of daily boundary layer  of 2020")
dev.off()

##ammonia
meanAGC <- aggregate(EM_nh3_livestock_mm + EM_nh3_agr_soils + EM_nh3_agr_waste_burn ~ Latitude + Longitude,
                     data = AGC_Dataset, FUN = mean)
colnames(meanAGC) <- c("Latitude", "Longitude", "Ammonia")

AGC_Dataset$Ammonia <- AGC_Dataset$EM_nh3_livestock_mm + AGC_Dataset$EM_nh3_agr_soils + AGC_Dataset$EM_nh3_agr_waste_burn

png("ammonia.png")
ggplot() +
  geom_tile(data = meanAGC, aes(x = Longitude, y = Latitude, fill = Ammonia),  color="black") +
  geom_sf(data = regions, color = "black", linewidth = 1, fill = NA) +
  theme_minimal() +
  geom_sf(data = provinces,color = "black", fill = NA) +
  geom_sf(data = buf, fill = NA, color = "black", size = 20) +
  #scale_fill_gradient(low = "lightgreen", high = "red")
  scale_fill_continuous(type="viridis", name="Nh3 (mg/m^2)")+
  ggtitle("Mean of daily ammonia emissions of 2020")
dev.off()


##Livestock
meanAGC <- aggregate(LI_pigs ~ Latitude + Longitude, data = AGC_Dataset, FUN = mean)

png("livestock.png")
ggplot() +
  geom_tile(data = meanAGC, aes(x = Longitude, y = Latitude, fill = LI_pigs),  color="black") +
  geom_sf(data = regions, color = "black", linewidth = 1, fill = NA) +
  theme_minimal() +
  geom_sf(data = provinces,color = "black",fill = NA) +
  geom_sf(data = buf, fill = NA, color = "black", size = 20) +
  scale_fill_continuous(type="viridis",name="Livestock (Number km^(-2))")+
  ggtitle("Mean of daily livestock of 2020")
dev.off()

