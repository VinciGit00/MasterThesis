setwd("~/R/RasterPlot")
library(ggplot2)
AGC <- read.csv("AGC_Dataset.csv")

#Plot altitude
unique_values <- unique(AGC[, c("Latitude", "Longitude", "Altitude")])

ggplot(unique_values, aes(x = Longitude, y = Latitude, fill = Altitude)) +
  geom_tile() +
  labs(title = "Map of Altitude", x = "Longitude", y = "Latitude") 

#Plot ammonia
aggregated_data <- aggregate(cbind(EM_nh3_agr_soils, EM_nh3_agr_waste_burn, EM_nh3_livestock_mm) ~ Latitude + Longitude, data = AGC, FUN = mean)

ggplot(aggregated_data, aes(x = Longitude, y = Latitude, fill = EM_nh3_agr_soils + EM_nh3_agr_waste_burn + EM_nh3_livestock_mm)) +
  geom_tile() +
  labs(title = "Map of EM_nh3", x = "Longitude", y = "Latitude") 
