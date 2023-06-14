setwd("~/R/HistPlot")
library(ggplot2)

AGC <- read.csv("AGC_Dataset.csv")

load("agrimonia_v2.0.3_intern.rdata")
load("AGC_Dataset_v_3_0_0.rdata")
agrimonia_v2.2 <- agrimonia_v2.2.3_intern

#Temperatura
ggplot() + 
  geom_density(data = AGC_Dataset, aes(x = as.numeric(WE_temp_2m), fill = "AGC_Dataset"), 
               color = "black", alpha = 0.35, position = "identity", bw = 5) +
  geom_density(data = agrimonia_v2.2, aes(x = as.numeric(WE_temp_2m), fill = "agrimonia_v2.2"), 
               color = "black", alpha = 0.35, position = "identity", bw = 5) + 
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Relative frequency distribution of temperature", 
       x = "Temperature (°C)", y = "Frequency") + 
  scale_x_continuous(limits = c(-40, 40)) +
  scale_fill_manual(values = c("AGC_Dataset" = alpha("blue", 0.5), "agrimonia_v2.2" = alpha("yellow", 0.5)), 
                    name = "Temperature",
                    labels = c("AGC_Dataset", "Agrimonia dataset")) +
  theme_classic()


#Precipitations
png(file="Precipitations.png")
ggplot() + 
  geom_density(data = AGC_Dataset, aes(x = as.numeric(WE_tot_precipitation), fill = "AGC_Dataset"), 
               color = "black", alpha = 0.35, position = "identity") +
  geom_density(data = agrimonia_v2.2, aes(x = as.numeric(WE_tot_precipitation), fill = "Agrimonia dataset"), 
               color = "black", alpha = 0.35, position = "identity") + 
   
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Relative frequency distribution of WE_tot_precipitation", 
       x = "Precipitation (m)", y = "Frequency") + 
  scale_x_continuous(limits = c(0, 0.02), trans="sqrt") +
  scale_fill_manual(values = c("AGC_Dataset" = alpha("blue", 0.5), "Agrimonia dataset" = alpha("yellow", 0.5)), 
                    name = "Density",
                    breaks = c("AGC_Dataset", "Agrimonia dataset"),
                    labels = c("AGC_Dataset", "Agrimonia dataset")) +
  theme_classic()
dev.off()


#WE_rh_mean
png(file="rh.png")
ggplot() + 
  geom_density(data = agrimonia_v2.2, aes(x = as.numeric(WE_rh_mean), fill = "Agrimonia dataset"), 
               color = "black", alpha = 0.35, position = "identity") + 
  geom_density(data = AGC, aes(x = as.numeric(WE_rh_mean), fill = "AGC"), 
               color = "black", alpha = 0.35, position = "identity") + 
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Relative frequency distribution of WE_rh_mean", 
       x = "Mean rh", y = "Frequency") + 
  scale_fill_manual(values = c("Agrimonia dataset" = "blue", "AGC" = "yellow"), 
                    name = "Density",
                    breaks = c("Agrimonia dataset", "AGC"),
                    labels = c("Agrimonia dataset", "AGC")) +
  scale_x_continuous(limits = c(75, 105)) +
  theme_classic()
dev.off()


png(file=".png")

ggplot() + 
  geom_density(data = agrimonia_v2.2, aes(x = as.numeric(WE_rh_mean), fill = "Agrimonia dataset"), 
               color = "black", alpha = 0.35, position = "identity") + 
  
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Relative frequency distribution of WE_tot_precipitation", 
       x = "Precipitation  (m)", y = "Frequenza") + 
  scale_x_continuous(limits = c(0, 0.02)) +
  scale_fill_manual(values = c("Agrimonia dataset" = alpha("blue", 0.5), "AGC_Dataset" = alpha("yellow", 0.5)), 
                    name = "Density") +
  theme_classic()
dev.off()

#WE_blh_layer_max
png(file="Weather.png")
ggplot() + 
  geom_density(data = AGC_Dataset, aes(x = as.numeric(WE_blh_layer_max), fill = "AGC_Dataset"), 
               color = "black", alpha = 0.35, position = "identity") + 
  geom_density(data = agrimonia_v2.2, aes(x = as.numeric(WE_blh_layer_max), fill = "Agrimonia dataset"), 
               color = "black", alpha = 0.35, position = "identity") + 
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Relative frequency distribution of WE_blh_layer_max", 
       x = "WE_blh_layer_max (m)", y = "Frequency") + 
  scale_fill_manual(values = c("Agrimonia dataset" = "yellow", "AGC_Dataset" = "blue"), 
                    name = "Density") +
  theme_classic()

dev.off()


#NH3
nh3AGC <- data.frame(NH3 = AGC_Dataset$EM_nh3_livestock_mm + AGC_Dataset$EM_nh3_agr_soils + AGC_Dataset$EM_nh3_agr_waste_burn)
NH3Agrimonia <- data.frame(NH3 = agrimonia_v2.2$EM_nh3_livestock_mm + agrimonia_v2.2$EM_nh3_agr_soils + agrimonia_v2.2$EM_nh3_agr_waste_burn)
png(file="NH3.png")
ggplot() + 
  geom_density(data = as.data.frame(nh3AGC), aes(x = as.numeric(NH3), fill = "AGC_Dataset"), 
               color = "black", alpha = 0.35, position = "identity") + 
  geom_density(data = as.data.frame(NH3Agrimonia), aes(x = as.numeric(NH3), fill = "Agrimonia Dataset"), 
               color = "black", alpha = 0.35, position = "identity") + 
  
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Relative frequency distribution of NH3", 
       x = "NH3", y = "Frequency") + 
  scale_fill_manual(values = c("AGC_Dataset" = "blue", "Agrimonia Dataset" = "yellow"), 
                    name = "Density") +
  scale_x_continuous(limits = c(0, 60)) +
  theme_classic()


dev.off()


#Li
png(file="Li.png")
ggplot() + 
  geom_density(data = AGC_Dataset, aes(x = as.numeric(LI_pigs), fill = "AGC_Dataset"), 
               color = "black", alpha = 0.35, position = "identity") + 
  geom_density(data = agrimonia_v2.2, aes(x = as.numeric(LI_pigs), fill = "Agrimonia Dataset"), 
               color = "black", alpha = 0.35, position = "identity") + 
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Relative frequency distribution of LI_pigs", 
       x = "LI_pigs (Number km^(-2))", y = "Frequency") + 
  scale_fill_manual(values = c("Agrimonia Dataset" = "yellow", "AGC_Dataset" = "blue"), 
                    name = "Density") +
  scale_x_continuous(limits = c(0, 800), trans = "sqrt") +
  theme_classic()

dev.off()
