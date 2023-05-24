setwd("~/R/HistPlot")
library(ggplot2)

AGC <- read.csv("AGC_Dataset.csv")

load("agrimonia_v2.0.3_intern.rdata")
load("AGC_Dataset_v_3_0_0.rdata")
agrimonia_v2.2 <- agrimonia_v2.2.3_intern

#Temperatura
ggplot() + 
  geom_density(data = AGC_Dataset, aes(x = as.numeric(WE_temp_2m), fill = "AGC_Dataset"), 
               color = "black", alpha = 0.8, position = "identity", bw = 5) +
  geom_density(data = agrimonia_v2.2, aes(x = as.numeric(WE_temp_2m), fill = "agrimonia_v2.2"), 
               color = "black", alpha = 0.8, position = "identity", bw = 5) + 
  geom_vline(xintercept = mean(as.numeric(agrimonia_v2.2$WE_temp_2m)), 
             color = "black", linetype = "dashed") +
  geom_vline(xintercept = mean(as.numeric(AGC_Dataset$WE_temp_2m)), 
             color = "red", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Histogram of WE_temp_2m", 
       x = "Temperature (°C)", y = "Frequency") + 
  scale_x_continuous(limits = c(-40, 40)) +
  scale_fill_manual(values = c("AGC_Dataset" = alpha("yellow", 0.5), "agrimonia_v2.2" = alpha("red", 0.5)), 
                    name = "Density",
                    breaks = c("AGC_Dataset", "agrimonia_v2.2"),
                    labels = c("AGC_Dataset", "agrimonia_v2.2")) +
  theme_classic()



#Precipitations
ggplot() + 
  geom_density(data = agrimonia_v2.2, aes(x = as.numeric(WE_tot_precipitation), fill = "agrimonia_v2.2"), 
               color = "black", alpha = 0.8, position = "identity") + 
  geom_density(data = AGC_Dataset, aes(x = as.numeric(WE_tot_precipitation), fill = "AGC_Dataset"), 
               color = "black", alpha = 0.8, position = "identity") + 
  geom_vline(xintercept = mean(as.numeric(agrimonia_v2.2$WE_tot_precipitation), na.rm = TRUE), 
             color = "blue", linetype = "dashed") +
  geom_vline(xintercept = mean(as.numeric(AGC_Dataset$WE_tot_precipitation), na.rm = TRUE), 
             color = "red", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Histogram of WE_tot_precipitation", 
       x = "Precipitation", y = "Frequency") + 
  scale_x_continuous(limits = c(0, 0.02)) +
  scale_fill_manual(values = c("AGC_Dataset" = alpha("yellow", 0.5), "agrimonia_v2.2" = alpha("red", 0.5)), 
                    name = "Density",
                    breaks = c("AGC_Dataset", "agrimonia_v2.2"),
                    labels = c("AGC_Dataset", "agrimonia_v2.2")) +
  theme_classic()


#WE_rh_mean
ggplot() + 
  geom_density(data = agrimonia_v2.2, aes(x = as.numeric(WE_rh_mean), fill = "agrimonia_v2.2.2"), 
               color = "black", alpha = 0.8, position = "identity") + 
  geom_density(data = AGC, aes(x = as.numeric(WE_rh_mean), fill = "AGC"), 
               color = "black", alpha = 0.8, position = "identity") + 
  geom_vline(xintercept = mean(agrimonia_v2.2$WE_rh_mean), 
             color = "blue", linetype = "dashed") +
  geom_vline(xintercept = mean(AGC$WE_rh_mean), 
             color = "red", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Histogram of WE_rh_mean", 
       x = "Mean rh", y = "Frequency") + 
  scale_fill_manual(values = c("agrimonia_v2.2.2" = "red", "AGC" = "yellow"), 
                    name = "Density",
                    breaks = c("agrimonia_v2.2.2", "AGC"),
                    labels = c("agrimonia_v2.2.2", "AGC")) +
  scale_x_continuous(limits = c(75, 105)) +
  theme_classic()



ggplot() + 
  geom_density(data = agrimonia_v2.2, aes(x = as.numeric(WE_rh_mean), fill = "agrimonia_v2.2"), 
               color = "black", alpha = 0.8, position = "identity") + 
  geom_density(data = AGC_Dataset, aes(x = as.numeric(WE_rh_mean), fill = "AGC_Dataset"), 
               color = "black", alpha = 0.8, position = "identity") + 
  geom_vline(xintercept = mean(as.numeric(agrimonia_v2.2$WE_rh_mean), na.rm = TRUE), 
             color = "blue", linetype = "dashed") +
  geom_vline(xintercept = mean(as.numeric(AGC_Dataset$WE_rh_mean), na.rm = TRUE), 
             color = "red", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Histogram of WE_tot_precipitation", 
       x = "Precipitazioni", y = "Frequenza") + 
  scale_x_continuous(limits = c(0, 0.02)) +
  scale_fill_manual(values = c("agrimonia_v2.2" = alpha("blue", 0.5), "AGC_Dataset" = alpha("red", 0.5)), 
                    name = "Density") +
  theme_classic()

#WE_blh_layer_max
ggplot() + 
  geom_density(data = agrimonia_v2.2, aes(x = as.numeric(WE_blh_layer_max), fill = "agrimonia_v2.2.2"), 
               color = "black", alpha = 0.8, position = "identity") + 
  geom_density(data = AGC_Dataset, aes(x = as.numeric(WE_blh_layer_max), fill = "AGC"), 
               color = "black", alpha = 0.8, position = "identity") + 
  geom_vline(xintercept = mean(agrimonia_v2.2$WE_blh_layer_max), 
             color = "blue", linetype = "dashed") +
  geom_vline(xintercept = mean(AGC_Dataset$WE_blh_layer_max), 
             color = "red", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Histogram of WE_blh_layer_max", 
       x = "WE_blh_layer_max", y = "Frequency") + 
  scale_fill_manual(values = c("agrimonia_v2.2.2" = "red", "AGC" = "yellow"), 
                    name = "Density") +
  scale_x_continuous(limits = c(75, 110)) +
  theme_classic()


#NH3
nh3AGC <- data.frame(NH3 = AGC_Dataset$EM_nh3_livestock_mm + AGC_Dataset$EM_nh3_agr_soils + AGC_Dataset$EM_nh3_agr_waste_burn)
NH3Agrimonia <- data.frame(NH3 = agrimonia_v2.2$EM_nh3_livestock_mm + agrimonia_v2.2$EM_nh3_agr_soils + agrimonia_v2.2$EM_nh3_agr_waste_burn)

ggplot() + 
  geom_density(data = as.data.frame(nh3AGC), aes(x = as.numeric(NH3), fill = "nh3AGC"), 
               color = "black", alpha = 0.8, position = "identity") + 
  geom_density(data = as.data.frame(NH3Agrimonia), aes(x = as.numeric(NH3), fill = "NH3Agrimonia"), 
               color = "black", alpha = 0.8, position = "identity") + 
  geom_vline(xintercept = mean(nh3AGC$NH3), 
             color = "blue", linetype = "dashed") +
  geom_vline(xintercept = mean(NH3Agrimonia$NH3), 
             color = "red", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Histogram of NH3", 
       x = "NH3", y = "Frequency") + 
  scale_fill_manual(values = c("nh3AGC" = "yellow", "NH3Agrimonia" = "red"), 
                    name = "Density") +
  scale_x_continuous(limits = c(0, 60)) +
  theme_classic()
