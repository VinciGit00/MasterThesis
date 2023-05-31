setwd("~/R/HistPlot")
load("agrimonia_v2.0.3_intern.rdata")

#Pigs 
par(mfrow = c(1, 2))
hist(agrimonia_v2.2.3_intern$LI_pigs_v2, freq = FALSE, main = "New values for pigs", ylab = "Relative Frequency", xlim = c(0, 2500))
hist(agrimonia_v2.2.3_intern$LI_pigs, freq = FALSE, main = "Old values for pigs", ylab = "Relative Frequency", ylim= c(0, 0.012))

#Bovines
par(mfrow = c(1, 2))

hist(agrimonia_v2.2.3_intern$LI_bovine_v2, freq = FALSE, main = "New values for bovines", ylab = "Relative Frequency", xlim = c(0,350), ylim = c(0, 0.030))
hist(agrimonia_v2.2.3_intern$LI_bovine, freq = FALSE, main = "New values for bovines", ylab = "Relative Frequency",)


#GGplot
library(ggplot2)

#Pigs
ggplot() + 
  geom_density(data = agrimonia_v2.2.3_intern, aes(x = as.numeric(LI_pigs_v2), fill = "Pigs new version"), 
               color = "black", alpha = 0.8, position = "identity", bw = 5)+
  scale_x_continuous(limits = c(0, 100)) + 
  scale_fill_manual( values = c("blue"))+

  theme_classic()
  
ggplot() + 
  geom_density(data = agrimonia_v2.2.3_intern, aes(x = as.numeric(LI_pigs), fill = "Pigs old version"), 
               color = "black", alpha = 0.8, position = "identity", bw = 5)+
  scale_x_continuous(limits = c(0, 100)) +
  
  theme_classic()

#Bovines
ggplot() + 
  geom_density(data = agrimonia_v2.2.3_intern, aes(x = as.numeric(LI_bovine_v2), fill = "Bovines new version"), 
               color = "black", alpha = 0.8, position = "identity", bw = 5)+
  scale_x_continuous(limits = c(0, 300)) +
  
  theme_classic()

ggplot() + 
  geom_density(data = agrimonia_v2.2.3_intern, aes(x = as.numeric(LI_bovine), fill = "Bovines old version"), 
               color = "black", alpha = 0.8, position = "identity", bw = 5)+
  scale_x_continuous(limits = c(0, 300)) +
  scale_fill_manual( values = c("blue"))+

  theme_classic()