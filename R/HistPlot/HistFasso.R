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
