setwd("~/Desktop")
load("newDataset.rdata")

load("Agrimonia_Dataset_v_2_0_2.rdata")

par(mfrow=c(1,2))
hist(Agrimonia_Dataset$LI_bovine, main = "Old dataset", xlab = "LI_bovine")

# Create the second histogram
hist(agrimonia_new$LI_bovine, main = "New dataset", xlab = "LI_bovine", )


mean(Agrimonia_Dataset$LI_bovine, na.rm=TRUE)
mean(agrimonia_new$LI_bovine, na.rm=TRUE)

mean(Agrimonia_Dataset$LI_pigs, na.rm=TRUE)
mean(agrimonia_new$LI_pigs, na.rm=TRUE)

par(mfrow=c(1,2))
hist(Agrimonia_Dataset$LI_pigs, main = "Old dataset", xlab = "LI_pigs")
hist(agrimonia_new$LI_pigs, , main = "New dataset", xlab = "LI_pigs")
