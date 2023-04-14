library(dplyr)

setwd("~/R/bdn")

load("newDensity.rdata")

setwd("~/R/bdn/New dataset")

load("Agrimonia_Dataset_v_2_0_2.Rdata")


Agrimonia_Dataset <- subset(Agrimonia_Dataset, select = -c(LI_pigs,LI_bovine))
  
colnames(result)[2] ="LI_bovine"
colnames(result)[3] ="LI_pigs"

#Merge  
agrimonia_new <- merge(x=Agrimonia_Dataset, y=result, by.x=c("IDStations","Time"), by.y=c("IDStations", "Date"), all.x = TRUE)

save(agrimonia_new, file = "newDataset.RData")
write.csv(agrimonia_new, "newDataset.csv", row.names=FALSE)
