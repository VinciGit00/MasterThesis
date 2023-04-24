setwd("~/R/bdn/MergeAdd column")
load("Agrimonia_Dataset_v_2_0_2.Rdata")
load("newDataset.RData")

justfour <- agrimonia_new[,c("IDStations", "Time", "LI_pigs", "LI_bovine")]
names(justfour)[3] = "LI_pigs_v2"
names(justfour)[4] = "LI_bovine_v2"

agrimonia_v2.2.3_intern <- merge(justfour, Agrimonia_Dataset, by = c("IDStations","Time"))

save(agrimonia_v2.2.3_intern, file="agrimonia_v2.2.3_intern.rdata")
write.csv(agrimonia_v2.2.3_intern, "agrimonia_v2.2.3_intern.csv", row.names=FALSE)
