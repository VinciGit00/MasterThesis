setwd("~/R/bdn")
#Librerie
library(ggplot2)

#Open datasets and data frames
csv <- read.csv("Agrimonia_Dataset_v_2_0_1.csv")
subset <- csv[, c("IDStations", "Time","LI_pigs", "LI_bovine")]

load("newDensity.Rdata")

#Rename 
names(result) <- c("IDStations", "New_Bovini", "New_Suini", "Date")
names(subset) <- c("IDStations", "Date", "Old_Suini", "Old_Bovini")

#Merge
merge <- merge(result, subset, by.x = c("IDStations", "Date"), by.y =  c("IDStations", "Date"))
 
#Calcolo del valore medio
avg_dens_bovini_old <- aggregate(merge$Old_Bovini, by = list(merge$IDStations), FUN = mean)
avg_dens_bovini_new <- aggregate(merge$New_Bovini, by = list(merge$IDStations), FUN = mean)

avg_dens_suini_old <- aggregate(merge$Old_Suini, by = list(merge$IDStations), FUN = mean)
avg_dens_suini_new <- aggregate(merge$New_Suini, by = list(merge$IDStations), FUN = mean)

##Renaming
names(avg_dens_bovini_old) <- c("IDStations", "avg_bov_old")
names(avg_dens_bovini_new) <- c("IDStations", "avg_bov_new")
names(avg_dens_suini_old) <- c("IDStations", "avg_suini_old")
names(avg_dens_suini_new) <- c("IDStations", "avg_suini_new")

merge_mean <- merge(avg_dens_bovini_old, avg_dens_bovini_new, by= "IDStations")
merge_mean <- merge(merge_mean, avg_dens_suini_old, by= "IDStations")
merge_mean <- merge(merge_mean, avg_dens_suini_new, by= "IDStations")


#Plot dei suini
setwd("~/R/bdn/Plot Differenze")
# Aprire il file PDF
pdf("grafici_suini.pdf")

# Impostare le dimensioni della pagina e i margini
par(mfrow = c(5, 1), mar = c(5, 5, 2, 2), oma = c(2, 2, 2, 2))

# Generare un grafico diverso per le prime 5 IDStations
unique_ids <- unique(merge_mean$IDStations)

for (i in 1:c(1:96, 98:140)) {
  # Selezionare solo i dati per l'ID corrente
  id_data <- merge_mean[merge_mean$IDStations == unique_ids[i], ]
  
  # Selezionare solo le righe con avg_suini_old non NaN
  id_data <- id_data[complete.cases(id_data$avg_suini_old),]
  
  # Calcolare la media delle densità vecchie e nuove per l'ID corrente
  mean_data <- colMeans(id_data[, 4:5])
  
  # Creare il barplot delle densità vecchie e nuove per l'ID corrente
  barplot(mean_data, beside = TRUE, col = c("red", "blue"), 
          main = "", xlab = "", ylab = "Media", xlim = c(0, max(mean_data) * 1.2),
          horiz = TRUE, 
          # Aggiungere il titolo del grafico sopra il grafico
          sub = paste("Medie di avg_sui_old e avg_sui_new per IDStations:", unique_ids[i]),
          # Aggiungere i nomi delle legende
          legend.text = c("avg_sui_old", "avg_sui_new"), args.legend = list(y = "top", x = "right"))
  
  # Aggiungere il titolo del grafico sopra il grafico
  mtext(paste("Medie di avg_sui_old e avg_sui_new per IDStations:", unique_ids[i]), side = 3, line = -1, outer = TRUE)
}

# Chiudere il file PDF
dev.off()

#Plot dei bovini
# Selezionare solo le colonne "IDStations", "avg_bov_old" e "avg_bov_new"
setwd("~/R/bdn/Plot Differenze")
# Aprire il file PDF
pdf("grafici_bovini.pdf")

# Impostare le dimensioni della pagina e i margini
par(mfrow = c(5, 1), mar = c(5, 5, 2, 2), oma = c(2, 2, 2, 2))

# Generare un grafico diverso per le prime 5 IDStations
unique_ids <- unique(merge_mean$IDStations)

for (i in 1:c(1:96, 98:140)) {
  
  # Selezionare solo i dati per l'ID corrente
  id_data <- merge_mean[merge_mean$IDStations == unique_ids[i], ]
  
  # Calcolare la media delle densità vecchie e nuove per l'ID corrente
  mean_data <- colMeans(id_data[, 2:3])
  
  # Creare il barplot delle densità vecchie e nuove per l'ID corrente
  barplot(mean_data, beside = TRUE, col = c("red", "blue"), 
          main = "", xlab = "", ylab = "Media", xlim = c(0, max(mean_data) * 1.2),
          horiz = TRUE, 
          # Aggiungere il titolo del grafico sopra il grafico
          sub = paste("Medie di avg_bov_old e avg_bov_new per IDStations:", unique_ids[i]),
          # Aggiungere i nomi delle legende
          legend.text = c("avg_bov_old", "avg_bov_new"), args.legend = list(y = "top", x = "right"))
  
  # Aggiungere il titolo del grafico sopra il grafico
  mtext(paste("Medie di avg_bov_old e avg_bov_new per IDStations:", unique_ids[i]), side = 3, line = -1, outer = TRUE)
}

# Chiudere il file PDF
dev.off()



########################################################################################################################
#Calcolo delle differenze
diff <- data.frame(IDStations = merge_mean$IDStations, 
                   dif_bov = sqrt((merge_mean$avg_bov_old- merge_mean$avg_bov_new)^2), 
                   dif_sui = sqrt((merge_mean$avg_suini_old- merge_mean$avg_suini_new)^2))

# Selezionare la variabile di interesse
# Bovini
data <- diff[, c("IDStations", "dif_bov")]

# Definire la funzione di generazione del barplot
generate_barplot <- function(var_name) {
  barplot(data[[var_name]], names.arg = data$IDStations, main = var_name, xlab = "IDStations", ylab = "abs dif")
}

# Iterare su ogni colonna del dataset e applicare la funzione di generazione del grafico
for (col_name in names(data)) {
  if (col_name != "IDStations") {
    generate_barplot(col_name)
  }
} 
  
# Suini
data <- diff[, c("IDStations", "dif_sui")]

# Definire la funzione di generazione del barplot
generate_barplot <- function(var_name) {
  barplot(data[[var_name]], names.arg = data$IDStations, main = var_name, xlab = "IDStations", ylab = "abs dif")
}

# Iterare su ogni colonna del dataset e applicare la funzione di generazione del grafico
for (col_name in names(data)) {
  if (col_name != "IDStations") {
    generate_barplot(col_name)
  }
} 

########################################################################################################################
# Seleziono le top 20 differenze maggiori
# Ordinare il dataframe in base al valore di dif_bov in ordine decrescente
diff <- diff[order(-diff$dif_bov),]

# Selezionare le prime 20 righe (ovvero le prime 20 stazioni con il valore di dif_bov maggiore)
top_20_stazioni <- head(diff, 20)

save(top_20_stazioni, file="top20.rdata")
save(merge_mean, file="merge_mean.rdata")
