# Imposta la directory di lavoro
setwd("~/Github/MasterThesis/R")

# Leggi il file CSV
df <- read.csv("Agrimonia_scaled_Bertonico_for_interactions.csv")

# Rimuovi le righe con valori mancanti
df <- na.omit(df)

# Aggiungi colonne per le stagioni
df$Spring <- ifelse(df$Season == "Spring", 1, 0)
df$Summer <- ifelse(df$Season == "Summer", 1, 0)
df$Autumn <- ifelse(df$Season == "Autumn", 1, 0)
df$Winter <- ifelse(df$Season == "Winter", 1, 0)

# Aggiungi una colonna binaria per il lockdown basata su date specifiche
df$During_Lockdown <- ifelse(df$Lockdown >= as.Date("2020-03-09") & df$Lockdown <= as.Date("2020-05-03"), 1, 0)

# Estrai le colonne rilevanti per il modello
predictors <- df[, c("Spring", "Summer", "Autumn", "Winter", "During_Lockdown")]

# Variabili legate al meteo
WE_variables <- c("WE_temp_2m", "WE_wind_speed_10m_mean", "WE_wind_speed_10m_max", "WE_tot_precipitation", "WE_precipitation_t",
                  "WE_surface_pressure", "WE_solar_radiation", "WE_rh_min", "WE_rh_mean", "WE_rh_max", "WE_wind_speed_100m_mean", 
                  "WE_wind_speed_100m_max", "WE_blh_layer_max", "WE_blh_layer_min")

# Combina predittori e variabili meteorologiche
all_predictors <- cbind(predictors, df[, WE_variables])

# Risposta (variabile dipendente)
response <- df$AQ_nh3

# Numero di bootstrap (k)
k <- 5

# Lunghezza del dataset
n <- nrow(df)

# Inizializza un vettore per memorizzare i risultati del RMSE
rmse_results <- numeric(k)

# Loop per eseguire il bootstrap
for (i in 1:k) {
  # Crea un campione bootstrap
  index_bootstrap <- sample(1:n, replace = TRUE)
  df_bootstrap <- df[index_bootstrap, ]
  
  # Crea un data frame con tutti i predittori
  model_data <- as.data.frame(cbind(all_predictors, response))
  
  # Fit del modello di regressione lineare con interazioni
  model <- lm(response ~ .^3, data = model_data)
  
  # Calcola le previsioni del modello
  predictions <- predict(model, newdata = model_data)
  
  # Calcola il RMSE
  rmse_results[i] <- sqrt(mean((predictions - model_data$response)^2))
}

# Stampa la media del RMSE sui bootstrap
cat("Media del Root Mean Squared Error (RMSE) sui bootstrap:", mean(rmse_results), "\n")

# Grafico della distribuzione dei RMSE sui bootstrap
hist(rmse_results, main = "Bootstrap RMSE distribution",
     xlab = "RMSE", col = "lightblue", border = "black")
