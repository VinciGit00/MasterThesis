# Imposta la directory di lavoro
setwd("~/Github/MasterThesis/R")

# Carica le librerie necessarie
library(em)
library(caret)
library(e1071)  # Questa libreria contiene la funzione skewness
library(moments)  # Questa libreria contiene la funzione kurtosis

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

# Crea un data frame con tutti i predittori
model_data <- as.data.frame(cbind(all_predictors, response))

# Fit del modello di regressione lineare con interazioni
model <- lm(response ~ .^3, data = model_data)

# Visualizza il riassunto del modello
summary(model)

# Calcola i residui del modello
residuals <- residuals(model)

# Calcola la media dei residui
residuals_mean <- mean(residuals)

# Calcola la skewness dei residui
residuals_skewness <- skewness(residuals)

# Calcola la deviazione standard dei residui
residuals_sd <- sd(residuals)

# Calcola la kurtosis dei residui
residuals_kurtosis <- kurtosis(residuals)

# Stampa i risultati
cat("Media dei residui:", residuals_mean, "\n")
cat("Skewness dei residui:", residuals_skewness, "\n")
cat("Deviazione standard dei residui:", residuals_sd, "\n")
cat("Kurtosis dei residui:", residuals_kurtosis, "\n")

# Calcola le previsioni del modello
predictions <- predict(model, newdata = model_data)

# Calcola il RMSE
rmse <- sqrt(mean((predictions - model_data$response)^2))

# Stampa il valore del RMSE
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Grafico scatterplot delle previsioni rispetto ai valori effettivi
plot(model_data$response, predictions, main = "Scatterplot previsions vs real values",
     xlab = "Real values", ylab = "Previsions", col = "blue", pch = 16)

# Grafico della distribuzione degli errori
hist(model_data$response - predictions, main = "Error distribution",
     xlab = "Errors", col = "lightblue", border = "black")

