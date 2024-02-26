# Imposta la directory di lavoro
setwd("~/Github/MasterThesis/R")

# Carica le librerie necessarie
library(em)
library(caret)
library(e1071)  
library(moments)  
library(glmnet)

# Leggi il file CSV
df <- read.csv("Agrimonia_scaled_Bertonico_for_interactions.csv")

# Rimuovi le righe con valori mancanti
df <- na.omit(df)

# Aggiungi colonne per le stagioni
df$Spring <- ifelse(df$Season == 1, 1, 0)
df$Summer <- ifelse(df$Season == 2, 1, 0)
df$Autumn <- ifelse(df$Season == 3, 1, 0)
df$Winter <- ifelse(df$Season == 4, 1, 0)

# Aggiungi una colonna binaria per il lockdown basata sugli indici di riga
df$During_Lockdown <- ifelse(1:nrow(df) >= 1529 & 1:nrow(df) <= 1584, 1, 0)

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

# Calcola l'autocorrelazione dei residui
residuals_autocorrelation <- acf(residuals, plot = FALSE)

# Estrai i valori di autocorrelazione
autocorrelation_values <- residuals_autocorrelation$acf

# Stampa i valori di autocorrelazione
cat("Autocorrelazione dei residui:\n")
print(autocorrelation_values)

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

# Utilizza la funzione glmnet per la regressione LASSO
# Converti il dataframe in una matrice
x <- as.matrix(all_predictors)

# Adatta il modello LASSO
lasso_model <- cv.glmnet(x, model_data$response, alpha = 1)

# Trova il valore di lambda ottimale
lambda_min <- lasso_model$lambda.min

# Seleziona le variabili con il lambda ottimale
lasso_selected_variables <- coef(lasso_model, s = lambda_min)
lasso_selected_variables <- lasso_selected_variables[-1]  # Rimuovi l'intercetta

# Ottieni i nomi delle variabili selezionate
selected_variable_names <- names(lasso_selected_variables)[lasso_selected_variables != 0]

# Stampa le variabili selezionate
cat("Variabili selezionate con LASSO regression:\n")
print(selected_variable_names)

# Crea un nuovo dataframe solo con le variabili selezionate
lasso_model_data <- model_data[, c(selected_variable_names, "response")]

# Fit del modello lineare con le sole variabili selezionate
lasso_model_fit <- lm(response ~ ., data = as.data.frame(lasso_model_data))

# Visualizza il riassunto del modello LASSO
summary(lasso_model_fit)

# Stampa le variabili selezionate con LASSO
cat("Variabili selezionate con LASSO regression:\n")
print(selected_variable_names)

print(residuals_autocorrelation$acf)
