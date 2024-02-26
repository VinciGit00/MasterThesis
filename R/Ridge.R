# Load required libraries
library(glmnet)

# Set the working directory and read the data
setwd("~/Github/MasterThesis/R")
df <- read.csv("Agrimonia_scaled_Bertonico_for_interactions.csv")

# Remove rows with missing values
df <- na.omit(df)

# Select columns of interest
colonne_da_includere <- c("EM_nh3_agr_waste_burn", "EM_nox_traffic", "EM_nox_sum", "EM_so2_sum", 
                          "WE_temp_2m", "WE_wind_speed_10m_mean", "WE_blh_layer_max",
                          "WE_blh_layer_min", "WE_tot_precipitation", "WE_precipitation_t")

subset <- df[, colonne_da_includere]

# Create a matrix with predictor variables
x_matrix <- as.matrix(subset)

# Response variable
y <- df$AQ_nh3

# Perform ridge regression using glmnet
ridge_model <- cv.glmnet(x_matrix, y, alpha = 0)  # alpha = 0 for ridge regression

# Choose the best lambda based on cross-validation
best_lambda <- ridge_model$lambda.min

# Predict the response variable using the ridge model
y_pred <- predict(ridge_model, newx = x_matrix, s = best_lambda)

# Calculate residuals
residuals <- y - y_pred

# Calculate autocorrelation of residuals for ridge
autocorrelation_ridge <- acf(residuals, plot = FALSE)$acf[2]

cat("Autocorrelation of Residuals (Ridge):", autocorrelation_ridge, "\n")

# Perform lasso regression using glmnet
lasso_model <- cv.glmnet(x_matrix, y, alpha = 1)  # alpha = 1 for lasso regression

# Choose the best lambda based on cross-validation for lasso
best_lambda_lasso <- lasso_model$lambda.min

# Predict the response variable using the lasso model
y_pred_lasso <- predict(lasso_model, newx = x_matrix, s = best_lambda_lasso)

# Calculate residuals for lasso
residuals_lasso <- y - y_pred_lasso

# Calculate autocorrelation of residuals for lasso
autocorrelation_lasso <- acf(residuals_lasso, plot = FALSE)$acf[2]

cat("Autocorrelation of Residuals (Lasso):", autocorrelation_lasso, "\n")
