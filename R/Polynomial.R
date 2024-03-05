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

# Fit a ridge regression model
ridge_model <- cv.glmnet(x_matrix, y, alpha = 0)  # Use alpha = 0 for ridge regression

# Select the best lambda value based on cross-validated MSE
best_lambda_ridge <- ridge_model$lambda.min

# Get the coefficients of the selected ridge model
selected_coefs_ridge <- coef(ridge_model, s = best_lambda_ridge)

# Get the indices of non-zero coefficients for ridge
non_zero_indices_ridge <- which(selected_coefs_ridge != 0)

# Extract the names of selected covariates for ridge
selected_covariates_ridge <- colnames(x_matrix)[non_zero_indices_ridge]

# Calculate the standard deviations of coefficients manually
n <- nrow(x_matrix)
residual_std_error <- sqrt(sum((predict(ridge_model, newx = x_matrix) - y)^2) / (n - length(non_zero_indices_ridge)))
coefficients_sd <- residual_std_error / sqrt(n)

# Create a data frame to store coefficients and standard deviations
coefficients_df <- data.frame(
  Variable = selected_covariates_ridge,
  Coefficient = selected_coefs_ridge[non_zero_indices_ridge],
  Standard_Deviation = rep(coefficients_sd, length(non_zero_indices_ridge))
)

# Print the table
print(coefficients_df)
