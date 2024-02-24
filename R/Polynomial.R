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

# Fit a polynomial regression model
degree <- 2  # You can change this to the desired degree of the polynomial
poly_model <- lm(y ~ poly(x_matrix, degree, raw = TRUE), data = df)

# Perform Lasso regression
lasso_model <- cv.glmnet(x_matrix, y, alpha = 1)  # Use alpha = 1 for Lasso regression

# Plot the cross-validated mean squared error (MSE) against lambda
plot(lasso_model)

# Select the best lambda value based on cross-validated MSE
best_lambda <- lasso_model$lambda.min

# Print the best lambda value
cat(sprintf("\nBest lambda value: %.4f\n", best_lambda))

# Get the coefficients of the selected model
selected_coefs <- coef(lasso_model, s = best_lambda)

# Print the coefficients
print(selected_coefs)

# Get the indices of non-zero coefficients
non_zero_indices <- which(selected_coefs != 0)

# Extract the names of selected covariates
selected_covariates <- colnames(x_matrix)[non_zero_indices]

# Print the selected covariates and their coefficients
cat("\nSelected covariates and their coefficients:\n")
for (i in non_zero_indices) {
  coef_value <- selected_coefs[i]
  covariate_name <- colnames(x_matrix)[i]
  cat(sprintf("%s: %.4f\n", covariate_name, coef_value))
}

# Print the names of parameters included in Lasso model
cat("\nNames of parameters included in Lasso model:\n")
print(selected_covariates)

# Make predictions using the selected covariates
x_selected <- x_matrix[, non_zero_indices-1]
lasso_predictions <- predict(poly_model, newdata = as.data.frame(x_selected))

# Calculate residuals
lasso_residuals <- y - lasso_predictions

# Calculate Root Mean Squared Error (RMSE) for Lasso model
lasso_rmse <- sqrt(mean(lasso_residuals^2))

# Print the RMSE for Lasso model
cat(sprintf("\nRoot Mean Squared Error (RMSE) for Lasso model: %.4f\n", lasso_rmse))

# Print all the parameters of the Lasso model
cat("\nParameters of the Lasso model:\n")
print(lasso_model)
