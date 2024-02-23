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

# Plot the cross-validated mean squared error as a function of lambda
plot(ridge_model)

# Choose the best lambda based on cross-validation
best_lambda <- ridge_model$lambda.min

# Print the coefficients and corresponding variable names for the best lambda
coefficients <- coef(ridge_model, s = best_lambda)
variable_names <- colnames(x_matrix)

cat("Coefficients for the Best Lambda (Ridge):\n")
for (i in 1:length(coefficients)) {
  cat(sprintf("%s: %.4f\n", variable_names[i], coefficients[i]))
}

# Predict the response variable using the ridge model
y_pred <- predict(ridge_model, newx = x_matrix, s = best_lambda)

# Calculate residuals
residuals <- y - y_pred

# Plot the distribution of residuals
hist(residuals, main = "Distribution of Residuals (Ridge)", xlab = "Residuals")

# Print summary statistics of residuals (Ridge)
cat("Summary Statistics of Residuals (Ridge):\n")
summary(residuals)

# Additional summary statistics of residuals (Ridge)
#cat("\nAdditional Summary Statistics of Residuals (Ridge):\n")
#cat(sprintf("Skewness: %.4f\n", skewness(residuals)))
#cat(sprintf("Mean: %.4f\n", mean(residuals)))
#cat(sprintf("Standard Deviation: %.4f\n", sd(residuals)))
#cat(sprintf("Kurtosis: %.4f\n", kurtosis(residuals)))

# Plot predicted vs. real values for ridge
plot(y, y_pred, main = "Predicted vs. Real (Ridge)", xlab = "Real Values", ylab = "Predicted Values", pch = 16, col = "blue")
abline(0, 1, col = "red", lty = 2)

# Print summary statistics of predicted vs. real values (Ridge)
cat("\nSummary Statistics of Predicted vs. Real Values (Ridge):\n")
summary(cbind(Real = y, Predicted = y_pred))

# Calculate the Root Mean Squared Error (RMSE) for ridge
rmse <- sqrt(mean(residuals^2))

# Print the RMSE for ridge
cat(sprintf("\nRoot Mean Squared Error (RMSE) for Ridge: %.4f\n", rmse))

# Perform lasso regression using glmnet
lasso_model <- cv.glmnet(x_matrix, y, alpha = 1)  # alpha = 1 for lasso regression

# Plot the cross-validated mean squared error as a function of lambda
plot(lasso_model)

# Choose the best lambda based on cross-validation for lasso
best_lambda_lasso <- lasso_model$lambda.min

# Print the coefficients and corresponding variable names for the best lambda for lasso
coefficients_lasso <- coef(lasso_model, s = best_lambda_lasso)
variable_names <- colnames(x_matrix)

cat("\nCoefficients for the Best Lambda (Lasso):\n")
for (i in 1:length(coefficients_lasso)) {
  cat(sprintf("%s: %.4f\n", variable_names[i], coefficients_lasso[i]))
}

# Predict the response variable using the lasso model
y_pred_lasso <- predict(lasso_model, newx = x_matrix, s = best_lambda_lasso)

# Calculate residuals for lasso
residuals_lasso <- y - y_pred_lasso

# Plot the distribution of residuals for lasso
hist(residuals_lasso, main = "Distribution of Residuals (Lasso)", xlab = "Residuals")

# Print summary statistics of residuals for lasso
cat("Summary Statistics of Residuals (Lasso):\n")
summary(residuals_lasso)

# Additional summary statistics of residuals for lasso
#cat("\nAdditional Summary Statistics of Residuals (Lasso):\n")
#cat(sprintf("Skewness: %.4f\n", skewness(residuals_lasso)))
#cat(sprintf("Mean: %.4f\n", mean(residuals_lasso)))
#cat(sprintf("Standard Deviation: %.4f\n", sd(residuals_lasso)))
#cat(sprintf("Kurtosis: %.4f\n", kurtosis(residuals_lasso)))

# Plot predicted vs. real values for lasso
plot(y, y_pred_lasso, main = "Predicted vs. Real (Lasso)", xlab = "Real Values", ylab = "Predicted Values", pch = 16, col = "blue")
abline(0, 1, col = "red", lty = 2)

# Print summary statistics of predicted vs. real values for lasso
cat("\nSummary Statistics of Predicted vs. Real Values (Lasso):\n")
summary(cbind(Real = y, Predicted = y_pred_lasso))

# Calculate the Root Mean Squared Error (RMSE) for lasso
rmse_lasso <- sqrt(mean(residuals_lasso^2))

# Print the RMSE for lasso
cat(sprintf("\nRoot Mean Squared Error (RMSE) for Lasso: %.4f\n", rmse_lasso))

# Find non-zero coefficients and corresponding variable names for lasso
non_zero_indices <- which(coefficients_lasso != 0)
non_zero_coefficients <- coefficients_lasso[non_zero_indices]
non_zero_variable_names <- variable_names[non_zero_indices]

cat("\nNon-zero Coefficients and Corresponding Variable Names for the Best Lambda (Lasso):\n")
for (i in 1:length(non_zero_coefficients)) {
  cat(sprintf("%s: %.4f\n", non_zero_variable_names[i], non_zero_coefficients[i]))
}

