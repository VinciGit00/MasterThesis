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

cat("Coefficients for the Best Lambda:\n")
for (i in 1:length(coefficients)) {
  cat(sprintf("%s: %.4f\n", variable_names[i], coefficients[i]))
}

# Predict the response variable using the ridge model
y_pred <- predict(ridge_model, newx = x_matrix, s = best_lambda)

# Calculate residuals
residuals <- y - y_pred

# Plot the distribution of residuals
hist(residuals, main = "Distribution of Residuals", xlab = "Residuals")

# Print summary statistics of residuals
cat("Summary Statistics of Residuals:\n")
summary(residuals)

# Add skewness, mean, standard deviation, and kurtosis calculations
cat("\nAdditional Summary Statistics of Residuals:\n")
cat(sprintf("Skewness: %.4f\n", skewness(residuals)))
cat(sprintf("Mean: %.4f\n", mean(residuals)))
cat(sprintf("Standard Deviation: %.4f\n", sd(residuals)))
cat(sprintf("Kurtosis: %.4f\n", kurtosis(residuals)))

# Plot predicted vs. real values
plot(y, y_pred, main = "Predicted vs. Real", xlab = "Real Values", ylab = "Predicted Values", pch = 16, col = "blue")
abline(0, 1, col = "red", lty = 2)

# Print summary statistics of predicted vs. real values
cat("\nSummary Statistics of Predicted vs. Real Values:\n")
summary(cbind(Real = y, Predicted = y_pred))

# Calculate the Root Mean Squared Error (RMSE)
rmse <- sqrt(mean(residuals^2))

# Print the RMSE
cat(sprintf("\nRoot Mean Squared Error (RMSE): %.4f\n", rmse))
