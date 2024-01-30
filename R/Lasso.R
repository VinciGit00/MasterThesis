# Load required libraries
library(glmnet)
library(e1071)

# Set the working directory and read the data
setwd("~/Github/MasterThesis/R")

# Read data
df <- read.csv("Agrimonia_scaled_Bertonico_for_interactions.csv")

# Remove rows with missing values
df <- na.omit(df)

# Select columns of interest
colonne_da_includere <- c("EM_nh3_agr_waste_burn", "EM_nox_traffic", "EM_nox_sum", "EM_so2_sum",
                          "WE_temp_2m", "WE_wind_speed_10m_mean", "WE_blh_layer_max",
                          "WE_blh_layer_min", "WE_tot_precipitation", "WE_precipitation_t")
subset <- df[, colonne_da_includere]

# Calculate correlation matrix
subset_correlation_matrix <- cor(subset)

# Find variable pairs with correlation greater than 0.75
correlation_threshold <- 0.75
high_correlation_pairs <- which(abs(subset_correlation_matrix) > correlation_threshold & upper.tri(subset_correlation_matrix, diag = TRUE), arr.ind = TRUE)

# Display variable pairs with high correlation
for (row in 1:nrow(high_correlation_pairs)) {
  var1 <- colnames(subset)[high_correlation_pairs[row, 1]]
  var2 <- colnames(subset)[high_correlation_pairs[row, 2]]
  correlation_value <- subset_correlation_matrix[high_correlation_pairs[row, 1], high_correlation_pairs[row, 2]]
  cat(sprintf("Coppia di variabili: %s e %s, Correlazione: %.2f\n", var1, var2, correlation_value))
}

# Create predictor matrix (x_matrix) and response variable (y)
x_matrix <- as.matrix(df[, colonne_da_includere])
y <- df$AQ_nh3

# Cross-validation using cv.glmnet
cv_res <- cv.glmnet(x = x_matrix, y = y, nfolds = 10)

# Plot cross-validated error curve with lambda values
plot(cv_res$lambda, cv_res$cvm, type = "b", xlab = "Lambda", ylab = "Cross-validated Error", log = "x")

# Add a vertical line for the optimal lambda
abline(v = log(cv_res$lambda.min), col = "red", lty = 2)

# Fit the model using cv.glmnet with the optimal lambda
best_model <- glmnet(x_matrix, y, alpha = 1, lambda = cv_res$lambda.min)

# Extract non-zero coefficients
non_zero_coef <- coef(best_model)

# Print variables of the best model
cat("Variables in the best model:\n")
for (i in 1:length(non_zero_coef)) {
  if (non_zero_coef[i] != 0) {
    variable_name <- colnames(x_matrix)[i]
    coefficient <- non_zero_coef[i]
    cat(sprintf("%s: %.4f\n", variable_name, coefficient))
  }
}

# Use the fitted model to make predictions on the training data
predictions <- predict(best_model, newx = x_matrix, s = cv_res$lambda.min, type = "response")

# Calculate residuals
residuals <- y - predictions

# Plot the distribution of residuals
hist(residuals, main = "Error Distribution", xlab = "Residuals", col = "lightblue", border = "black")

# Print summary statistics of residuals
cat("Summary Statistics of Residuals:\n")
cat("Mean:", mean(residuals), "\n")
cat("Standard Deviation:", sd(residuals), "\n")
cat("Skewness:", e1071::skewness(residuals), "\n")
cat("Kurtosis:", e1071::kurtosis(residuals), "\n")

# Calculate the Root Mean Squared Error (RMSE)
rmse <- sqrt(mean(residuals^2))

# Print the RMSE
cat(sprintf("\nRoot Mean Squared Error (RMSE): %.4f\n", rmse))
