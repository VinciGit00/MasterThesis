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
model <- lm(y ~ poly(x_matrix, degree, raw = TRUE), data = df)

# Make predictions for the original data
predictions <- predict(model, newdata = df)

# Calculate residuals
residuals <- df$AQ_nh3 - predictions

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean(residuals^2))

# Print the RMSE
cat(sprintf("\nRoot Mean Squared Error (RMSE): %.4f\n", rmse))

# Plot the time series
par(mfrow = c(2, 1))  # Set up a 2x1 grid for two plots

# Plot actual values
plot(df$AQ_nh3, type = "l", col = "blue", lwd = 2, main = "Actual vs Predicted Time Series",
     xlab = "Index", ylab = "AQ_nh3")
lines(predictions, col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), lwd = 2)

# Plot residuals
plot(residuals, type = "l", col = "green", lwd = 2, main = "Residuals", xlab = "Index", ylab = "Residuals")

# Add a horizontal line at y = 0
abline(h = 0, col = "black", lty = 2)

# Reset the plotting layout
par(mfrow = c(1, 1))

# Print the model summary to see coefficients
summary(model)
