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
