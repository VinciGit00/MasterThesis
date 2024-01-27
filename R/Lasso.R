library(glmnet)

setwd("~/Github/MasterThesis/R")

df <- read.csv("Agrimonia_scaled_Bertonico_for_interactions.csv")

df <- na.omit(df)

matrix_correlation <- cor(df);

colonne_da_includere <- c("EM_nh3_agr_waste_burn","EM_nox_traffic", "EM_nox_sum", "EM_so2_sum", 
                          "WE_temp_2m", "WE_wind_speed_10m_mean", "WE_blh_layer_max",
                          "WE_blh_layer_min", "WE_tot_precipitation", "WE_precipitation_t");

subset <- df[, colonne_da_includere];

subset_correlation_matrix <- cor(subset);


# Trovare le coppie di variabili con correlazione in valore assoluto maggiore di 0.75
correlation_threshold <- 0.75
high_correlation_pairs <- which(abs(subset_correlation_matrix) > correlation_threshold & upper.tri(subset_correlation_matrix, diag = TRUE), arr.ind = TRUE)

# Visualizzare le coppie di variabili con correlazione maggiore di 0.75
for (row in 1:nrow(high_correlation_pairs)) {
  var1 <- colnames(subset)[high_correlation_pairs[row, 1]]
  var2 <- colnames(subset)[high_correlation_pairs[row, 2]]
  correlation_value <- subset_correlation_matrix[high_correlation_pairs[row, 1], high_correlation_pairs[row, 2]]
  cat(sprintf("Coppia di variabili: %s e %s, Correlazione: %.2f\n", var1, var2, correlation_value))
}

# Create a matrix with two columns (WE_temp_2m and AQ_no2)
x_matrix <- as.matrix(df[, c("EM_nh3_agr_waste_burn","EM_nox_traffic", "EM_nox_sum", "EM_so2_sum", 
                             "WE_temp_2m", "WE_wind_speed_10m_mean", "WE_blh_layer_max",
                             "WE_blh_layer_min", "WE_tot_precipitation", "WE_precipitation_t")])

# Response variable
y <- df$AQ_nh3
 
# Cross-validation using cv.glmnet
cv_res <- cv.glmnet(x = x_matrix, y = y, nfolds = 10)

# Plot with glmnet plot function (lambda)
plot(cv_res)

# Plot the cross-validated error curve with lambda values
plot(cv_res$lambda, cv_res$cvm, type = "b", xlab = "Lambda", ylab = "Cross-validated Error", log = "x")

# Add a vertical line for the optimal lambda
abline(v = log(cv_res$lambda.min), col = "red", lty = 2)

lambda_opt=cv_res$lambda.min 

# Fit the model using cv.glmnet with the optimal lambda
cv_res <- cv.glmnet(x = x_matrix, y = y, nfolds = 10)
best_model <- glmnet(x_matrix, y, alpha = 1, lambda = cv_res$lambda.min)

# Extracting non-zero coefficients
non_zero_coef <- coef(best_model)

# Printing variables of the best model
cat("Variables in the best model:\n")
for (i in 1:length(non_zero_coef)) {
  if (non_zero_coef[i] != 0) {
    variable_name <- colnames(x_matrix)[i]
    coefficient <- non_zero_coef[i]
    cat(sprintf("%s: %.4f\n", variable_name, coefficient))
  }
}
