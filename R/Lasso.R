library(glmnet)

setwd("~/Github/MasterThesis/R")

df <- read.csv("Agrimonia_scaled_Bertonico_for_interactions.csv")

df <- na.omit(df)

matrix_correlation <- cor(df)

# Create a matrix with two columns (WE_temp_2m and AQ_no2)
x_matrix <- as.matrix(df[, c("WE_temp_2m", "WE_wind_speed_10m_mean", "WE_tot_precipitation", "WE_surface_pressure",
                             "WE_solar_radiation", "WE_rh_min", "WE_rh_mean", "WE_rh_max", "WE_wind_speed_100m_mean",
                             "WE_wind_speed_100m_max", "WE_blh_layer_max", "WE_blh_layer_min", "EM_nh3_livestock_mm", 
                             "EM_nh3_agr_soils", "EM_nh3_agr_waste_burn", "EM_nh3_sum", "EM_nox_traffic", "EM_nox_sum", "EM_so2_sum"   
                             )])

# Response variable
y <- df$AQ_nh3
 
# Cross-validation using cv.glmnet
cv_res <- cv.glmnet(x = x_matrix, y = y, nfolds = 10)

# Plot with glmnet plot function (lambda)
plot(cv_res)

