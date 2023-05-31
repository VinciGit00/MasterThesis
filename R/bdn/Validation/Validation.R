# Load the required packages
library(dplyr)
library(sp)
library(sf)

# Set the working directory
setwd("~/R/bdn/Validation")

# Read the data from the CSV file
csv <- read.csv('AGC_Dataset.csv')

# Select the columns of interest and remove any rows with missing values
interest <- csv[, c("Latitude", "Longitude", "Time", "LI_pigs", "LI_bovine")]
interest <- na.omit(interest)

# Convert the LI_pigs and LI_bovine columns to numeric
interest$LI_pigs <- as.numeric(interest$LI_pigs)
interest$LI_bovine <- as.numeric(interest$LI_bovine)

# Compute the mean LI values for each unique location
df_means <- interest %>%
  group_by(Latitude, Longitude) %>%
  summarize(
    mean_LI_pigs = mean(LI_pigs),
    mean_LI_bovine = mean(LI_bovine)
  )

# Select the location with the highest combined mean LI value
max_row <- df_means[which.max(df_means$mean_LI_pigs + df_means$mean_LI_bovine), c("Longitude", "Latitude", "mean_LI_pigs", "mean_LI_bovine")]
max <- interest[interest$Latitude == max_row$Latitude & interest$Longitude == max_row$Longitude,]

# Find the date with the highest LI_bovine value for the selected location
max_day <- max$Time[which.max(max$LI_bovine)]

# Get unique values of Latitude and Longitude
unique_data <- distinct(interest, Latitude, Longitude)

# Create a buffer around the unique data
unique_data_sf <- st_as_sf(unique_data, coords = c("Longitude", "Latitude"), crs = 4326)
st_buffer <- st_union(unique_data_sf)
st_buffer <- st_buffer(st_buffer, dist = c(0.04, 0.22), nQuadSegs = 4, endCapStyle = 'SQUARE')

# extract the geometry and coordinate information
coords <- st_coordinates(st_buffer)
geometry <- st_buffer$geometry

unique_data$buffer <- st_buffer$geometry

unique_data <- st_as_sf(unique_data, coords = c("Longitude", "Latitude"), crs = 4326)


for (i in 1:nrow(st_buffer)) {
  # Get the values in the current row
  row_values <- st_buffer[i, ]
  
  area_buffer <- st_area(row_values)/1000000
  
  #Calculate the intersection
  intersection <- st_intersection(row_values, unique_data$buffer)
  
  area <- st_area(intersection)/1000000
  
  weigth <- area/area_buffer
  
  df <- data.frame(cbind(intersection[,c(1,7)]))
  
  df$weigth <- round(weigth, 5 )
  
  df <- df[, -3]
  
  if(i==1) {
    all_df <- df
  } else {
    all_df<-rbind(all_df,df)
  }
  
}
