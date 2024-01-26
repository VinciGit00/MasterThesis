library(glmnet)

setwd("~/Github/MasterThesis/R")

df <- read.csv("Agrimonia_scaled_Bertonico_for_interactions.csv")

df <- na.omit(df)

# Add columns for seasons
df$Spring <- ifelse(df$Season == "Spring", 1, 0)
df$Summer <- ifelse(df$Season == "Summer", 1, 0)
df$Autumn <- ifelse(df$Season == "Autumn", 1, 0)
df$Winter <- ifelse(df$Season == "Winter", 1, 0)

# Add a binary column for lockdown
df$During_Lockdown <- ifelse(df$Lockdown == 1, 1, 0)

