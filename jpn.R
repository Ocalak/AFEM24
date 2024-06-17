# Install and load necessary packages
install.packages(c("quantreg", "dplyr", "ggplot2"))
library(quantreg)
library(dplyr)
library(ggplot2)

# Load the data
df_train <- read.csv("~/Desktop/python/dfexternal.csv")

# Convert float64 columns to float32
for (col in names(df_train)) {
  if (class(df_train[[col]]) == "numeric") {
    df_train[[col]] <- as.numeric(df_train[[col]])
  }
}

# Drop unnecessary columns
df_external <- df_train %>% select(-c(2, 3))

# Create separate dataframes for each hour
df_external_list <- list()
for (hour in 0:23) {
  df_external_list[[hour + 1]] <- df_external %>% filter(houroftheday == hour)
}

# Define quantiles for probabilistic forecast
quantiles <- c(0.05, 0.5, 0.95)  # 5th, 50th (median), and 95th percentiles

# Create a list to store models for each quantile
models <- list()

# Train a separate model for each hour and quantile
for (i in 1:24) {
  # Split the data into training and testing sets
  X <- df_external_list[[i]] %>% select(-Load_Act)
  y <- df_external_list[[i]]$Load_Act
  train_index <- sample(1:nrow(X), size = floor(0.8 * nrow(X)))
  X_train <- X[train_index, ]
  y_train <- y[train_index]
  X_test <- X[-train_index, ]
  y_test <- y[-train_index]

  # Train each model for each quantile
  for (j in 1:length(quantiles)) {
    model <- rq(Load_Act ~ ., data = X_train, tau = quantiles[j])
    models[[(i - 1) * length(quantiles) + j]] <- model
  }
}

# Make predictions for each hour and quantile
predictions <- list()
for (i in 1:24) {
  X <- df_external_list[[i]] %>% select(-Load_Act)
  for (j in 1:length(quantiles)) {
    model <- models[[(i - 1) * length(quantiles) + j]]
    predictions[[(i - 1) * length(quantiles) + j]] <- predict(model, newdata = X)
  }
}

# Reshape predictions for plotting
predictions <- matrix(unlist(predictions), nrow = 24, ncol = length(quantiles))

# Plot the probabilistic forecast for each hour
for (i in 1:24) {
  plot(predictions[i, 2, ], type = "l", col = "blue", ylim = c(min(predictions), max(predictions)),
       main = paste("Probabilistic Load Forecast for Hour", i), xlab = "Time", ylab = "Load")
  lines(predictions[i, 1, ], col = "green")
  lines(predictions[i, 3, ], col = "red")
  legend("topleft", legend = c("95th Percentile", "Median", "5th Percentile"),
         col = c("blue", "green", "red"), lty = 1)
}
