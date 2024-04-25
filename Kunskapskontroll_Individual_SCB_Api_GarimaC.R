# Load libraries
library(pxweb)
library(tidyverse)
library(tidymodels)

# Install packages 
 install.packages("tidyverse")
 install.packages("pxweb")

# Data preparation
# PXWEB query
pxweb_query_list <- list("EkoIndikator"=c("HAH10","HAH20","HAH30"),
                         "ContentsCode"=c("HA0201A7"),
                         "Tid"=c("2022M07","2022M08","2022M09","2022M10","2022M11","2022M12","2023M01","2023M02","2023M03","2023M04","2023M05","2023M06","2023M07","2023M08","2023M09","2023M10","2023M11","2023M12","2024M01","2024M02"))

# Download data
px_data <- pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/en/ssd/HA/HA0201/HA0201S/SnabbStatHA0201H",
                     query = pxweb_query_list)

# Convert to data.frame
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Subset the data for the desired columns
px_subset <- px_data_frame %>% 
  filter(`economic indicator` == "Net trade of goods") %>%
  select(month, `Past 12 month`)

# Convert 'month' column to proper date format
px_subset$month <- as.Date(paste0(px_subset$month, "01"), format = "%YM%m%d")

# Rename columns for easier access
names(px_subset) <- c("Month", "Net_Trade")

# Linear model
# Create a simple linear regression model
lm_model <- lm(Net_Trade ~ Month, data = px_subset)

# Summary of the regression
summary(lm_model)

# Prediction
# Predicted trade for February 2025
feb_2025 <- as.numeric(as.Date("2025-02-01"))
predicted_trade <- coef(lm_model)["(Intercept)"] + coef(lm_model)["Month"] * feb_2025
print(predicted_trade)

# Plot the data points and the regression line
plot(px_subset$Month, px_subset$Net_Trade, main = "Net Trade Over Time", xlab = "Month", ylab = "Net Trade")
abline(lm_model, col = "red")

# Residual Analysis
# Plot residuals against fitted values
plot(lm_model$fitted.values, lm_model$residuals, 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values")

# Q-Q Plot
qqnorm(lm_model$residuals)
qqline(lm_model$residuals)

# Variable Transformation
# Log transformation of Net_Trade
px_subset$log_Net_Trade <- log(px_subset$Net_Trade)

# Refit the model with transformed variable
lm_log <- lm(log_Net_Trade ~ Month, data = px_subset)

# Check model summary and diagnostics
summary(lm_log)

# Model comparison
# Fit alternative models
lm_poly <- lm(Net_Trade ~ poly(Month, 2), data = px_subset)

# Compare models using AIC or BIC
AIC(lm_model, lm_poly)
BIC(lm_model, lm_poly)

# Forecasting
# Forecasting future values using linear regression model
future_months <- seq(max(px_subset$Month) + 1, length = 12, by = "month")
forecast_data <- data.frame(Month = future_months)
forecast_data$Forecast <- predict(lm_model, newdata = forecast_data)

# Plot forecasted values
plot(px_subset$Month, px_subset$Net_Trade, type = "l", xlab = "Month", ylab = "Net Trade",
     main = "Net Trade Forecast", ylim = range(px_subset$Net_Trade, forecast_data$Forecast))
lines(forecast_data$Month, forecast_data$Forecast, col = "red")

# Cross-validation
# Remove rows with missing values
px_subset_clean <- na.omit(px_subset)

# Define the training control
ctrl <- trainControl(method = "cv",    # Cross-validation method (e.g., "cv" for k-fold CV)
                     number = 5)       # Number of folds for cross-validation

# Train the model using cross-validation
lm_cv <- train(log_Net_Trade ~ Month,     # Model formula
               data = px_subset_clean,    # Cleaned dataset
               method = "lm",             # Modeling method (linear regression)
               trControl = ctrl)          # Training control

# View the cross-validation results
print(lm_cv)

# Inference
# p-values for testing the null hypothesis that the beta coefficients are equal to 0
summary(lm_model)

# Confidence interval for parameters
confint(lm_model)

# Confidence intervals and prediction intervals for predicted values
# Interval for the average net trade
predict(lm_model, data.frame(Month = c(as.Date("2025-02-01"))), interval = "confidence")

# Interval for an individual's net trade
predict(lm_model, data.frame(Month = c(as.Date("2025-02-01"))), interval = "prediction")