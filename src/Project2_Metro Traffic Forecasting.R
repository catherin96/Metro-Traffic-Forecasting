# -----------------------------------
# Metro Traffic Forecasting Project
# -----------------------------------

# Install required libraries
required_libraries <- c(
  "fpp3", "tidyverse", "stringr", "readr", "readxl", 
  "dplyr", "tsibble", "ggplot2", "forecast", "TSA", 
  "seasonal", "fable", "fabletools", "stats"
)

for (lib in required_libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib, dependencies = TRUE)
  }
}

# Load libraries
library(fpp3)
library(tidyverse)
library(stringr)
library(readr)
library(readxl)
library(dplyr)
library(tsibble)
library(ggplot2)
library(forecast)
library(TSA)
library(seasonal)
library(fable)
library(fabletools)
library(stats)

# Load the dataset
Traffic_Volume <- read.csv("Metro_Interstate_Traffic_Volume.csv")
head(Traffic_Volume)

# Data preprocessing
# Split the "date_time" column into separate "date" and "time" columns
New_Traffic_data <- tidyr::separate(Traffic_Volume, date_time, c("date", "time"), sep = " ")

# Split the "date" column into "year," "month," and "day"
New_Traffic_data <- tidyr::separate(New_Traffic_data, date, c("year", "month", "day"), sep = "-")

# Group traffic volume data by month and calculate the total volume per month
Traffic_by_Month <- New_Traffic_data %>%
  group_by(year, month) %>%
  summarise(volume_by_month = sum(traffic_volume))

# Create a time series object for traffic volume
Traffic_obj <- ts(as.vector(Traffic_by_Month$volume_by_month), start = 2012, end = 2018, frequency = 12)

# Plot the time series
plot(Traffic_obj, ylab = "Traffic Volume", type = "o", main = "Time Series Plot for Monthly Interstate Traffic Volume")

# Analyze autocorrelation (ACF plot)
acf(Traffic_by_Month$volume_by_month, lag.max = 70, main = "ACF Plot for Traffic Volume by Month")

# Narrow down the lag to 12 and observe patterns in the data
acf(Traffic_by_Month$volume_by_month, lag.max = 12, main = "ACF Plot for Traffic Volume by Month")

# Time series decomposition
Traffic_dcmp <- ts(Traffic_by_Month$volume_by_month, start = c(2012, 10), end = c(2018, 9), frequency = 12)

# Perform and plot multiplicative decomposition
multiplicative_decomposition <- decompose(Traffic_dcmp, "multiplicative")
plot(multiplicative_decomposition, type = "o")

# Perform and plot additive decomposition
additive_decomposition <- decompose(Traffic_dcmp, "additive")
plot(additive_decomposition, type = "o")

# Perform and plot STL decomposition
s_window_value <- 13  # Adjustable parameter
stl_traffic <- stl(Traffic_dcmp, s.window = "periodic", t.window = s_window_value)
plot(stl_traffic, main = "STL Decomposition of Traffic Data")

# Evaluate normality of the traffic volume data
hist(Traffic_obj, col = "violet", main = "Histogram of Monthly Interstate Traffic Volume")

# Apply Box-Cox transformation and compare
lambda <- BoxCox.lambda(Traffic_obj)
Traffic_transformation <- (Traffic_obj^lambda - 1) / lambda
par(mfrow = c(1, 2))
plot(Traffic_obj, ylab = "Original Series", main = "Original Data")
plot(Traffic_transformation, ylab = "Transformed Series", main = "Transformed Data")
hist(Traffic_transformation, col = "seagreen", main = "Histogram of Box-Cox Transformed Data")

# Forecasting evaluations
# Drift method forecasting
Traffic_drift_forecasting <- forecast(ets(Traffic_dcmp, model = "AAN"), h = 24)
print(Traffic_drift_forecasting)

# Naive forecasting
Traffic_naive_forecasting <- forecast::naive(Traffic_dcmp, h = 24)
print(Traffic_naive_forecasting)

# Compare accuracy using MAE
MAE_drift <- mean(abs(Traffic_drift_forecasting$mean - mean(Traffic_dcmp[(length(Traffic_dcmp) - 12):length(Traffic_dcmp)])))
MAE_naive <- mean(abs(Traffic_naive_forecasting$mean - mean(Traffic_dcmp[(length(Traffic_dcmp) - 12):length(Traffic_dcmp)])))
print(paste("MAE for Drift Forecasting:", MAE_drift))
print(paste("MAE for Naive Forecasting:", MAE_naive))

# Plot residuals
plot(residuals(Traffic_drift_forecasting), main = "Drift Forecast Residuals")
plot(residuals(Traffic_naive_forecasting), main = "Naive Forecast Residuals")

# Visualize forecasts using ggplot
traffic_date <- seq(as.Date("2018-10-01"), by = "months", length.out = 24)
forecast_data <- data.frame(
  DATE = traffic_date,
  Naive_Forecast = Traffic_naive_forecasting$mean,
  Drift_Forecast = Traffic_drift_forecasting$mean
)

Traffic_by_Month$month_date <- as.Date(paste(Traffic_by_Month$year, Traffic_by_Month$month, "01", sep = "-"))

ggplot(Traffic_by_Month, aes(x = month_date, y = volume_by_month)) +
  geom_line() +
  geom_line(data = forecast_data, aes(x = DATE, y = Naive_Forecast), color = "orange", linetype = "dashed") +
  geom_line(data = forecast_data, aes(x = DATE, y = Drift_Forecast), color = "brown", linetype = "dashed") +
  labs(x = "Month", y = "Traffic Volume", title = "Traffic Volume Forecast") +
  theme_minimal()


