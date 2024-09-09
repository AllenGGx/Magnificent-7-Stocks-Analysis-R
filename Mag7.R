####################################################
##The Magnificent 7: Strategic Insights and Market Analysis of Leading Tech Stocks
## Forecasting & Predicting the Future Using Data  DAT-3531 - VNA1
####################################################
####################################################
## Author: Allen Guico
## aguico@student.hult.edu
## Hult International Business School
####################################################


# Install necessary libraries if not already installed
install.packages(c("dplyr", "lubridate", "ggplot2", "forecast", "rugarch", "rpart", "caret", "psych", "plotly", "heatmaply", "tidyr", "rpart.plot", "viridis"))

# Load libraries for data manipulation and visualization
library(dplyr)     # Data manipulation
library(lubridate) # Date manipulation
library(ggplot2)   # Data visualization
library(plotly)    # Interactive plots
library(heatmaply) # Correlation heatmap

# Load libraries for time series forecasting
library(forecast)  # ARIMA models

# Load libraries for advanced models
library(rugarch)   # GARCH models
library(rpart)     # Decision trees

# Load libraries for model evaluation
library(caret)     # Model evaluation and cross-validation

# Load libraries for advanced analytical techniques
library(psych)     # Principal Component Analysis (PCA)

# Load tidyr for data reshaping
library(tidyr)

# Load rpart.plot for decision tree visualization
library(rpart.plot)

# Load viridis for color schemes
library(viridis)

# Define the path to the datasets
path <- "~/Downloads"

# Load the datasets
aapl_data <- read.csv(file.path(path, "AAPL1424.csv"))
amzn_data <- read.csv(file.path(path, "AMZN1424.csv"))
goog_data <- read.csv(file.path(path, "GOOGL1424.csv"))
meta_data <- read.csv(file.path(path, "META1424.csv"))
msft_data <- read.csv(file.path(path, "MSFT1424.csv"))
nvda_data <- read.csv(file.path(path, "NVDA1424.csv"))
tsla_data <- read.csv(file.path(path, "TSLA1424.csv"))

# Function to inspect datasets
inspect_dataset <- function(df, name) {
  cat("Inspecting", name, "\n")
  print(str(df))
  print(head(df))
  print(names(df))
  cat("\n\n")
}

# Inspect each dataset
inspect_dataset(aapl_data, "AAPL")
inspect_dataset(amzn_data, "AMZN")
inspect_dataset(goog_data, "GOOGL")
inspect_dataset(meta_data, "META")
inspect_dataset(msft_data, "MSFT")
inspect_dataset(nvda_data, "NVDA")
inspect_dataset(tsla_data, "TSLA")

# Function to format and rename columns
format_and_rename <- function(df, name) {
  # Check if df is a data frame
  if (!is.data.frame(df)) {
    stop("Input is not a data frame")
  }
  
  # Ensure the date column is named consistently
  if("Date" %in% names(df)) {
    df$date <- as.Date(df$Date, format = "%Y-%m-%d")
    df <- df %>% select(-Date)
  } else if("date" %in% names(df)) {
    df$date <- as.Date(df$date, format = "%Y-%m-%d")
  } else {
    stop("Date column not found in the dataset")
  }
  
  # Ensure column names are lowercase for consistency
  names(df) <- tolower(names(df))
  
  # Add a name column to identify the dataset
  df$name <- name
  return(df)
}

# Format and rename columns in each dataset
aapl_data <- format_and_rename(aapl_data, "AAPL")
amzn_data <- format_and_rename(amzn_data, "AMZN")
goog_data <- format_and_rename(goog_data, "GOOGL")
meta_data <- format_and_rename(meta_data, "META")
msft_data <- format_and_rename(msft_data, "MSFT")
nvda_data <- format_and_rename(nvda_data, "NVDA")
tsla_data <- format_and_rename(tsla_data, "TSLA")

# Combine all datasets into one dataframe
sp500_stocks <- bind_rows(aapl_data, amzn_data, goog_data, meta_data, msft_data, nvda_data, tsla_data)

# Inspect the combined dataset
print("First few rows of the dataset:")
print(head(sp500_stocks))

# Summary statistics of the dataset
print("Summary statistics of the dataset:")
print(summary(sp500_stocks))

# Structure of the dataset
print("Structure of the dataset:")
print(str(sp500_stocks))

# Check for missing values in each column
print("Missing values in each column:")
missing_values <- sapply(sp500_stocks, function(x) sum(is.na(x)))
print(missing_values)

# Remove rows with missing values
sp500_stocks <- na.omit(sp500_stocks)

# Check unique values in 'name' column
print("Unique values in 'name' column:")
tech_list <- unique(sp500_stocks$name)
print(tech_list)

# Function to plot closing prices of all stocks with improved color and labels
plt_closing_prices <- function() {
  p <- plotly::subplot(
    lapply(tech_list, function(company) {
      df <- sp500_stocks[sp500_stocks$name == company, ]
      plot_ly(df, x = ~date, y = ~close, name = company, type = 'scatter', mode = 'lines', line = list(color = sample(viridis(100), 1))) %>%
        layout(title = paste("Closing Prices Over Time"), 
               xaxis = list(title = "Date"),
               yaxis = list(title = "Closing Price"),
               plot_bgcolor = 'white',
               paper_bgcolor = 'white',
               font = list(color = 'black'))
    }),
    nrows = 3, shareX = TRUE, shareY = TRUE
  )
  p
}

# Plot closing prices
print(plt_closing_prices())

# Function to plot total volume of stock being traded each day with improved color and labels
plt_volume <- function() {
  p <- plotly::subplot(
    lapply(tech_list, function(company) {
      df <- sp500_stocks[sp500_stocks$name == company, ]
      plot_ly(df, x = ~date, y = ~volume, name = company, type = 'scatter', mode = 'lines', line = list(color = sample(viridis(100), 1))) %>%
        layout(title = paste("Total Volume Traded Each Day"), 
               xaxis = list(title = "Date"),
               yaxis = list(title = "Volume"),
               plot_bgcolor = 'white',
               paper_bgcolor = 'white',
               font = list(color = 'black'))
    }),
    nrows = 3, shareX = TRUE, shareY = TRUE
  )
  p
}
print(plt_volume())

# Normalize stock prices and volumes
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

sp500_stocks_normalized <- sp500_stocks %>%
  mutate(across(c(open, close, high, low, volume), normalize))

# Calculate daily percentage return for each stock
sp500_stocks <- sp500_stocks %>%
  group_by(name) %>%
  mutate(daily_return = (close - open) / open * 100)

# Function to plot daily percentage return for each stock with improved color and labels
plt_daily_return <- function() {
  p <- plotly::subplot(
    lapply(tech_list, function(company) {
      df <- sp500_stocks[sp500_stocks$name == company, ]
      plot_ly(df, x = ~date, y = ~daily_return, name = company, type = 'scatter', mode = 'lines', line = list(color = sample(viridis(100), 1))) %>%
        layout(title = paste("Daily Percentage Return"), 
               xaxis = list(title = "Date"),
               yaxis = list(title = "Daily Return (%)"),
               plot_bgcolor = 'white',
               paper_bgcolor = 'white',
               font = list(color = 'black'))
    }),
    nrows = 3, shareX = TRUE, shareY = TRUE
  )
  p
}
print(plt_daily_return())

# Analyze monthly mean of closing prices
sp500_stocks <- sp500_stocks %>%
  mutate(month = floor_date(date, "month"))

monthly_mean_close <- sp500_stocks %>%
  group_by(name, month) %>%
  summarise(monthly_mean_close = mean(close))

# Function to plot monthly mean of closing prices with improved color and labels
plt_monthly_mean <- function() {
  p <- plotly::subplot(
    lapply(tech_list, function(company) {
      df <- monthly_mean_close[monthly_mean_close$name == company, ]
      plot_ly(df, x = ~month, y = ~monthly_mean_close, name = company, type = 'scatter', mode = 'lines', line = list(color = sample(viridis(100), 1))) %>%
        layout(title = paste("Monthly Mean Closing Prices"), 
               xaxis = list(title = "Month"),
               yaxis = list(title = "Monthly Mean Close"),
               plot_bgcolor = 'white',
               paper_bgcolor = 'white',
               font = list(color = 'black'))
    }),
    nrows = 3, shareX = TRUE, shareY = TRUE
  )
  p
}
print(plt_monthly_mean())

# Check correlation between stock prices of tech companies
tech_data <- sp500_stocks %>%
  filter(name %in% tech_list) %>%
  select(date, name, close) %>%
  pivot_wider(names_from = name, values_from = close)

correlation_matrix <- cor(tech_data %>% select(-date), use = "complete.obs")

# Function to plot correlation heatmap of closing prices
plt_correlation_heatmap <- function() {
  heatmaply::heatmaply(correlation_matrix, 
                       main = "Correlation Matrix of Major Tech Companies' Closing Prices",
                       xlab = "Company", ylab = "Company",
                       colors = viridis::viridis(256),
                       plot_method = "plotly",
                       margins = c(60, 100, 40, 20),
                       plot_bgcolor = 'white',
                       paper_bgcolor = 'white',
                       font = list(color = 'black'))
}
print(plt_correlation_heatmap())

# Analyze daily return correlation
daily_return_data <- sp500_stocks %>%
  filter(name %in% tech_list) %>%
  select(date, name, daily_return) %>%
  pivot_wider(names_from = name, values_from = daily_return)

daily_return_correlation_matrix <- cor(daily_return_data %>% select(-date), use = "complete.obs")

# Function to plot correlation heatmap of daily returns
plt_daily_return_correlation <- function() {
  heatmaply::heatmaply(daily_return_correlation_matrix, 
                       main = "Correlation Matrix of Major Tech Companies' Daily Returns",
                       xlab = "Company", ylab = "Company",
                       colors = viridis::viridis(256),
                       plot_method = "plotly",
                       margins = c(60, 100, 40, 20),
                       plot_bgcolor = 'white',
                       paper_bgcolor = 'white',
                       font = list(color = 'black'))
}
print(plt_daily_return_correlation())

# Decision Tree Model
# Create a simple decision tree model to predict whether the closing price will go up or down
sp500_stocks <- sp500_stocks %>%
  mutate(target = ifelse(close > open, "Up", "Down"))

# Split the data into training and test sets (70-30 split)
set.seed(123)  # For reproducibility
train_indices <- sample(seq_len(nrow(sp500_stocks)), size = 0.7 * nrow(sp500_stocks))
train_data <- sp500_stocks[train_indices, ]
test_data <- sp500_stocks[-train_indices, ]

# Train a decision tree model
tree_model <- rpart(target ~ open + high + low + volume, data = train_data, method = "class")

# Plot the decision tree
rpart.plot(tree_model, main = "Decision Tree for Predicting Stock Price Movement", box.palette = "RdYlGn", shadow.col = "gray", nn = TRUE)

# Predict on the test set
predictions <- predict(tree_model, test_data, type = "class")

# Evaluate the model
confusion_matrix <- table(test_data$target, predictions)
print("Confusion Matrix:")
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# Function to plot decision tree
plt_decision_tree <- function() {
  rpart.plot(tree_model, main = "Decision Tree for Predicting Stock Price Movement", box.palette = "RdYlGn", shadow.col = "gray", nn = TRUE)
}

# Display decision tree plot
print(plt_decision_tree())

# Save the combined dataset to a CSV file
write.csv(sp500_stocks, file.path(path, "sp500_tech_stocks_combined.csv"), row.names = FALSE)

# Forecasting top 5 performing S&P 500 stocks in next year using ARIMA and GARCH models
# Select top 5 stocks based on the average closing price over the dataset period
top_5_stocks <- sp500_stocks %>%
  group_by(name) %>%
  summarise(avg_close = mean(close)) %>%
  arrange(desc(avg_close)) %>%
  top_n(5) %>%
  pull(name)

# Function to forecast using ARIMA and GARCH models and plot results
forecast_stock <- function(stock_name) {
  df <- sp500_stocks[sp500_stocks$name == stock_name, ]
  ts_data <- ts(df$close, frequency = 252)
  
  # ARIMA model
  arima_model <- auto.arima(ts_data)
  arima_forecast <- forecast(arima_model, h = 252)  # Forecasting for 1 year
  arima_plot <- autoplot(arima_forecast) + 
    ggtitle(paste("ARIMA Forecast for", stock_name)) + 
    theme_minimal() +
    theme(plot.title = element_text(color = "black"),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"))
  
  # GARCH model
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 1))
  )
  garch_model <- ugarchfit(spec = spec, data = ts_data)
  garch_forecast <- ugarchforecast(garch_model, n.ahead = 252)
  
  garch_plot <- plot(garch_forecast, which = 1) +
    ggtitle(paste("GARCH Forecast for", stock_name)) +
    theme_minimal() + 
    theme(plot.title = element_text(color = "black"),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"))
  
  list(arima_plot = arima_plot, garch_plot = garch_plot)
}

# Run forecasts for top 5 stocks
forecasts <- lapply(top_5_stocks, forecast_stock)

# Function to plot ARIMA and GARCH forecast with improved visualization
plt_forecast <- function(stock_name, arima_forecast, garch_forecast) {
  p1 <- autoplot(arima_forecast) + 
    ggtitle(paste("ARIMA Forecast for", stock_name)) + 
    theme_minimal() +
    theme(plot.title = element_text(color = "black"),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"))
  
  p2 <- plot(garch_forecast, which = 1) +
    ggtitle(paste("GARCH Forecast for", stock_name)) +
    theme_minimal() + 
    theme(plot.title = element_text(color = "black"),
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"))
  
  subplot(p1, p2, nrows = 2, shareX = TRUE, shareY = TRUE)
}

# Plot forecasts for each top stock
for (i in 1:length(top_5_stocks)) {
  df <- sp500_stocks[sp500_stocks$name == top_5_stocks[i], ]
  ts_data <- ts(df$close, frequency = 252)
  
  arima_model <- auto.arima(ts_data)
  arima_forecast <- forecast(arima_model, h = 252)
  
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(1, 1))
  )
  garch_model <- ugarchfit(spec = spec, data = ts_data)
  garch_forecast <- ugarchforecast(garch_model, n.ahead = 252)
  
  print(plt_forecast(top_5_stocks[i], arima_forecast, garch_forecast))
}
