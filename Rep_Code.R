# Clean the memory
rm(list = ls()) 

# Clean the screen
cat("\014")   # Equivalent to "ctrl+L"

# Set the working directory+

setwd("C:/Aalto/aMaster/Advanced Econometrics for Finen Markets/Term Paper/Analysis")
#--------------------------------------------------------

library(quantmod)
library(fBasics)
library(readxl)
library(dplyr)
library(zoo)
library(ggplot2)
library(lubridate)

# Read the CSV file
df <- read.csv("def_term_inf.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Convert the date column by appending "-01" to ensure it's a complete date (YYYY-MM-01)
df$date <- as.Date(paste0(df$date, "-01"), format="%Y-%m-%d")

# Drop the 'X' column
df <- df[, -1]


# Convert to a zoo object
df_zoo <- read.zoo(df, index.column = "date", FUN = as.Date)

# Convert to xts
def_term <- as.xts(df)


# View actual column names
print(colnames(def_term))  

# View first rows
head(def_term)
tail(def_term)

#------------------------------------------------------------------------------
# Load the Dowjones dataset
df <- read.csv("dow_jones.csv")

# Drop the 'X' column
df <- df[, -1]

# Ensure the 'Date' column is in Date format
df$Date <- as.Date(df$Date)

# Convert the data frame to xts format, using the Date column as the index
#df <- xts(df[, -1], order.by = df$Date)
colnames(df) <- c("date","pt","p52_t","pmax_t","x52_t","xmax_t","Dt","It")

# Display the xts object
head(df)

# Rename columns for clarity
colnames(df) <- c("date", "pt","p52_t","pmax_t","x52_t","xmax_t","Dt","It")
# Keep only numeric columns for analysis
df_numeric <- df[, sapply(df, is.numeric)]

# Compute basic statistics and assign to 'stats'
stats <- basicStats(df_numeric)

# Compute first-order autocorrelation (AC(1)) for each column
ac1_values <- sapply(df_numeric, function(x) acf(x, lag.max = 1, plot = FALSE)$acf[2])

# Convert AC(1) values into a matrix with the same column names
ac1_matrix <- matrix(ac1_values, nrow = 1, dimnames = list("AC(1)", colnames(df_numeric)))

# Append AC(1) row to 'stats'
stats <- rbind(stats, ac1_matrix)

# Print results
print(stats)

# Convert to monthly data (last value of each month)
df_monthly <- df %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise(
    pt = last(pt),
    p52_t = last(p52_t),
    pmax_t = last(pmax_t),
    x52_t = last(x52_t),
    xmax_t = last(xmax_t),
    Dt = last(Dt),
    It = last(It),
    .groups = "drop"  # Prevents group structure warning
  )

# Print first few rows of the final monthly dataset
print(head(df_monthly))


# Convert to xts object
dowjones <- as.xts(df_monthly)

head(dowjones)                

## merge data
# Merge terms with Dows data
data <- merge(dowjones,def_term,join = "inner")
# data = na.omit(data)
data <- as.data.frame(data)
class(data)  # Should return "data.frame"

data <- data %>%
  dplyr::select(Mkt.RF, x52_t, xmax_t, Dt, It, DEFt, TERMt, Inflation, 
                Real_interest, cay, dividend_yield, St, Value_weighted, 
                Near_52_NY, Near_max_NY, D_NY, I_NY)


#--------------------------------------------------------------------------------
### Table 1A Summary Statistics
# Compute basic statistics and assign to 'stats'
stats <- basicStats(data)

# Compute first-order autocorrelation (AC(1)) for each column
ac1_values <- sapply(data, function(x) acf(x, lag.max = 1, plot = FALSE)$acf[2])

# Convert AC(1) values into a matrix with the same column names
ac1_matrix <- matrix(ac1_values, nrow = 1, dimnames = list("AC(1)", colnames(data)))

# Append AC(1) row to 'stats'
stats <- rbind(stats, ac1_matrix)

# Print results
print(stats)

### Table 1B Correlation matrix
#Calculate the correlation matrix
cor_matrix <- cor(data, use = "complete.obs")

# View the correlation matrix
print(cor_matrix)
#-------------------------------------------------------------------------------------
### Table 2
library(dplyr)

# Create a X-month ahead data frame

data$excess_return_1m <- lead(data$Mkt.RF, 1)
data$excess_return_3m <- lead(data$Mkt.RF, 3)
data$excess_return_6m <- lead(data$Mkt.RF, 6)
data$excess_return_12m <- lead(data$Mkt.RF, 12)
head(data)
tail(data)


# Load necessary libraries
library(sandwich)
library(lmtest)

# Function to perform regression and calculate Newey-West t-stats with R-squared
regression_with_nw <- function(data, formula, lag = 1) {
  reg <- lm(formula, data = data)  # Perform linear regression
  nw_se <- NeweyWest(reg, lag = lag)  # Compute Newey-West standard errors
  
  # Get coefficients and t-stats
  coefficients <- coef(reg)
  t_stats <- coefficients / sqrt(diag(nw_se))
  
  # Create a summary table
  summary_results <- cbind(coefficients, t_stats)
  colnames(summary_results) <- c("Estimate", "Newey-West t-stat")
  
  # Compute R-squared
  r_squared <- summary(reg)$r.squared
  
  # Return results as a list
  return(list(
    summary_table = summary_results,
    r_squared = r_squared
  ))
}

# 1-month ahead regressions
reg_1var_1m_summary <- regression_with_nw(data, excess_return_1m ~ Mkt.RF)
reg_2var_1m_summary <- regression_with_nw(data, excess_return_1m ~ Mkt.RF + xmax_t)
reg_5var_1m_summary <- regression_with_nw(data, excess_return_1m ~ Mkt.RF + x52_t + xmax_t + Dt + It)

# 3-month ahead regressions
reg_1var_3m_summary <- regression_with_nw(data, excess_return_3m ~ Mkt.RF)
reg_2var_3m_summary <- regression_with_nw(data, excess_return_3m ~ Mkt.RF + xmax_t)
reg_5var_3m_summary <- regression_with_nw(data, excess_return_3m ~ Mkt.RF + x52_t + xmax_t + Dt + It)

# 6-month ahead regressions
reg_1var_6m_summary <- regression_with_nw(data, excess_return_6m ~ Mkt.RF)
reg_2var_6m_summary <- regression_with_nw(data, excess_return_6m ~ Mkt.RF + xmax_t)
reg_5var_6m_summary <- regression_with_nw(data, excess_return_6m ~ Mkt.RF + x52_t + xmax_t + Dt + It)

# 12-month ahead regressions
reg_1var_12m_summary <- regression_with_nw(data, excess_return_12m ~ Mkt.RF)
reg_2var_12m_summary <- regression_with_nw(data, excess_return_12m ~ Mkt.RF + xmax_t)
reg_5var_12m_summary <- regression_with_nw(data, excess_return_12m ~ Mkt.RF + x52_t + xmax_t + Dt + It)

# View the summary results for all regressions
print(reg_1var_1m_summary)
print(reg_1var_3m_summary)
print(reg_1var_6m_summary)
print(reg_1var_12m_summary)

print(reg_2var_1m_summary)
print(reg_2var_3m_summary)
print(reg_2var_6m_summary)
print(reg_2var_12m_summary)

print(reg_5var_1m_summary)
print(reg_5var_3m_summary)
print(reg_5var_6m_summary)
print(reg_5var_12m_summary)

#--------------------------------------------------------------------------------
### Table 3 Macro Regression

reg_macro_1m <- regression_with_nw(data, excess_return_1m ~ Mkt.RF + x52_t + xmax_t + Dt + It + DEFt + TERMt + Inflation + Real_interest + cay + dividend_yield + St)
reg_macro_3m <- regression_with_nw(data, excess_return_3m ~ Mkt.RF + x52_t + xmax_t + Dt + It + DEFt + TERMt + Inflation + Real_interest + cay + dividend_yield +St)
reg_macro_6m <- regression_with_nw(data, excess_return_6m ~ Mkt.RF + x52_t + xmax_t + Dt + It + DEFt + TERMt + Inflation + Real_interest + cay + dividend_yield +St)
reg_macro_12m <- regression_with_nw(data, excess_return_12m ~ Mkt.RF + x52_t + xmax_t + Dt + It + DEFt + TERMt + Inflation + Real_interest + cay + dividend_yield +St)

print(reg_macro_1m)
print(reg_macro_3m)
print(reg_macro_6m)
print(reg_macro_12m)

#---------------------------------------------------------------------------------
### Table 4A NYSE/Amex market cap as benchmark Overlapping Regression

reg_index_1m <- regression_with_nw(data, excess_return_1m ~ Mkt.RF + Near_52_NY + Near_max_NY + D_NY + I_NY)
reg_index_3m <- regression_with_nw(data, excess_return_3m ~ Mkt.RF + Near_52_NY + Near_max_NY + D_NY + I_NY)
reg_index_6m <- regression_with_nw(data, excess_return_6m ~ Mkt.RF + Near_52_NY + Near_max_NY + D_NY + I_NY)
reg_index_12m <- regression_with_nw(data, excess_return_12m ~ Mkt.RF + Near_52_NY + Near_max_NY + D_NY + I_NY)


print(reg_index_1m)
print(reg_index_3m)
print(reg_index_6m)
print(reg_index_12m)

### Table 4B Horse race between the Dow index and NYSE/ Amex market cap Overlapping Regression
data$excess_return_1m <- lead(data$Value_weighted, 1)
data$excess_return_3m <- lead(data$Value_weighted, 3)
data$excess_return_6m <- lead(data$Value_weighted, 6)
data$excess_return_12m <- lead(data$Value_weighted, 12)

reg_indexB_1m <- regression_with_nw(data, excess_return_1m ~ Mkt.RF + x52_t + xmax_t + Dt + It + Near_52_NY + Near_max_NY)
reg_indexB_3m <- regression_with_nw(data, excess_return_3m ~ Mkt.RF + x52_t + xmax_t + Dt + It + Near_52_NY + Near_max_NY)
reg_indexB_6m <- regression_with_nw(data, excess_return_6m ~ Mkt.RF + x52_t + xmax_t + Dt + It + Near_52_NY + Near_max_NY)
reg_indexB_12m <- regression_with_nw(data, excess_return_12m ~ Mkt.RF + x52_t + xmax_t + Dt + It + Near_52_NY + Near_max_NY)


print(reg_indexB_1m)
print(reg_indexB_3m)
print(reg_indexB_6m)
print(reg_indexB_12m)


#--------------------------------------------------------------------------------
### Table 5 Subsamples Monthly overlapping regression
# Subsamples
data_t1 <- data[1:260,]
data_t2 <- data[261:524,]
data_t3 <- data[525:716,]
head(data_t3)
tail(data_t3)

# Subsamples regression
reg_t1_1m <- regression_with_nw(data_t1, excess_return_1m ~ Mkt.RF + x52_t + xmax_t + Dt + It + DEFt + TERMt + Inflation + Real_interest + cay + dividend_yield +St)
reg_t1_3m <- regression_with_nw(data_t1, excess_return_3m ~ Mkt.RF + x52_t + xmax_t + Dt + It + DEFt + TERMt + Inflation + Real_interest + cay + dividend_yield +St)
reg_t1_6m <- regression_with_nw(data_t1, excess_return_6m ~ Mkt.RF + x52_t + xmax_t + Dt + It + DEFt + TERMt + Inflation + Real_interest + cay + dividend_yield +St)
reg_t1_12m <- regression_with_nw(data_t1, excess_return_12m ~ Mkt.RF + x52_t + xmax_t + Dt + It + DEFt + TERMt + Inflation + Real_interest + cay + dividend_yield +St)

reg_t2_1m <- regression_with_nw(data_t2, excess_return_1m ~ Mkt.RF + x52_t + xmax_t + Dt + It + DEFt + TERMt + Inflation + Real_interest + cay + dividend_yield +St)
reg_t2_3m <- regression_with_nw(data_t2, excess_return_3m ~ Mkt.RF + x52_t + xmax_t + Dt + It + DEFt + TERMt + Inflation + Real_interest + cay + dividend_yield +St)
reg_t2_6m <- regression_with_nw(data_t2, excess_return_6m ~ Mkt.RF + x52_t + xmax_t + Dt + It + DEFt + TERMt + Inflation + Real_interest + cay + dividend_yield +St)
reg_t2_12m <- regression_with_nw(data_t2, excess_return_12m ~ Mkt.RF + x52_t + xmax_t + Dt + It + DEFt + TERMt + Inflation + Real_interest + cay + dividend_yield +St)

reg_t3_1m <- regression_with_nw(data_t3, excess_return_1m ~ Mkt.RF + x52_t + xmax_t + Dt + It + DEFt + TERMt + Inflation + Real_interest + cay + dividend_yield +St)
reg_t3_3m <- regression_with_nw(data_t3, excess_return_3m ~ Mkt.RF + x52_t + xmax_t + Dt + It + DEFt + TERMt + Inflation + Real_interest + cay + dividend_yield +St)
reg_t3_6m <- regression_with_nw(data_t3, excess_return_6m ~ Mkt.RF + x52_t + xmax_t + Dt + It + DEFt + TERMt + Inflation + Real_interest + cay + dividend_yield +St)
reg_t3_12m <- regression_with_nw(data_t3, excess_return_12m ~ Mkt.RF + x52_t + xmax_t + Dt + It + DEFt + TERMt + Inflation + Real_interest + cay + dividend_yield +St)

print(reg_t1_1m)
print(reg_t1_3m)
print(reg_t1_6m)
print(reg_t1_12m)

print(reg_t2_1m)
print(reg_t2_3m)
print(reg_t2_6m)
print(reg_t2_12m)

print(reg_t3_1m)
print(reg_t3_3m)
print(reg_t3_6m)
print(reg_t3_12m)

#--------------------------------------------------------------------------------
### Table 6
# Load required packages
library(vars)
library(sandwich)
library(lmtest)
library(MASS)

# Data contains time series data for x52 and xmax
# Ensure the data is in time series format
ts_data <- ts(data[, c("x52_t", "xmax_t")], start = c(1953, 5), frequency = 12)  # Monthly data


# Set seed for reproducibility
set.seed(123)

# Set simulation parameters
n_simulations <- 10000  # Number of simulations
S <- 200              # Sample size of the actual data
T <- S + 100
forecast_horizons <- c(1,3,6,12)  # Forecast horizons in months

# Null hypothesis model for returns
g0 <- 0   # No predictability in returns (constant)
g1 <- 0    # Coefficient of x52_t-1 (predictor 1)
g2 <- 0    # Coefficient of xmax_t-1 (predictor 2)

# Storage for results
t_stats_g1 <- matrix(NA, nrow = n_simulations, ncol = length(forecast_horizons))
t_stats_g2 <- matrix(NA, nrow = n_simulations, ncol = length(forecast_horizons))
r_squared <- matrix(NA, nrow = n_simulations, ncol = length(forecast_horizons))

# Adjust the loop to handle the forecast horizon correctly
for (sim in 1:n_simulations) {
  
  # Generate random data for predictors (x52_t and xmax_t) using VAR(1)
  e1_t <- rnorm(T, mean = 0, sd = 1)  # Errors for x52
  e2_t <- rnorm(T, mean = 0, sd = 1)  # Errors for xmax
  
  # Initialize x52 and xmax series
  x52_t <- numeric(T)
  xmax_t <- numeric(T)
  

  # Loop over different forecast horizons
  for (i in 1:length(forecast_horizons)) {
    forecast_horizon <- forecast_horizons[i]
    # Fit a VAR(1) model
    var_model <- VAR(ts_data, p = forecast_horizon, type = "const")  # p = 1 for lag-1 model
    
    # Extract coefficients
    coef_matrix <- coef(var_model)
    
    # Assign estimated parameters from the VAR model
    a1  <- coef_matrix$x52_t["const", "Estimate"]       # Intercept for x52 equation
    r11 <- coef_matrix$x52_t["x52_t.l1", "Estimate"]    # Coefficient of x52_{t-1} in x52 equation
    r12 <- coef_matrix$x52_t["xmax_t.l1", "Estimate"]   # Coefficient of xmax_{t-1} in x52 equation
    
    a2  <- coef_matrix$xmax_t["const", "Estimate"]      # Intercept for xmax equation
    r21 <- coef_matrix$xmax_t["x52_t.l1", "Estimate"]  # Coefficient of x52_{t-1} in xmax equation
    r22 <- coef_matrix$xmax_t["xmax_t.l1", "Estimate"] # Coefficient of xmax_{t-1} in xmax equation
    
    # Simulate the VAR(forecast_horizon) process for x52 and xmax
    for (t in (forecast_horizon + 1):T) {
      # Use the forecast horizon lag instead of t-1
      x52_t[t] <- a1 + r11 * x52_t[t - forecast_horizon] + r12 * xmax_t[t - forecast_horizon] + e1_t[t]
      xmax_t[t] <- a2 + r21 * x52_t[t - forecast_horizon] + r22 * xmax_t[t - forecast_horizon] + e2_t[t]
    }
    
    # Generate returns under the null hypothesis
    r_t <- g0 + g1 * x52_t[-T] + g2 * xmax_t[-T] + rnorm(T-1, mean = 0, sd = 1)
    
    
    # Ensure that enough data points are available for the lagged series
    if (T - forecast_horizon >= 1) {
      # Define the predictors for the regression (lagged values)
      x52_lag <- x52_t[1:(T - forecast_horizon)]
      xmax_lag <- xmax_t[1:(T - forecast_horizon)]
      r_t_forecast <- r_t[1:(T - forecast_horizon)]
      
      # Run the predictive regression: r_t = g0 + g1 * x52_{t-1} + g2 * xmax_{t-1} + error
      regression <- lm(r_t_forecast ~ x52_lag + xmax_lag)
      
      # Extract coefficients safely
      coefs <- summary(regression)$coefficients
      
      # Handle t-statistics for g1 and g2 safely
      if (nrow(coefs) >= 2) {
        t_stats_g1[sim, i] <- coefs[2, 3]  # t-statistic for x52_lag
      } else {
        t_stats_g1[sim, i] <- NA
      }
      
      if (nrow(coefs) >= 3) {
        t_stats_g2[sim, i] <- coefs[3, 3]  # t-statistic for xmax_lag
      } else {
        t_stats_g2[sim, i] <- NA
      }
      
      # Calculate R-squared for each regression
      r_squared[sim, i] <- summary(regression)$r.squared
    }
  }
}

# Calculate empirical sizes of t-statistics (how often they exceed 1.96)
empirical_size_g1 <- apply(t_stats_g1 > 1.96, 2, mean, na.rm = TRUE)
empirical_size_g2 <- apply(t_stats_g2 > 1.96, 2, mean, na.rm = TRUE)
empirical_size_both <- apply(abs(t_stats_g1) > 1.96 & abs(t_stats_g2) > 1.96, 2, mean, na.rm = TRUE)

# Calculate mean R-squared for each forecast horizon
mean_r_squared <- apply(r_squared, 2, mean, na.rm = TRUE)

# Calculate 97.5% quantile of the t-statistics for x52 (t(52-week))
t_52_week <- apply(t_stats_g1, 2, quantile, probs = 0.975, na.rm = TRUE)

# Calculate 2.5% quantile of the t-statistics for xmax (t(max))
t_max <- apply(t_stats_g2, 2, quantile, probs = 0.025, na.rm = TRUE)

# Calculate 95% Confidence Interval for R-squared (2.5% and 97.5% quantiles)
r_squared_CI <- apply(r_squared, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)

# Print results
cat("Empirical size of t-statistics for g1 (x52_t-1 coefficient):\n")
print(empirical_size_g1)
cat("\nEmpirical size of t-statistics for g2 (xmax_t-1 coefficient):\n")
print(empirical_size_g2)
cat("\nSize (both predictors jointly significant): \n")
print(empirical_size_both)
      
cat("\nMean R-squared values for each forecast horizon:\n")
print(mean_r_squared)

cat("\n97.5% quantile of t-statistics for x52 (t(52-week)):\n")
print(t_52_week)

cat("\n2.5% quantile of t-statistics for xmax (t(max)):\n")
print(t_max)

cat("\n95% Confidence Interval for Monte Carlo-generated R²:\n")
print(r_squared_CI)

print(t_stats_g1)
print(coefs)
a <- cor(x52_t,xmax_t)
print(a)
# Conclusion:
# - If empirical size is significantly larger than 0.05 (5%), it suggests size distortion.
# - Report findings at each forecast horizon.
cor(x52_lag, xmax_lag)
#------------------------------------------------------------------------------------------
# Table 6B
# Panel B

### Panel B: Out-of-Sample Forecasting Analysis

# Load required packages
library(lmtest)
library(sandwich)
library(zoo)

# Data contains:
# - excess_returns: Future excess returns
# - x52_t: Nearness to 52-week high
# - xmax_t: Nearness to historical high
# - date: Time index

# Define time frame
data <- data[1:706,]
start_year <- 1953
initial_window <- 20 * 12  # First 20 years (monthly data)
total_months <- nrow(data)  # Total number of months

# Define forecast horizons
forecast_horizons <- c(1, 3, 6, 12)  # 1-month, 3-month, 6-month, 12-month

# Storage for results
oos_r2_model1 <- numeric(length(forecast_horizons))  # Only xmax_t predictor
oos_r2_model2 <- numeric(length(forecast_horizons))  # Both xmax_t & x52_t predictors

# Loop over forecast horizons
for (h in forecast_horizons) {
  
  mse_model1 <- c()
  mse_model2 <- c()
  mse_benchmark <- c()
  
  # Recursive estimation loop
  for (t in (initial_window + 1):(total_months - h)) {
    
    # Define training sample (up to time t)
    train_data <- data[1:t, ]
    
    # Define test sample (out-of-sample forecast at t+h)
    test_data <- data[t + h, ]
    
    # Calculate cumulative future excess return over horizon h
    future_return <- sum(data$Mkt.RF[t:(t + h - 1)])  # Cumulative return
    
    # Model 1: Only nearness to historical high (xmax_t)
    model1 <- lm(Mkt.RF ~ xmax_t, data = train_data)
    pred1 <- predict(model1, newdata = test_data)
    
    # Model 2: Both nearness to historical high (xmax_t) and 52-week high (x52_t)
    model2 <- lm(Mkt.RF ~ xmax_t + x52_t, data = train_data)
    pred2 <- predict(model2, newdata = test_data)
    
    # Benchmark: Historical mean of excess returns
    hist_mean <- mean(train_data$Mkt.RF, na.rm = TRUE)
    
    # Compute squared errors
    mse_model1 <- c(mse_model1, (future_return - pred1)^2)
    mse_model2 <- c(mse_model2, (future_return - pred2)^2)
    mse_benchmark <- c(mse_benchmark, (future_return - hist_mean)^2)
  }
  
  # Compute Out-of-Sample R²
  oos_r2_model1[which(forecast_horizons == h)] <- 1 - (mean(mse_model1) / mean(mse_benchmark))
  oos_r2_model2[which(forecast_horizons == h)] <- 1 - (mean(mse_model2) / mean(mse_benchmark))
}

# Print results
cat("\nOut-of-Sample R² for Model 1 (Only Nearness to Historical High):\n")
print(oos_r2_model1)

cat("\nOut-of-Sample R² for Model 2 (Both Nearness to Historical and 52-week High):\n")
print(oos_r2_model2)





