
# Regressions for Bitcoin Volatility --------------------------------------

# More information in my GitHub repo 

setwd("C:/Users/Henning/Desktop/Studium/bachelor_thesis")
data = read.csv2("regression_data_set.csv", header=TRUE, sep=",")

# packages ----------------------------------------------------------------

#install.packages("tseries")
#install.packages("lmtest")
#install.packages("modelsummary")
#install.packages("dplyr")
#install.packages("tidyverse")
library(tidyverse)
library(modelsummary)
library(tseries)
library(lmtest)
#install.packages("car")
#install.packages("tseries")
library(car)
library(tseries)
library(zoo)
library(dplyr)
#install.packages("dynamac")
library(dynamac)
#install.packages("urca")
library(urca)
#install.packages("TSstudio")
library(TSstudio)
#install.packages("ARDL")
library(ARDL)

# regression --------------------------------------------------------------

data$date = as.Date(data$date)
data$vola = as.numeric(data$vola)
data$open = as.numeric(data$open)
data$close = as.numeric(data$close)
data$delta_price = as.numeric(data$delta_price)
View(data)
data = data[-(1:29), ]
rownames(data) = NULL
View(data)

start = min(data$date)
end = max(data$date)


# filling in NA where no data entry
# you can see in row 80 for example the is no entry for the twtr variable
data[data == 0] = NA
data[data == ""] = NA

# interpolation for NA values
data$btc <- na.approx(data$btc, rule=2)
data$twtr <- na.approx(data$twtr, rule=2)

View(data)

# checking for dates being correct and complete

full_date_range <- seq(from = min(data$date), to = max(data$date), by = "day")

missing_dates <- setdiff(full_date_range, data$date)

num_missing_dates <- length(missing_dates)

cat("Anzahl der fehlenden Daten: ", num_missing_dates, "\n")
cat("Fehlende Daten:\n")
print(missing_dates)


# checking for stationarity

adf.test(data$vola) #stationary 1% sig
adf.test(data$btc) #non-stationary --> diff
adf.test(data$twtr) # stationary 10% sig close to 5%

# ADL-model regression

data$vola_lag1 = lag(data$vola, 1)
data$twtr_lag1 = lag(data$twtr, 1)
data$btc_lag1 = lag(data$btc, 1)
data$btc_percent_change = ((data$btc - data$btc_lag1) / data$btc_lag1)*100
data$btc_percent_change_lag1 = lag(data$btc_percent_change, 1)


print(sum(is.na(data)))
# NAs because of lag variables:
data = na.omit(data)
rownames(data) = NULL

model1 = lm(vola ~ vola_lag1 + twtr + twtr_lag1 + btc + btc_lag1, data = data)
model2 = lm(vola ~ vola_lag1 + twtr + twtr_lag1 + btc_percent_change + btc_percent_change_lag1, data = data)
model3 = lm(vola ~ vola_lag1 + lag(vola, 2) + twtr + twtr_lag1 + lag(twtr, 2) + btc_percent_change + btc_percent_change_lag1, data = data)

models = list("Standard (t-1 Lag)" = model1, "ΔBTC/BTC" = model2, "t-2 Lag" = model3)

modelsummary(models, stars=c('***' = 0.01, '**' = 0.05, '*' = 0.1), gof_omit = "^(?!R2|Num)")

