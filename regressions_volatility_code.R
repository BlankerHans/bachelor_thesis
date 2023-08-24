
# Regressions for Bitcoin Volatility --------------------------------------

# More information in my GitHub repo 

setwd("C:/Users/Henning/Desktop/Studium/bachelor_thesis")
data = read.csv2("regression_data_set.csv", header=TRUE, sep=",")

# packages ----------------------------------------------------------------

#install.packages("tseries")
#install.packages("lmtest")
#install.packages("modelsummary")
library(modelsummary)
library(tseries)
library(lmtest)


# regression --------------------------------------------------------------

View(data)
data = na.omit(data)
View(data)

# df <– as.data.frame(diff(as.matrix(data), lag = 1))
# colnames(df) <– c(“LRM”, “LRY”, “IBO”, “IDE”)

# checking for stationarity

adf.test(data$Vola)
adf.test(data$Supply)
adf.test(data$Tweets)
