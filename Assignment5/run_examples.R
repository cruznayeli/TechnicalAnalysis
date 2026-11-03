# Run from the TechnicalAnalysis project. Moves into the Assignment5 folder if needed
if (!file.exists("sma.R") && file.exists("Assignment5/sma.R")) setwd("Assignment5")

for (f in c("sma", "ema", "macd", "stdev", "linreg", "rsi", "stoch_rsi",
            "crossover", "crossunder")) {
  source(paste0(f, ".R"))
}

data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
prices <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)

print(sma(data, period = 3))
print(ema(data, period = 3))
print(macd(c(100, 105, 110, 115, 120, 125, 130),
           short_period = 3, long_period = 5, signal_period = 2))
print(stdev(data))
print(linreg(data, length(data), 0))
print(rsi(prices, period = 5))
print(stoch_rsi(prices, period = 5, k_period = 3, d_period = 3))
print(crossover(arr1, arr2))
print(crossunder(arr1, arr2))
