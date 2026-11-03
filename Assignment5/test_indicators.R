# Run from the TechnicalAnalysis project. Moves into the Assignment5 folder if needed
if (!file.exists("sma.R") && file.exists("Assignment5/sma.R")) setwd("Assignment5")

for (f in c("sma", "ema", "macd", "stdev", "linreg", "rsi", "stoch_rsi",
            "crossover", "crossunder")) {
  source(paste0(f, ".R"))
}

check <- function(name, ok) {
  cat(sprintf("%-45s %s\n", name, if (isTRUE(ok)) "PASS" else "FAIL"))
}
fails <- function(expr) inherits(try(expr, silent = TRUE), "try-error")

x <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr1 <- x
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)

# SMA
check("sma length", length(sma(x, 3)) == length(x) - 3 + 1)
check("sma matches stats::filter",
      isTRUE(all.equal(sma(x, 3), as.numeric(stats::filter(x, rep(1 / 3, 3), sides = 1))[3:9])))
check("sma stops when data is too short", fails(sma(c(1, 2), 3)))

# EMA
e <- ema(x, 3)
check("ema first value equals first data point", e[1] == x[1])
check("ema follows the recursive formula",
      isTRUE(all.equal(e[-1], 0.5 * x[-1] + 0.5 * e[-length(e)])))
check("ema of a constant series is constant", all(ema(rep(5, 6), 3) == 5))

# MACD
m <- macd(x, 3, 5, 2)
check("macd line is short ema minus long ema",
      isTRUE(all.equal(m$macd_line, ema(x, 3) - ema(x, 5))))
check("macd histogram is macd minus signal",
      isTRUE(all.equal(m$histogram, m$macd_line - m$signal_line)))

# Standard deviation
check("stdev matches the population formula",
      isTRUE(all.equal(stdev(x), sd(x) * sqrt((length(x) - 1) / length(x)))))

# Linear regression
l <- linreg(x, length(x), 0)
fit <- as.numeric(coef(lm(x ~ seq_along(x))))
check("linreg intercept matches lm", isTRUE(all.equal(l$intercept, fit[1])))
check("linreg slope matches lm", isTRUE(all.equal(l$slope, fit[2])))
check("linreg stops when length is too big", fails(linreg(x, 20, 0)))
check("linreg stops when offset is too big", fails(linreg(x, 5, 5)))

# RSI
r <- rsi(c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62), 5)
check("rsi first period values are NA", sum(is.na(r)) == 5)
check("rsi stays between 0 and 100", all(r[!is.na(r)] >= 0 & r[!is.na(r)] <= 100))
check("rsi is 100 when prices only rise",
      all(rsi(as.numeric(1:10), 3)[-(1:3)] == 100))

# StochRSI
s <- stoch_rsi(c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62), 5, 3, 3)
check("stoch_rsi k line stays between 0 and 1",
      all(s$k_line >= 0 & s$k_line <= 1, na.rm = TRUE))
check("stoch_rsi d line stays between 0 and 1",
      all(s$d_line >= 0 & s$d_line <= 1, na.rm = TRUE))

# Crossover and crossunder
up <- crossover(arr1, arr2)
down <- crossunder(arr2, arr1)
check("crossover length matches input", length(up) == length(arr1))
check("crossover finds one Up at position 4", identical(which(up == "Up"), 4L))
check("crossover finds no Down", !any(up == "Down"))
check("crossunder finds True at position 4", down[4] == "True")
check("crossunder finds no True for arr1 over arr2", !any(crossunder(arr1, arr2) == "True"))
check("crossover stops on different lengths", fails(crossover(1:3, 1:4)))
