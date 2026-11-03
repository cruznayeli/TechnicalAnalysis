rsi <- function(data, period) {
  # Calculate the differences between consecutive data points
  diff_values <- diff(data)

  # Initialize two vectors to store the gains and losses
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  # Calculate gains and losses
  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }

  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA_real_, length(data))

  # Not enough data for the first RSI value
  if (length(data) <= period) {
    return(rsi_values)
  }

  # Calculate the average gains and average losses for the first 'period' data points
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period

  # Calculate RSI values using the Wilder's smoothing method
  for (i in (period + 1):length(data)) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period

    rs <- avg_gain / avg_loss
    rsi_values[i] <- 100 - (100 / (1 + rs))
  }

  return(rsi_values)
}
