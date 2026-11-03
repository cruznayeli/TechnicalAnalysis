for (f in c("rsi", "sma")) {
  if (!exists(f, mode = "function")) {
    source(if (file.exists(paste0(f, ".R"))) paste0(f, ".R") else paste0("Assignment5/", f, ".R"))
  }
}

# Stochastic RSI (StochRSI) function
stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)

  # Calculate the StochRSI
  if (all(is.na(rsi_values))) {
    k_values <- rsi_values
  } else {
    min_rsi <- min(rsi_values, na.rm = TRUE)
    max_rsi <- max(rsi_values, na.rm = TRUE)
    k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  }

  # Calculate the %K line (StochRSI)
  k_line <- sma(k_values, k_period)

  # Calculate the %D line (simple moving average of %K)
  d_line <- sma(k_line, d_period)

  # Return the %K and %D lines as a list
  result <- list(k_line = k_line, d_line = d_line)

  return(result)
}
