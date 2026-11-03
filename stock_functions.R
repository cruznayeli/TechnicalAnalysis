library(quantmod)

# Reads portfolio.txt and returns one data frame per symbol
load_stock_data <- function(file = "portfolio.txt",
                            from = "2026-01-01",
                            to = "2026-10-30") {
  symbols <- trimws(readLines(file))
  symbols <- symbols[symbols != ""]
  stocks <- list()
  for (s in symbols) {
    stocks[[s]] <- tryCatch({
      raw <- getSymbols(s, src = "yahoo", from = from, to = to,
                        auto.assign = FALSE)
      df <- data.frame(Date = index(raw), coredata(raw))
      names(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
      df <- df[complete.cases(df), ]
      rownames(df) <- NULL
      df
    }, error = function(e) {
      message("Could not load ", s)
      NULL
    })
  }
  stocks
}

# Base R has no mode function for data, so this finds the most common value
get_mode <- function(x) {
  x <- x[!is.na(x)]
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}

# Statistics on the closing price of one stock
calculate_statistics <- function(df, n = 20) {
  close <- df$Close
  list(
    mean = mean(close),
    median = median(close),
    mode = get_mode(close),
    sd = sd(close),
    moving_average = TTR::SMA(close, n = min(n, length(close)))
  )
}

# One row per stock, for easy display
summary_table <- function(all_stats) {
  data.frame(
    Symbol = names(all_stats),
    Mean = sapply(all_stats, function(x) x$mean),
    Median = sapply(all_stats, function(x) x$median),
    Mode = sapply(all_stats, function(x) x$mode),
    SD = sapply(all_stats, function(x) x$sd),
    Latest_MA = sapply(all_stats, function(x) tail(x$moving_average, 1)),
    row.names = NULL
  )
}
