# Portfolio dashboard for stock data, indicators and trading signals

required_packages <- c("shiny", "ggplot2", "quantmod", "TTR", "xts")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(xts)

stock_symbol <- "AAPL"
start_date   <- as.Date("2023-01-01")
end_date     <- as.Date("2023-07-01")

WARMUP_DAYS <- 1800

fetch_from_yahoo <- function(symbol, from, to) {
  tryCatch({
    x <- suppressWarnings(
      getSymbols(symbol, src = "yahoo", from = from, to = to, auto.assign = FALSE)
    )
    if (is.null(x) || nrow(x) == 0) NULL else x
  }, error = function(e) NULL)
}

fetch_from_stooq <- function(symbol, from, to) {
  tryCatch({
    url <- sprintf(
      "https://stooq.com/q/d/l/?s=%s.us&i=d&d1=%s&d2=%s",
      tolower(symbol), format(from, "%Y%m%d"), format(to, "%Y%m%d")
    )
    raw <- read.csv(url, stringsAsFactors = FALSE)
    needed <- c("Date", "Open", "High", "Low", "Close")
    if (!all(needed %in% names(raw)) || nrow(raw) == 0) return(NULL)
    if (!"Volume" %in% names(raw)) raw$Volume <- NA_real_
    x <- xts(raw[, c("Open", "High", "Low", "Close", "Volume")],
             order.by = as.Date(raw$Date))
    colnames(x) <- paste0(toupper(symbol), c(".Open", ".High", ".Low", ".Close", ".Volume"))
    x
  }, error = function(e) NULL)
}

fetch_stock <- function(symbol, from, to) {
  symbol <- toupper(trimws(symbol))
  from_padded <- from - WARMUP_DAYS

  x <- fetch_from_yahoo(symbol, from_padded, to)
  source_name <- "Yahoo Finance"

  if (is.null(x)) {
    x <- fetch_from_stooq(symbol, from_padded, to)
    source_name <- "Stooq (backup)"
  }
  if (is.null(x)) return(NULL)

  x <- x[!is.na(Cl(x)) & !is.na(Op(x)) & !is.na(Hi(x)) & !is.na(Lo(x)), ]
  if (nrow(x) == 0) return(NULL)

  list(data = x, source = source_name, symbol = symbol)
}

change_time_frame <- function(x, time_frame) {
  if (time_frame == "Daily") return(x)

  on <- if (time_frame == "Weekly") "weeks" else "months"
  ep <- endpoints(x, on = on)

  first_val <- function(z) as.numeric(z[1])
  last_val  <- function(z) as.numeric(z[length(z)])

  out <- cbind(
    period.apply(Op(x), ep, first_val),
    period.apply(Hi(x), ep, function(z) max(as.numeric(z), na.rm = TRUE)),
    period.apply(Lo(x), ep, function(z) min(as.numeric(z), na.rm = TRUE)),
    period.apply(Cl(x), ep, last_val)
  )
  if (has.Vo(x)) {
    out <- cbind(out, period.apply(Vo(x), ep, function(z) sum(as.numeric(z), na.rm = TRUE)))
  } else {
    out <- cbind(out, xts(rep(NA_real_, nrow(out)), order.by = index(out)))
  }
  colnames(out) <- c("Open", "High", "Low", "Close", "Volume")
  out
}

to_data_frame <- function(x) {
  data.frame(
    Date   = as.Date(index(x)),
    Open   = as.numeric(Op(x)),
    High   = as.numeric(Hi(x)),
    Low    = as.numeric(Lo(x)),
    Close  = as.numeric(Cl(x)),
    Volume = if (has.Vo(x)) as.numeric(Vo(x)) else NA_real_
  )
}

base_price_chart <- function(df, chart_type, time_frame, symbol) {

  half_width <- switch(time_frame, "Daily" = 0.35, "Weekly" = 2, "Monthly" = 9)

  p <- ggplot(df, aes(x = Date))

  if (chart_type == "Candlestick") {
    df$Direction <- ifelse(df$Close >= df$Open, "Up", "Down")
    p <- ggplot(df, aes(x = Date)) +
      geom_segment(aes(xend = Date, y = Low, yend = High), colour = "grey30",
                   linewidth = 0.4) +
      geom_rect(aes(xmin = Date - half_width, xmax = Date + half_width,
                    ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                    fill = Direction), colour = "grey30", linewidth = 0.2) +
      scale_fill_manual(values = c(Up = "#2e9e5b", Down = "#d64545"), name = "Candle")
  } else if (chart_type == "Area") {
    p <- p +
      geom_area(aes(y = Close), fill = "#9ecae1", alpha = 0.6) +
      geom_line(aes(y = Close), colour = "#2171b5", linewidth = 0.7)
  } else {
    p <- p + geom_line(aes(y = Close), colour = "#2171b5", linewidth = 0.8)
  }

  p +
    labs(title = paste0(symbol, " Price (", time_frame, ", ", chart_type, ")"),
         x = NULL, y = "Price (USD)") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "bottom")
}

add_indicators <- function(df, short_n, long_n, rsi_n) {
  df$ShortMA <- SMA(df$Close, n = short_n)
  df$LongMA  <- SMA(df$Close, n = long_n)
  df$RSI     <- RSI(df$Close, n = rsi_n)

  macd_vals    <- MACD(df$Close, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
  df$MACD      <- macd_vals[, "macd"]
  df$MACDSig   <- macd_vals[, "signal"]
  df$MACDHist  <- df$MACD - df$MACDSig
  df
}

overlay_moving_averages <- function(p, df, short_n, long_n) {
  ma_long <- rbind(
    data.frame(Date = df$Date, Value = df$ShortMA, Line = paste0("SMA ", short_n)),
    data.frame(Date = df$Date, Value = df$LongMA,  Line = paste0("SMA ", long_n))
  )
  ma_long <- ma_long[!is.na(ma_long$Value), ]
  ma_colours <- setNames(c("#ff7f0e", "#6a3d9a"),
                         c(paste0("SMA ", short_n), paste0("SMA ", long_n)))

  p +
    geom_line(data = ma_long, aes(x = Date, y = Value, colour = Line),
              linewidth = 0.9, inherit.aes = FALSE) +
    scale_colour_manual(values = ma_colours, name = "Moving averages")
}

rsi_chart <- function(df, rsi_n, x_limits) {
  ggplot(df, aes(x = Date, y = RSI)) +
    annotate("rect", xmin = x_limits[1], xmax = x_limits[2], ymin = 30, ymax = 70,
             fill = "grey80", alpha = 0.35) +
    geom_hline(yintercept = c(30, 70), linetype = "dashed", colour = "grey40") +
    geom_line(colour = "#8c2d9c", linewidth = 0.8, na.rm = TRUE) +
    scale_y_continuous(limits = c(0, 100), breaks = c(0, 30, 50, 70, 100)) +
    scale_x_date(limits = x_limits) +
    labs(title = paste0("RSI (", rsi_n, ")"), x = NULL, y = "RSI") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
}

macd_chart <- function(df, x_limits) {
  df$HistColour <- ifelse(df$MACDHist >= 0, "Positive", "Negative")
  ggplot(df, aes(x = Date)) +
    geom_col(aes(y = MACDHist, fill = HistColour), alpha = 0.6, na.rm = TRUE) +
    geom_line(aes(y = MACD, colour = "MACD"), linewidth = 0.8, na.rm = TRUE) +
    geom_line(aes(y = MACDSig, colour = "Signal"), linewidth = 0.8, na.rm = TRUE) +
    scale_fill_manual(values = c(Positive = "#2e9e5b", Negative = "#d64545"),
                      name = "Histogram") +
    scale_colour_manual(values = c(MACD = "#1f77b4", Signal = "#ff7f0e"), name = NULL) +
    scale_x_date(limits = x_limits) +
    labs(title = "MACD (12, 26, 9)", x = NULL, y = "MACD") +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
}

generate_signals <- function(df, use_rsi_filter = FALSE,
                             overbought = 70, oversold = 30) {
  position <- ifelse(is.na(df$ShortMA) | is.na(df$LongMA), NA,
                     ifelse(df$ShortMA > df$LongMA, 1,
                            ifelse(df$ShortMA < df$LongMA, -1, 0)))

  prev_position <- c(NA, head(position, -1))

  signal <- rep("Hold", nrow(df))
  buy  <- !is.na(position) & !is.na(prev_position) & prev_position <= 0 & position == 1
  sell <- !is.na(position) & !is.na(prev_position) & prev_position >= 0 & position == -1

  if (use_rsi_filter) {
    buy  <- buy  & !is.na(df$RSI) & df$RSI < overbought
    sell <- sell & !is.na(df$RSI) & df$RSI > oversold
  }

  signal[buy]  <- "Buy"
  signal[sell] <- "Sell"
  signal
}

annotate_signals <- function(p, df) {
  buys  <- df[df$Signal == "Buy", ]
  sells <- df[df$Signal == "Sell", ]
  y_range <- diff(range(c(df$Low, df$High), na.rm = TRUE))
  offset  <- y_range * 0.04

  if (nrow(buys) > 0) {
    p <- p +
      geom_point(data = buys, aes(x = Date, y = Low - offset),
                 shape = 24, size = 4, fill = "#1b9e3f", colour = "black",
                 inherit.aes = FALSE) +
      geom_text(data = buys, aes(x = Date, y = Low - offset * 2.6, label = "Buy"),
                colour = "#1b9e3f", fontface = "bold", size = 4, inherit.aes = FALSE)
  }
  if (nrow(sells) > 0) {
    p <- p +
      geom_point(data = sells, aes(x = Date, y = High + offset),
                 shape = 25, size = 4, fill = "#d62728", colour = "black",
                 inherit.aes = FALSE) +
      geom_text(data = sells, aes(x = Date, y = High + offset * 2.6, label = "Sell"),
                colour = "#d62728", fontface = "bold", size = 4, inherit.aes = FALSE)
  }
  p
}

ui <- fluidPage(
  titlePanel("Portfolio Dashboard: Technical Analysis"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Data"),
      textInput("symbol", "Stock Symbol:", value = stock_symbol),
      dateRangeInput("date_range", "Select Date Range:",
                     start = start_date, end = end_date),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly")),
      selectInput("chart_type", "Chart Type:",
                  choices = c("Candlestick", "Line", "Area")),
      actionButton("load_data", "Load Data", class = "btn-primary"),

      hr(),
      h4("Technical Indicators"),
      checkboxGroupInput("technical_indicators", "Show on chart:",
                         choices = c("Moving Averages", "RSI", "MACD"),
                         selected = c("Moving Averages")),
      numericInput("short_n", "Short MA period:", value = 20, min = 2, max = 100),
      numericInput("long_n",  "Long MA period:",  value = 50, min = 3, max = 250),
      numericInput("rsi_n",   "RSI period:",      value = 14, min = 2, max = 50),

      hr(),
      h4("Trading Rules"),
      checkboxInput("show_signals", "Show Buy / Sell annotations", value = TRUE),
      checkboxInput("use_rsi_filter", "Confirm signals with RSI", value = FALSE),
      conditionalPanel(
        condition = "input.use_rsi_filter",
        sliderInput("rsi_levels", "RSI oversold / overbought:",
                    min = 10, max = 90, value = c(30, 70))
      )
    ),

    mainPanel(
      width = 9,
      textOutput("data_info"),
      plotOutput("stock_chart", height = "450px"),
      conditionalPanel(
        condition = "input.technical_indicators.indexOf('RSI') > -1",
        plotOutput("rsi_chart", height = "200px")
      ),
      conditionalPanel(
        condition = "input.technical_indicators.indexOf('MACD') > -1",
        plotOutput("macd_chart", height = "230px")
      ),
      h4("Signal Summary"),
      textOutput("signal_counts"),
      tableOutput("signal_table")
    )
  )
)

server <- function(input, output, session) {

  raw_data <- eventReactive(input$load_data, {
    validate(need(nchar(trimws(input$symbol)) > 0, "Please enter a stock symbol."))
    validate(need(input$date_range[1] < input$date_range[2],
                  "The start date must be before the end date."))

    result <- withProgress(message = "Downloading stock data...", {
      fetch_stock(input$symbol, input$date_range[1], input$date_range[2])
    })
    validate(need(!is.null(result),
                  paste0("Could not get data for '", toupper(input$symbol),
                         "'. Check the symbol, the dates and your internet connection.")))
    result
  }, ignoreNULL = FALSE)

  analysis <- reactive({
    res <- raw_data()

    validate(need(input$short_n < input$long_n,
                  "The short MA period must be smaller than the long MA period."))

    tf_data <- change_time_frame(res$data, input$time_frame)
    df <- to_data_frame(tf_data)
    validate(need(nrow(df) > max(input$long_n, input$rsi_n, 35),
                  paste0("Not enough ", tolower(input$time_frame),
                         " bars for these indicator periods. Try smaller periods or a shorter time frame.")))
    df <- add_indicators(df, input$short_n, input$long_n, input$rsi_n)

    levels <- if (isTRUE(input$use_rsi_filter)) input$rsi_levels else c(30, 70)
    df$Signal <- generate_signals(df, isTRUE(input$use_rsi_filter),
                                  overbought = levels[2], oversold = levels[1])

    df <- df[df$Date >= input$date_range[1] & df$Date <= input$date_range[2], ]
    validate(need(nrow(df) > 1, "Not enough data in this range. Try a wider date range."))

    list(df = df, source = res$source, symbol = res$symbol)
  })

  x_limits <- reactive({
    df <- analysis()$df
    pad <- switch(input$time_frame, "Daily" = 2, "Weekly" = 7, "Monthly" = 30)
    c(min(df$Date) - pad, max(df$Date) + pad)
  })

  output$data_info <- renderText({
    a <- analysis()
    paste0("Source: ", a$source, "  |  Symbol: ", a$symbol,
           "  |  Bars shown: ", nrow(a$df))
  })

  output$stock_chart <- renderPlot({
    a <- analysis()
    df <- a$df

    p <- base_price_chart(df, input$chart_type, input$time_frame, a$symbol)

    if ("Moving Averages" %in% input$technical_indicators) {
      p <- overlay_moving_averages(p, df, input$short_n, input$long_n)
    }
    if (isTRUE(input$show_signals)) {
      p <- annotate_signals(p, df)
    }
    p + scale_x_date(limits = x_limits())
  })

  output$rsi_chart <- renderPlot({
    req("RSI" %in% input$technical_indicators)
    rsi_chart(analysis()$df, input$rsi_n, x_limits())
  })

  output$macd_chart <- renderPlot({
    req("MACD" %in% input$technical_indicators)
    macd_chart(analysis()$df, x_limits())
  })

  output$signal_counts <- renderText({
    s <- analysis()$df$Signal
    paste0("Buy: ", sum(s == "Buy"), "  |  Sell: ", sum(s == "Sell"),
           "  |  Hold: ", sum(s == "Hold"))
  })

  output$signal_table <- renderTable({
    df <- analysis()$df
    out <- df[df$Signal != "Hold", c("Date", "Close", "Signal")]
    if (nrow(out) == 0) {
      return(data.frame(Message = "No Buy or Sell signals in this range."))
    }
    out$Date  <- format(out$Date, "%Y-%m-%d")
    out$Close <- round(out$Close, 2)
    out
  })
}

shinyApp(ui, server)
