# Pure functions: data loading and the backtest. No Shiny code here, so they can be tested.

#' Download daily prices from Stooq (free, no API key). Returns Date + OHLCV data frame.
fetch_prices <- function(symbol, exchange = "us") {
  url <- sprintf("https://stooq.com/q/d/l/?s=%s.%s&i=d", tolower(symbol), exchange)
  df <- read.csv(url, stringsAsFactors = FALSE)
  validate_prices(df)
}

#' Simulated prices (geometric random walk) so the app works offline.
demo_prices <- function(n = 1500, seed = 1) {
  set.seed(seed)
  close <- 100 * exp(cumsum(rnorm(n, 0.0004, 0.015)))
  data.frame(
    Date = seq(Sys.Date() - n + 1, Sys.Date(), by = "day"),
    Open = close, High = close * 1.01, Low = close * 0.99, Close = close,
    Volume = 1e6
  )
}

#' Check/normalise a price table: needs Date and Close columns; sorted oldest -> newest.
validate_prices <- function(df) {
  names(df) <- tools::toTitleCase(tolower(names(df)))
  if (!all(c("Date", "Close") %in% names(df))) {
    stop("Data needs 'Date' and 'Close' columns.")
  }
  df$Date  <- as.Date(df$Date)
  df$Close <- suppressWarnings(as.numeric(df$Close))
  df <- df[!is.na(df$Date) & !is.na(df$Close), , drop = FALSE]
  if (nrow(df) < 2) stop("No usable price rows found.")
  df[order(df$Date), , drop = FALSE]
}

#' Backwards-looking simple moving average (NA until `n` observations exist).
sma <- function(x, n) {
  as.numeric(zoo::rollmeanr(x, n, fill = NA))
}

#' Moving-average crossover backtest, long-only.
#' Hold the stock while fast SMA > slow SMA. The signal is lagged one day so a
#' trade is only made on information available at the previous close (no look-ahead).
#' Returns the data with signal, strategy and buy-and-hold equity columns.
backtest <- function(prices, fast, slow, capital = 10000) {
  stopifnot(fast >= 1, slow > fast, nrow(prices) > slow)
  d <- prices
  d$Fast <- sma(d$Close, fast)
  d$Slow <- sma(d$Close, slow)
  want   <- !is.na(d$Fast) & !is.na(d$Slow) & d$Fast > d$Slow
  d$Position <- c(FALSE, head(want, -1))               # act next day
  ret <- c(0, diff(d$Close) / head(d$Close, -1))        # daily returns
  d$Strategy <- capital * cumprod(1 + ifelse(d$Position, ret, 0))
  d$BuyHold  <- capital * d$Close / d$Close[1]
  d
}

#' Summary numbers for a backtest result.
summarise_backtest <- function(bt) {
  last <- nrow(bt)
  trades <- sum(diff(as.integer(bt$Position)) == 1)
  years <- as.numeric(diff(range(bt$Date))) / 365.25
  cagr <- function(end, start) (end / start)^(1 / max(years, 1e-9)) - 1
  list(
    strategy_final = bt$Strategy[last],
    buyhold_final  = bt$BuyHold[last],
    strategy_cagr  = cagr(bt$Strategy[last], bt$Strategy[1]),
    buyhold_cagr   = cagr(bt$BuyHold[last],  bt$BuyHold[1]),
    trades = trades
  )
}
