# Run with: Rscript tests/test_backtest.R   (from stock_analyser/)
library(testthat)
source("R/backtest.R")

mk <- function(close) data.frame(Date = as.Date("2020-01-01") + seq_along(close) - 1, Close = close)

test_that("sma is backwards looking", {
  expect_equal(sma(1:5, 2), c(NA, 1.5, 2.5, 3.5, 4.5))
})

test_that("rising market: strategy invests after the signal, with a one-day lag", {
  bt <- backtest(mk(1:30), fast = 2, slow = 5)
  expect_false(any(bt$Position[1:5]))
  expect_true(all(bt$Position[7:30]))
  expect_equal(bt$BuyHold[30], 10000 * 30)
  expect_lt(bt$Strategy[30], bt$BuyHold[30])   # missed the early gains
})

test_that("falling market: stays in cash", {
  bt <- backtest(mk(seq(100, 1, length.out = 30)), 2, 5)
  expect_equal(bt$Strategy[30], 10000)
})

test_that("signal never uses same-day information", {
  x <- c(rep(10, 10), 100, rep(10, 10))       # one-day spike
  bt <- backtest(mk(x), 1, 3)
  expect_false(bt$Position[11])               # spike day: not yet held
})

test_that("validation", {
  expect_error(validate_prices(data.frame(a = 1)))
  v <- validate_prices(data.frame(date = c("2020-01-02", "2020-01-01"), close = c(2, 1)))
  expect_equal(v$Close, c(1, 2))
  expect_error(backtest(mk(1:30), 5, 2))
})

test_that("demo + summary", {
  s <- summarise_backtest(backtest(demo_prices(), 20, 100))
  expect_true(all(is.finite(unlist(s))))
})
