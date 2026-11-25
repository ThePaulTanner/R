# Stock Analyser

A rewrite of `my_app_03`: a Shiny app that backtests a moving-average crossover strategy against buy-and-hold.

- Data: download from Stooq (no API key), upload a CSV (Date, Close), or use simulated demo data.
- Strategy: hold while the fast average is above the slow one; trades happen the day after the signal.
- Logic lives in `R/backtest.R`; tests in `tests/test_backtest.R`.

```r
install.packages(c("shiny", "zoo", "testthat"))
shiny::runApp("stock_analyser")
```
Tests: `Rscript tests/test_backtest.R` from this folder.
