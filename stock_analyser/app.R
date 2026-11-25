# Stock Analyser: moving-average crossover backtester.
# Run with: shiny::runApp("stock_analyser")
library(shiny)
source("R/backtest.R", local = TRUE)

ui <- fluidPage(
  titlePanel("Stock Analyser"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("source", "Data", c("Download (Stooq)" = "web",
                                        "Upload CSV" = "csv",
                                        "Demo (simulated)" = "demo")),
      conditionalPanel("input.source == 'web'",
        textInput("symbol", "Ticker (US)", "AAPL")),
      conditionalPanel("input.source == 'csv'",
        fileInput("file", "CSV with Date and Close columns", accept = ".csv")),
      sliderInput("fast", "Fast average (days)", 2, 100, 20),
      sliderInput("slow", "Slow average (days)", 5, 400, 100),
      numericInput("capital", "Starting capital ($)", 10000, min = 1),
      helpText("Strategy: hold the stock while the fast average is above the slow one.",
               "Signals are acted on the next day. Past performance is not advice.")
    ),
    mainPanel(
      tableOutput("summary"),
      plotOutput("price"),
      plotOutput("equity")
    )
  )
)

server <- function(input, output, session) {
  prices <- reactive({
    switch(input$source,
      demo = demo_prices(),
      csv  = { req(input$file); validate_prices(read.csv(input$file$datapath)) },
      web  = { req(nzchar(input$symbol)); fetch_prices(trimws(input$symbol)) })
  })

  result <- reactive({
    validate(need(input$slow > input$fast, "Slow average must be longer than fast average."))
    p <- tryCatch(prices(), error = function(e) NULL)
    validate(need(!is.null(p), "Could not load data. Check the ticker or file, or try the demo."))
    validate(need(nrow(p) > input$slow, "Not enough data for that slow-average length."))
    backtest(p, input$fast, input$slow, input$capital)
  })

  output$summary <- renderTable({
    s <- summarise_backtest(result())
    data.frame(
      ` ` = c("Strategy", "Buy & hold"),
      `Final value` = sprintf("$%.0f", c(s$strategy_final, s$buyhold_final)),
      `Annual return` = sprintf("%.1f%%", 100 * c(s$strategy_cagr, s$buyhold_cagr)),
      Trades = c(s$trades, 1), check.names = FALSE
    )
  })

  output$price <- renderPlot({
    d <- result()
    plot(d$Date, d$Close, type = "l", xlab = "Date", ylab = "Close", main = "Price and averages")
    lines(d$Date, d$Fast, col = "green3", lwd = 2)
    lines(d$Date, d$Slow, col = "red", lwd = 2)
    legend("topleft", c("Close", "Fast", "Slow"), col = c("black", "green3", "red"), lty = 1, bty = "n")
  })

  output$equity <- renderPlot({
    d <- result()
    matplot(d$Date, cbind(d$Strategy, d$BuyHold), type = "l", lty = 1, lwd = 2,
            col = c("blue", "grey40"), xlab = "Date", ylab = "Portfolio value ($)",
            main = "Strategy vs buy & hold")
    legend("topleft", c("Strategy", "Buy & hold"), col = c("blue", "grey40"), lty = 1, bty = "n")
  })
}

shinyApp(ui, server)
