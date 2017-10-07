shinyServer(function(input, output) {
  
  output$DropDownStockChoice <- renderUI({
    selectInput("Stockname", "Stock name:" , choices = stocksOfInterest[])
  })
  
  output$dateRange <- renderUI({
          dateRangeInput("daterange", "Date range:",
                 start  = stocksOfInterestDataSet[[match(input$Stockname, stocksOfInterest[])]][nrow(stocksOfInterestDataSet[[match(input$Stockname, stocksOfInterest[])]]),1],
                 end    = Sys.Date(),
                 min    = stocksOfInterestDataSet[[match(input$Stockname, stocksOfInterest[])]][nrow(stocksOfInterestDataSet[[match(input$Stockname, stocksOfInterest[])]]),1],
                 max    = Sys.Date(),
                 format = "dd/mm/yyyy",
                 separator = " to ")
  })
  
  output$distPlot <- renderPlot
  ({   
    p = 1
      #match(input$Stockname, stocksOfInterest[])#plot which data set from stocksOfInterest[[i]] list
    title <- stocksOfInterest[[1]]
    xaxislabel <- "Date"
    yaxislabel <- "Volitility"
    plot(stocksOfInterestDataSet[[1]]$Date,stocksOfInterestDataSet[[1]]$Low, col='red', xlab=xaxislabel, ylab=yaxislabel, main=title)
  })
  
  
})