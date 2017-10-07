library(shiny)

shinyServer(function(input, output) {
  
    
  output$distPlot <- renderPlot
  ({   
     p = match(input$Stockname, stocksOfInterest[])#plot which data set from stocksOfInterest[[i]] list
     title <- stocksOfInterest[[1]]
     xaxislabel <- "Date"
     yaxislabel <- "Volitility"
     plot(stocksOfInterestDataSet[[1]]$Date,stocksOfInterestDataSet[[1]]$Low, col='red', xlab=xaxislabel, ylab=yaxislabel, main=title)
    })
  
})