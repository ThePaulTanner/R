library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("Stock Info"),
  
  sidebarLayout(
    sidebarPanel(
        
     # selectInput("Stockname", "Stock name:" , choices = stocksOfInterest[]),    
      dateRangeInput("daterange", "Date range:",
                     start  = stocksOfInterestDataSet[[1]][nrow(stocksOfInterestDataSet[[1]]),1],
                     end    = Sys.Date(),
                     min    = stocksOfInterestDataSet[[1]][nrow(stocksOfInterestDataSet[[1]]),1],
                     max    = Sys.Date(),
                     format = "dd/mm/yyyy",
                     separator = " to ")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))