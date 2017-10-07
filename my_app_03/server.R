library(shinyapps)
library(shiny)

shinyServer(function(input, output)
{
  
     
  
  output$DropDownStockChoice <- renderUI({
    selectInput("Stockname", "Stock name:" , choices = stocksOfInterest[])
  })  
  
# output$dateRange <- renderUI({
#    w=0    
#    e=1
#    w <- match(input$Stockname, stocksOfInterest)
#    if(is.integer(w))
#    {
#      e=w
#    }
#    if(is.null(input$Stockname))      
#      return()
#    
#    dateRangeInput("Daterange", "Date range:",
#                   start  = stocksOfInterestDataSet[[e]][nrow(stocksOfInterestDataSet[[e]]),1],
#                   end    = Sys.Date(),
#                   min    = stocksOfInterestDataSet[[e]][nrow(stocksOfInterestDataSet[[e]]),1],
#                   max    = Sys.Date(),
#                   format = "dd/mm/yyyy",
#                   separator = " to ")
#  })

  output$investmentday <- renderUI({
        w=0    
        e=1
        w <- match(input$Stockname, stocksOfInterest)
        if(is.integer(w))
        {
          e=w
        }
        if(is.null(input$Stockname))      
          return()
        
        dateInput(inputId = "InvestmentDay", "Investment Day ",
                  value = stocksOfInterestDataSet[[e]][nrow(stocksOfInterestDataSet[[e]]),1],
                  min = stocksOfInterestDataSet[[e]][nrow(stocksOfInterestDataSet[[e]]),1],
                  max = Sys.Date(),
                  format = "dd/mm/yyyy")    
  })

  
  output$FirstSliderValue <- renderUI({     
    #Buy
    for(j in 1:nrow(stocksOfInterestDataSet[[i]]))
    {   
      stocksOfInterestDataSet[[i]][j,(10+(2*q))] <- 1   
    }      
    
    sliderInput("FirstsliderValue", "Memory of Rolling Average",
                min     = 1,
                max     = q,
                value   = 1,
                step = 1
               )    
  })
 
  output$SecondSliderValue <- renderUI({     
    sliderInput("secondsliderValue", "Memory of Rolling Average",
                min     = 1,
                max     = q,
                value   = 1,
                step = 1
    )
  })  
  
  output$distPlot <- renderPlot({ 
    w=0    
    e=1
    w <- match(input$Stockname, stocksOfInterest)
    if(is.integer(w))
    {
      e=w
    }
    
    if(is.null(input$Stockname))      
    return()
    if(is.null(input$FirstsliderValue))      
    return()
    if(is.null(input$InvestmentDay))
    return() 

     LeftRowValue  <- match(as.Date(input$InvestmentDay,"%Y-%m-%d"), c(stocksOfInterestDataSet[[e]][0:nrow(stocksOfInterestDataSet[[e]]),1]))
     RightRowValue <- match(as.Date(input$InvestmentDay,"%Y-%m-%d") + MinimumInvestmentTime, c(stocksOfInterestDataSet[[e]][0:nrow(stocksOfInterestDataSet[[e]]),1]))           


    ii = 0
    jj = 0   
    while(is.na(RightRowValue))
    {     
     # RightRowValue <-match(as.Date(input$Daterange[2],"%Y-%m-%d")-ii, c(stocksOfInterestDataSet[[e]][0:nrow(stocksOfInterestDataSet[[e]]),1]))   
      RightRowValue <-match(as.Date(input$InvestmentDay,"%Y-%m-%d")-ii + MinimumInvestmentTime, c(stocksOfInterestDataSet[[e]][0:nrow(stocksOfInterestDataSet[[e]]),1]))   
      ii=ii+1     
    }
    
    while(is.na(LeftRowValue))
    {     
        LeftRowValue <- match(as.Date(input$InvestmentDay,"%Y-%m-%d")+jj, c(stocksOfInterestDataSet[[e]][0:nrow(stocksOfInterestDataSet[[e]]),1]))   
        if(jj<nrow(stocksOfInterestDataSet[[e]]))
        {
          jj=jj+1
        }
        
        if(jj == nrow(stocksOfInterestDataSet[[e]]))
        {
          jj=0   
          LeftRowValue = 2 
        }      
    }

   title <- paste(stocksOfInterest[[e]], "High")
   xaxislabel <- "Date"
   yaxislabel <- "Highest Price of the day"
   xx <- c(stocksOfInterestDataSet[[e]][LeftRowValue:RightRowValue,1])
   yy <- c(stocksOfInterestDataSet[[e]][LeftRowValue:RightRowValue,3])
   plot (xx, yy, col='black',cex=1, xlab=xaxislabel, ylab=yaxislabel, main=title)
   lines(xx, yy, col='black',lwd=2) 
   
   yyy <- c(stocksOfInterestDataSet[[e]][LeftRowValue:RightRowValue,(input$FirstsliderValue+10)])
   lines(xx, yyy, col='green') 
   yyyy <- c(stocksOfInterestDataSet[[e]][LeftRowValue:RightRowValue,(input$secondsliderValue+10+q)])
   lines(xx, yyyy, col='red')
 }) 
   
output$distPlot2 <- renderPlot({ 
   w=0    
   e=1
   w <- match(input$Stockname, stocksOfInterest)
   if(is.integer(w))
    {
     e=w
    }
   
   if(is.null(input$Stockname))      
     return()
#   if(is.null(input$Daterange[1]))
#     return()
#   if(is.null(input$Daterange[2]))
#     return()
   if(is.null(input$InvestmentDay))
     return() 
   
#   LeftRowValue  <- match(as.Date(input$Daterange[1],"%Y-%m-%d"), c(stocksOfInterestDataSet[[e]][0:nrow(stocksOfInterestDataSet[[e]]),1]))
#   RightRowValue <- match(as.Date(input$Daterange[2],"%Y-%m-%d"), c(stocksOfInterestDataSet[[e]][0:nrow(stocksOfInterestDataSet[[e]]),1]))           
    LeftRowValue  <- match(as.Date(input$InvestmentDay,"%Y-%m-%d"), c(stocksOfInterestDataSet[[e]][0:nrow(stocksOfInterestDataSet[[e]]),1]))
    RightRowValue <- match(as.Date(input$InvestmentDay,"%Y-%m-%d") + MinimumInvestmentTime, c(stocksOfInterestDataSet[[e]][0:nrow(stocksOfInterestDataSet[[e]]),1]))           


   ii=0
   jj=0   
   while(is.na(RightRowValue))
   {     
#     RightRowValue <-match(as.Date(input$Daterange[2],"%Y-%m-%d")-ii, c(stocksOfInterestDataSet[[e]][0:nrow(stocksOfInterestDataSet[[e]]),1]))   
      RightRowValue <-match(as.Date(input$InvestmentDay,"%Y-%m-%d")-ii + MinimumInvestmentTime, c(stocksOfInterestDataSet[[e]][0:nrow(stocksOfInterestDataSet[[e]]),1]))   
     
     ii=ii+1     
   }
   while(is.na(LeftRowValue))
   {     
#     LeftRowValue <- match(as.Date(input$Daterange[1],"%Y-%m-%d")+jj, c(stocksOfInterestDataSet[[e]][0:nrow(stocksOfInterestDataSet[[e]]),1]))   
      LeftRowValue <- match(as.Date(input$InvestmentDay,"%Y-%m-%d")+jj, c(stocksOfInterestDataSet[[e]][0:nrow(stocksOfInterestDataSet[[e]]),1]))   
     
     
     if(jj<nrow(stocksOfInterestDataSet[[e]]))
     {
       jj=jj+1
     }
     if(jj ==nrow(stocksOfInterestDataSet[[e]]))
     {
       jj=0   
       LeftRowValue = 2 
     }         
   }
   
   title <- paste(stocksOfInterest[[e]], "Low")
   xaxislabel <- "Date"
   yaxislabel <- "Lowest Price of the day"
   xx <- c(stocksOfInterestDataSet[[e]][LeftRowValue:RightRowValue,1])
   yy <- c(stocksOfInterestDataSet[[e]][LeftRowValue:RightRowValue,4])
   plot (xx, yy, col='red',cex=.5, xlab=xaxislabel, ylab=yaxislabel, main=title)
   lines(xx, yy, col='black') 
   yyy <- c(stocksOfInterestDataSet[[e]][LeftRowValue:RightRowValue,(input$secondsliderValue+10+q)])
   lines(xx, yyy, col='red')
   
  
  })

output$result1 <- renderPlot({
  
  
  
  
  
  })

output$Info <- renderText({
  w=0    
  e=1
  w <- match(input$Stockname, stocksOfInterest)
  if(is.integer(w))
  {
    e=w
  }
  
  if(is.null(input$FirstsliderValue))      
    return()
  if(is.null(input$secondsliderValue))      
    return()
  if(is.null(input$Stockname))      
    return()
  if(is.null(input$InvestmentDay))
    return()
    LeftRowValue  <- match(as.Date(input$InvestmentDay,"%Y-%m-%d"), c(stocksOfInterestDataSet[[e]][0:nrow(stocksOfInterestDataSet[[e]]),1]))
    RightRowValue <- match(as.Date(input$InvestmentDay,"%Y-%m-%d") + MinimumInvestmentTime, c(stocksOfInterestDataSet[[e]][0:nrow(stocksOfInterestDataSet[[e]]),1]))           

  ii=0
  jj=0  
  while(is.na(LeftRowValue) || Decisions[LeftRowValue,1,1,input$FirstsliderValue,input$secondsliderValue,e] == FALSE || Decisions[RightRowValue,1,2,input$FirstsliderValue,input$secondsliderValue,e] == TRUE)
  {     
      LeftRowValue <- match(as.Date(input$InvestmentDay,"%Y-%m-%d")+jj, c(stocksOfInterestDataSet[[e]][0:nrow(stocksOfInterestDataSet[[e]]),1]))   
      if(jj<nrow(stocksOfInterestDataSet[[e]]))
      {
        jj=jj+1
      }
      if(jj == nrow(stocksOfInterestDataSet[[e]]))
      {
        jj=0   
        LeftRowValue = 2 
      }      
  }
  while(is.na(RightRowValue) || Decisions[RightRowValue,1,2,input$FirstsliderValue,input$secondsliderValue,e] == FALSE || Decisions[RightRowValue,1,1,input$FirstsliderValue,input$secondsliderValue,e] == TRUE)
  {     
      RightRowValue <-match(as.Date(input$InvestmentDay,"%Y-%m-%d")+ii + MinimumInvestmentTime, c(stocksOfInterestDataSet[[e]][0:nrow(stocksOfInterestDataSet[[e]]),1]))   
      ii=ii+1  
  }
print(Decisions[RightRowValue,1,2,input$FirstsliderValue,input$secondsliderValue,e])
 
  for(j in LeftRowValue:RightRowValue)
  {
    #Buy
    if(StocksHeld == 0 & Decisions[j,1,1,input$FirstsliderValue,input$secondsliderValue,e] == TRUE)
    {
      
      print(paste("j:",j,"Buy :",stocksOfInterestDataSet[[i]][j,1]))
      StocksHeld = (Dollars/(stocksOfInterestDataSet[[input$Stockname]][j,3]))
      Dollars = 0           
    }
    #Sell
    if(Dollars == 0 & Decisions[j,1,2,input$FirstsliderValue,input$secondsliderValue,e] == TRUE)
    {
     
      print(paste("j:",j,"Sell :",stocksOfInterestDataSet[[i]][j,1]))
      Dollars = (StocksHeld*(stocksOfInterestDataSet[[input$Stockname]][j,4]))
      StocksHeld = 0      
    }
  }
  print(paste("LeftRowValue: ", LeftRowValue,"RightRowValue: " ,RightRowValue))
  
  TotalWealth <- Dollars + (StocksHeld*(stocksOfInterestDataSet[[input$Stockname]][j,4]))
  TheInfo = paste("Initial investment :" ,InitialInvestment ,"Dollars :" , Dollars, "Stocks held :" , StocksHeld, "Total wealth :", TotalWealth)
  TheInfo
})


})
