shinyUI(navbarPage("Paul's Stock Analyser",
  
  
  tabPanel("Buy",
           pageWithSidebar(  
             headerPanel(""),  
             sidebarPanel(
               uiOutput("DropDownStockChoice"),
               br(),
               uiOutput("investmentday"), 
               br(),
               uiOutput("FirstSliderValue"),               
               uiOutput("SecondSliderValue"),
               a()
             ),  
             mainPanel(
               
               plotOutput(outputId = "distPlot", width = "100%"),
               textOutput("Info")  
             )            
           )
  ),
  
  tabPanel("Sell",
           pageWithSidebar(  
             headerPanel(""),  
             sidebarPanel(
               
               a()
             ),  
             mainPanel(
               plotOutput(outputId = "distPlot2", width = "100%")    
             ) 
           )
  )
  )  
  
  ) 
  

  
