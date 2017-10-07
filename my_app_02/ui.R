shinyUI(pageWithSidebar(
  
  headerPanel("Stock Price"),
  
  sidebarPanel(
    uiOutput("DropDownStockChoice"),
    br(),
    uiOutput("dateRange"), 
    br(),
    a(href = "https://gist.github.com/4211337", "Source code")
  ),
  
  
  mainPanel(
    plotOutput("distPlot")
  )
  
))