shinyUI(pageWithSidebar(

  headerPanel("Title"),
  
  sidebarPanel(
    sliderInput(inputId = "opt.cex",
                label = "Point Size (cex)",                            
                min = 0, max = 2, step = 0.25, value = 1),
    sliderInput(inputId = "opt.cexaxis",
                label = "Axis Text Size (cex.axis)",                            
                min = 0, max = 2, step = 0.25, value = 1) 
  ),
  
  mainPanel(
    plotOutput(outputId = "main_plot",  width = "100%"),
    plotOutput(outputId = "main_plot2", width = "100%")
  )
))