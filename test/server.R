shinyServer(function(input, output) {

  x <- 1:10
  y <- x^2
  
  output$main_plot <- renderPlot({    
    plot(x, y)}, height = 200, width = 300)
  
  
  output$main_plot2 <- renderPlot({
    plot(x, y, cex=input$opt.cex, cex.lab=input$opt.cexaxis) }, height = 400, width = 600 )
} )