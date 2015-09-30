
# Karen Poblete 116452
# Tarea 1. Funcion Inversa
#

library(shiny)

shinyServer(function(input, output) {
  
  
  output$distPlot <- renderPlot({
    u <- runif(input$num, min = 0, max = 1)
    x <- (1/input$lamda) * log(1/(1-u))
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "Exponencial")
    
    output$table <- renderDataTable({data.frame(x = x)})
  })
  
  output$mezcla <- renderPlot({
    mix <- read.table(input$file, header=TRUE, sep=',')
    totexp <- rep(0,dim(mix)[1])
    for(i in 1:dim(mix)[1]){
      u <- runif(input$num, min = 0, max = 1)
      x <- mix$Peso[i] * (1/mix$Lamda[i]) * log(1/(1-u))
      totexp <- totexp + x
    }
    bins <- seq(min(totexp), max(totexp), length.out = input$bins + 1)
    hist(totexp, breaks = bins, col = 'darkgray', border = 'white', main = "Mezcla")
    
    output$tableMix <- renderDataTable({data.frame(mezcla = totexp)})
  })
  
  
  
  
})
