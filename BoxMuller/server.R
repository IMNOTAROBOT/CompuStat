
# TAREA 2. BOX MULLER
# SERVER CODE
# KAREN POBLETE 116452
# 25/AGO/2015
#

library(shiny)

shinyServer(function(input, output) {

  output$boxmuller <- renderPlot({
    theta <- runif(input$num, min = 0, max = 2*pi)
    u <- runif(input$num, min = 0, max = 1)
    k <- log(u)*(-2)
    r <- sqrt(k)
    x <- r * cos(theta)
    y <- r * sin(theta)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # draw the histogram with the specified number of bins
    
    hist(x, breaks = bins, col = 'darkgray', border = 'white', main = "BoxMuller X")
    
    output$boxmullerY <- renderPlot({
      bins <- seq(min(y), max(y), length.out = input$bins + 1)
      hist(y, breaks = bins, col = 'darkgray', border = 'white', main = "BoxMuller Y")
    })
    output$tableX <- renderDataTable({data.frame(x = x)})
    output$tableY <- renderDataTable({data.frame(y = y)})
    
    #Pruebas de ajuste shapiro-Wilk
    output$studentX <- renderPlot({
      plot(density(x))
      shapiro.test(x)
      qqnorm(x);qqline(x, col = 2)
    })
    
    output$studentY <- renderPlot({
      plot(density(y))
      shapiro.test(y)
      qqnorm(y);qqline(y, col = 2)
    })
  })

})
