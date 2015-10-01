
# TAREA 3. MONTECARLO
# SERVER CODE
# KAREN POBLETE 116452
# 31/AGO/2015
#

library(shiny)

shinyServer(function(input, output) {
  
  output$valReal <- renderText({ 
    paste("El valor real de la normal de [0,2]:", pnorm(2)-1/2)
  })
  
  output$monteCarloCrudo <- renderPlot({
    estim <- rep(0,input$rep)
    phi<-function(x){ 2*dnorm(x)}
    for(i in 1:input$rep){
      U<-runif(input$num,0,2)
      estim[i] <- mean(phi(U))
    }
    
    bins <- seq(min(estim), max(estim), length.out = input$bins + 1)
    hist(estim, breaks = bins, col = 'darkgray', border = 'white', main = paste("MonteCarlo crudo, con repeticiones: ", input$rep))
    
  })
  
  output$monteCarloCrudoError <- renderPlot({
    valor <- pnorm(2)-1/2
    error <- rep(0,input$num)
    phi<-function(x){ 2*dnorm(x)}
    for(i in 1:input$num){
      U<-runif(i,0,2)
      error[i] <- mean(phi(U)) - valor
    }
    muestras <- seq(1, input$num, 1) 
    plot(muestras,error, main = "MonteCarlo Error ", type="l",col="red")
  })
  
  output$monteCarlo <- renderPlot({
    #estim2 <- mean(phi(U))
    estim <- rep(0,input$rep)
    #La densidad de la exponencial truncada
    fun <- function(x) dexp(x)/(1-exp(-2))
    #Montecarlo
    phi <- function(x) dnorm(x)/fun(x)

    for(i in 1:input$rep){
      U<-runif(input$num,0,2)
      #exponencial(1) truncada a [0,2]
      X<- -log(1 - (1 - (1-exp(-2))*U))
      estim[i] <- mean(phi(U))
    }
    
    bins <- seq(min(estim), max(estim), length.out = input$bins + 1)
    hist(estim, breaks = bins, col = 'darkgray', border = 'white', main = paste("MonteCarlo, con repeticiones: ", input$rep))
    
  })
  
  output$monteCarloError <- renderPlot({
    valor <- pnorm(2)-1/2
    #estim2 <- mean(phi(U))
    error <- rep(0,input$num)
    #La densidad de la exponencial truncada
    fun <- function(x) dexp(x)/(1-exp(-2))
    #Montecarlo
    phi <- function(x) dnorm(x)/fun(x)
    
    for(i in 1:input$num){
      U<-runif(i,0,2)
      #exponencial(1) truncada a [0,2]
      X<- -log(1 - (1 - (1-exp(-2))*U))
      error[i] <- mean(phi(U)) - valor
    }
    
    muestras <- seq(1, input$num, 1) 
    plot(muestras,error, main = "MonteCarlo Error ", type="l",col="red")
    
  })
  
})






