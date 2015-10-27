
# TAREA 3. MONTECARLO
# SERVER CODE
# KAREN POBLETE 116452
# 31/AGO/2015
#

library(shiny)

shinyServer(function(input, output) {
  
  output$valReal <- renderText({ 
    paste(HTML("El valor de fm(x)=m*exp(-mx^2) de [0,2]: "), 1 - exp(-2*input$m))
  })
  
  output$monteCarloCrudo <- renderPlot({
    estim <- rep(0,input$rep)
    phi<-function(x){ 2 * input$m * exp(- input$m * x)}
    for(i in 1:input$rep){
      U<-runif(input$N,0,2)
      estim[i] <- mean(phi(U))
    }
    
    bins <- seq(min(estim), max(estim), length.out = input$bins + 1)
    hist(estim, breaks = bins, col = 'darkgray', border = 'white', main = paste("MonteCarlo crudo, con repeticiones: ", input$rep))
    
    alpha <- 0.05
    int.quant <- quantile(estim, c(alpha/2, 1- alpha/2))
    output$alphaCrudo <- renderText({
      paste(paste(paste("Intervalo de confianza con alpha 0.05, percentiles:", int.quant[1])," , "), int.quant[2])
    })
  })
  
  output$monteCarloCrudoError <- renderPlot({
    valor <- 1 - exp(-2*input$m)
    error <- rep(0,input$N)
    phi<-function(x){ 2 * input$m * exp(- input$m * x)}
    for(i in 1:input$N){
      U<-runif(i,0,2)
      error[i] <- abs (mean(phi(U)) - valor)
    }
    muestras <- seq(1, input$N, 1) 
    plot(muestras,error, main = "MonteCarlo Error ", type="l",col="red")
  })
  
  output$monteCarlo <- renderPlot({
    #estim2 <- mean(phi(U))
    estim <- rep(0,input$rep)
    #La densidad de la exponencial truncada
    fun <- function(x) {dexp(x * input$lambda)/(1-exp(-2 * input$lambda))}
    #Montecarlo
    phi <- function(x) {input$m * exp(- input$m * x )/fun(x)}

    for(i in 1:input$rep){
      U<-runif(input$N,0,2)
      #exponencial(1) truncada a [0,2]
      X <- -log(1 - (1 - (1-exp(-2))*U))
      estim[i] <- mean(phi(U))
    }
    
    bins <- seq(min(estim), max(estim), length.out = input$bins + 1)
    hist(estim, breaks = bins, col = 'darkgray', border = 'white', main = paste("MonteCarlo, con repeticiones: ", input$rep))
    
    alpha <- 0.05
    int.quant <- quantile(estim, c(alpha/2, 1- alpha/2))
    output$alphaMonte <- renderText({
      paste(paste(paste("Intervalo de confianza con alpha 0.05, percentiles:", int.quant[1])," , "), int.quant[2])
    })
  })
  
  output$monteCarloError <- renderPlot({
    valor <- 1 - input$m * exp(- input$m * x)
    #estim2 <- mean(phi(U))
    error <- rep(0,input$N)
    #La densidad de la exponencial truncada
    fun <- function(x) {dexp(x)/(1-exp(-2))}
    #Montecarlo
    phi <- function(x) {input$m * exp(- input$m * x * input$lambda)/fun(x)}
    
    for(i in 1:input$N){
      U<-runif(i,0,2)
      #exponencial(1) truncada a [0,2]
      X <- -log(1 - (1 - (1-exp(-2))*U))
      error[i] <- mean(phi(U)) - valor
    }
    
    muestras <- seq(1, input$num, 1) 
    plot(muestras,error, main = "MonteCarlo Error ", type="l",col="red")
    
  })
  
})






