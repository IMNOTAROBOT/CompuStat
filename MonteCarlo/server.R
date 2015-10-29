
# TAREA 3. MONTECARLO
# SERVER CODE
# KAREN POBLETE 116452
# 31/AGO/2015
#

library(shiny)

shinyServer(function(input, output) {
  
  output$valReal <- renderText({ 
    paste(HTML("El valor de fm(x)=m*exp(-mx) de [0,2]: "), 1 - exp(-2*input$m))
  })
  
  output$monteCarloCrudo <- renderPlot({
    estim <- rep(0,input$rep)
    phi<-function(x){ 
      2 * input$m * exp(- input$m * x)
    }
    
    for(i in 1:input$rep){
      U<-runif(input$N,0,2)
      estim[i] <- mean(phi(U))
    }
    
    hist(estim, col = 'darkgray', breaks = 10 ,border = 'white', main = paste("MonteCarlo crudo, con repeticiones: ", input$rep))
    alpha <- input$alpha
    int.quant <- quantile(estim, c(alpha/2, 1- alpha/2))
    abline(v = c(int.quant[1], int.quant[2], mean(estim)), col = "blue", lty = 3)
    abline(v = 1 - exp(-2*input$m), col = "red", lty = 3)
    
    output$alphaCrudo <- renderText({
      paste(paste(paste("Intervalo de confianza, percentiles:", int.quant[1])," , "), int.quant[2])
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
    estim <- rep(0,input$rep)
    fun <- function(x) {dexp(x, rate = input$lambda)/(1 - exp(-2*input$lambda))}
    #phi <- function(x) {input$m * dexp(x)/fun(x)}
    phi <- function(x) {input$m * dexp(x,rate = 2 * input$m)/fun(x)}
    #- input$m * 
    for(i in 1:input$rep){
      U<-runif(nsim,0,2)
      X<- abs(-log(1 - (1 - (1-exp(-2))*U)))
      fg <- phi(X)
      estim[i] <- mean(fg)
    }
    
    hist(estim, col = 'darkgray', border = 'white', main = paste("MonteCarlo, con repeticiones: ", input$rep))
    alpha <- 0.05
    int.quant <- quantile(estim, c(alpha/2, 1- alpha/2))
    abline(v = c(int.quant[1], int.quant[2], mean(estim)), col = "blue", lty = 3)
    abline(v = 1 - exp(-2*input$m), col = "red", lty = 3)
    
    output$alphaMonte <- renderText({
      paste(paste(paste("Intervalo de confianza con alpha 0.05, percentiles:", int.quant[1])," , "), int.quant[2])
    })
  })
  
  output$monteCarloError <- renderPlot({
    valor <- 1 - input$m * exp(-1)
    error <- rep(0,input$N)
    phi <- function(x) {input$m * exp(- input$m * x )}
    for(i in 1:input$N){
      X <- rnorm(i, mean = 0, sd = 1)
      fg <- phi(X)/dnorm(X,mean=0, sd =1)
      error[i] <- mean(fg) -valor
    }
    
    muestras <- seq(1, input$N, 1) 
    plot(muestras,error, main = "MonteCarlo Error ", type="l",col="red")
    
  })
  
})






