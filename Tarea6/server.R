# TAREA 6. GIBBS SAMPLING
# SERVER CODE
# KAREN POBLETE 116452
# 17/NOV/2015
#

library(shiny)

shinyServer(function(input, output) {
  #Leer la tabla de los datos utilizados
  data(iris)
  
  #Mostrar los datos utilizados para el ejemplo
  output$DatosIRIS <- renderDataTable({data.frame(iris)})
  
  #Para calcular la media de Sepal.length: 5.843333
  #La varianza: 0.6856935
  output$meanSEPALLENGTH <- renderText({ 
    paste("media muestral: ", mean(iris$Sepal.Length))
  })
  
  output$varSEPALLENGTH <- renderText({ 
    paste("varianza muestral: ", var(iris$Sepal.Length))
  })
  
  #Graficamos correlacion entre el numero de crimenes de 1960 y el gasto policiaco
  output$IRISPLOT <- renderPlot({
    #Usando GIBBS generamos valores condiciones de sigma^2 y mu usando los datos de iris$Sepal.Length
    x<-iris$Sepal.Length
    #mu0 = 0, sig0 = 1
    mu <- matrix(0,dim(iris)[1])
    sig <- matrix(1,dim(iris)[1])
    for(i in 2:dim(iris)[1]) {
      sig[i] <- rgamma(1,(length(x)/2),rate=sum((x-mu[i-1])^2)/2)
      sig[i] <- 1/sig[i]
      mu[i] <-rnorm(1,mean=mean(x),sd=sig[i]/(length(x)))
    }
    hist(x,prob=TRUE)
    min <- min(x)
    max <- max(x)
    hx <- rnorm(10000, mean(mu), sqrt(mean(sig)))
    lines(density(hx), col = "blue")
    
    output$meanSEPALLENGTHGIBBS <- renderText({ 
      paste("media gibbs: ", mean(mu))
    })
    
    output$varSEPALLENGTHGIBBS <- renderText({ 
      paste("varianza gibbs: ", mean(sig))
    })
  })
  
  output$NORMPLOT <- renderPlot({
    #Usando GIBBS generamos valores condiciones de sigma^2 y mu usando los datos de iris$Sepal.Length
    x<-rnorm(10000,5,1)
    
    output$meanNORM <- renderText({ 
      paste("media muestral: ", mean(x))
    })
    
    output$varNORM <- renderText({ 
      paste("varianza muestral: ", var(x))
    })
    
    #mu0 = 0, sig0 = 1
    mu <- matrix(0,length(x))
    sig <- matrix(1,length(x))
    for(i in 2:length(x)) {
      sig[i] <- rgamma(1,(length(x)/2),rate=sum((x-mu[i-1])^2)/2)
      sig[i] <- 1/sig[i]
      mu[i] <-rnorm(1,mean=mean(x),sd=sig[i]/(length(x)))
    }
    hist(x,prob=TRUE)
    min <- min(x)
    max <- max(x)
    hx <- rnorm(10000, mean(mu), sqrt(mean(sig)))
    lines(density(hx), col = "blue")
    
    output$meanNORMGIBBS <- renderText({ 
      paste("media gibbs: ", mean(mu))
    })
    
    output$varNORMGIBBS <- renderText({ 
      paste("varianza gibbs: ", mean(sig))
    })
  })
  
})  