# TAREA 4. BOOTSTRAP (Metodo de percentiles)
# SERVER CODE
# KAREN POBLETE 116452
# 10/OCT/2015
#

library(shiny)
library(bootstrap)
library(boot)

shinyServer(function(input, output) {
  #Leer la tabla de los datos utilizados
  crime <- read.table("crime.txt", header=T)
  
  #Mostrar los datos utilizados para el ejemplo
  output$tableData <- renderDataTable({data.frame(crime)})
  
  #Numero de tuplas
  N_crime<-dim(crime)[1]
  
  #Graficamos correlacion entre el numero de crimenes de 1960 y el gasto policiaco
  output$corr60 <- renderPlot({
    plot(crime$police60, crime$crimerat, xlim=c(0,200), ylim=c(0,200),
         xlab="Gasto policia", ylab="Tasa criminal",
         main="Datos crimenes")
  })
  
  #Calculamos los valores reales de correlacion entre tasa de crimen y gasto policiaco
  corr.pob_crime<-cor(crime$crimerat,crime$police60)
  #Calculamos el error estandar teorico original
  error.pob_crime<-sqrt((1-(corr.pob_crime^2))/(N_crime-2))
  
  output$corrReal <- renderText({ 
    paste("La correlacion entre tasa de crimen y gasto policiaco teorica: ", corr.pob_crime)
  })
  
  output$errReal <- renderText({ 
    paste("El error estandar teorico de la poblacion total: ",error.pob_crime) 
  })
  
  output$corrBootHist <- renderPlot({
    
    #Generamos una muestra
    n_muestra_crime<-input$tam_muestra #Tamaño de la muestra
    muestra_crime<-sample(1:N_crime,n_muestra_crime, replace = FALSE) #Eleccion de las tuplas que estaran en la muestra
    datos<-crime[muestra_crime, c(1,5)]  # muestra de las variables de interes
    
    #Calculando la correlacion muestral y el error estandar muestral
    corr.muestra_crime<-cor(crime$crimerat[muestra_crime], crime$police60[muestra_crime])  # Coeficiente de correlacion muestral
    error.muestra_crime<-sqrt((1-(corr.muestra_crime^2))/(n_muestra_crime-2))  # error estandar muestra
    
    output$corrMues <- renderText({ 
      paste("La correlacion entre tasa de crimen y gasto policiaco muestral: ", corr.muestra_crime)
    })
    
    output$errMues <- renderText({ 
      paste("El error estandar muestral: ",error.muestra_crime) 
    })
    
    #Hacemos bootstrap
    B<-boot_b<-input$B          # numero de replicas bootstrap
    R<-numeric(B)     # Guardar las replicas
    
    for(b in 1:B){
      i<-sample(1:n_muestra_crime,n_muestra_crime,replace=TRUE)  # con reemplazo
      crimen<-datos$crimerat[i]
      fondo<-datos$police60[i]
      R[b]<-cor(crimen,fondo)  
    }
    
    se_crime.b<-sqrt(var(R))
    corr_crime.b<-mean(R)
    
    output$corrBoot <- renderText({ 
      paste("La correlacion entre tasa de crimen y gasto policiaco Bootstrap: ", corr_crime.b)
    })
    
    output$errBoot <- renderText({ 
      paste("El error estandar Bootstrap: ", se_crime.b) 
    })
    
    #Estimando la distribucion muestral del estimador de la correlacion porque son pocos datos
    b_int<- 5000
    r <- rep(0, b_int)
    for (i_int in 1:b_int) {
      i_mues <- sample(1:47, n_muestra_crime, replace=F)
      datos_muestra<- crime[i_mues, c(1,5)]
      r[i_int] <- cor(datos_muestra[, 1], datos_muestra[, 2])
    }
    
    par(mfrow=c(1,2))
    hist(R, xlab="correlacion", main="Replicas bootstrap",breaks=15)
    hist(r, xlab="correlacion", main="Distribucion muestral", breaks=15)
    
    #Para calcular los percentiles
    theta <- function(xdata, x){
      cor(xdata[x,1], xdata[x,2])
    }
    
    boot_data <- cbind(crimerat = crime$crimerat, police60 = crime$police60)
    boot.out<-boot(boot_data,statistic=theta ,R=B)
    output$boot.ci.print <- renderPrint({ 
      boot.ci(boot.out,type=c("basic","norm","perc"))
    })
    
  })

  
})  