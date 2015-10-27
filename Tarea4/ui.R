
# TAREA 4. BOOTSTRAP (Metodo de percentiles)
# USER INTERFACE
# KAREN POBLETE 116452
# 10/OCT/2015
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  headerPanel("Tarea 4: Bootstrap (Metodo de percentiles)"),
  
  h5("Esta aplicacion ejemplifica la utilizacion del metodo de remuestreo bootstrap con MonteCarlo 
     y el rechazo de estimaciones fuera del intervalo de confianza por medio del metodo de percentiles."),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(
    sliderInput("tam_muestra",
                "Tamano de la muestra usada:",
                min = 1,
                max = 45,
                value = 15),
    sliderInput("B",
                "Numero de muestras usadas en remuestreo:",
                min = 1,
                max = 5000,
                value = 500),
    h3("Caracteristicas de la poblacion"),
    h5(textOutput("corrReal")), 
    h5(textOutput("errReal")),
    h3("Caracteristicas de la muestra"),
    h5(textOutput("corrMues")), 
    h5(textOutput("errMues"))
  ),
  
  mainPanel(
    h3("Resultados:") ,
    tabsetPanel(
      tabPanel('Correlacion Teorica',h4('Relacion entre el numero de crimenes y el gasto policiaco de la poblacion total'),
               plotOutput("corr60")),
      tabPanel('Bootstrap',h5(textOutput("corrBoot")),h5(textOutput("errBoot")),
               plotOutput("corrBootHist"), h4('Datos de intervalos de confianza: (basic, norm, perc)'),
               h5(textOutput("boot.ci.print"))),
      tabPanel('Los datos usados', h4('Estadisticas de crimenes (poblacion total)'),
               dataTableOutput("tableData"))
    )
    
  )
))
