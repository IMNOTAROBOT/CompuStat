
# TAREA 3. MONTECARLO
# USER INTERFACE
# KAREN POBLETE 116452
# 31/AGO/2015
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  headerPanel("Montecarlo"),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(
    sliderInput("alpha",
                "Grado de Confianza",
                min = 0,
                max = 1,
                value = 0.05),
    sliderInput("rep",
                "Numero de repeticiones del experimento:",
                min = 1,
                max = 5000,
                value = 500),
    sliderInput("N", 
                 label = "Numero de muestras por exp (N):", 
                 min = 1,
                 max = 10000,
                 value = 500),
    sliderInput("m",
                "Valor de m:",
                min = 1,
                max = 100,
                value = 1),
    sliderInput("lambda",
                "Valor de lambda:",
                min = 1,
                max = 100,
                value = 1)
  ),
  
  mainPanel(
    h4(textOutput("valReal")) ,
    tabsetPanel(
      tabPanel('MonteCarlo crudo', plotOutput("monteCarloCrudo"),h5(textOutput("alphaCrudo")),h4("Error"), plotOutput("monteCarloCrudoError")),
      tabPanel('MonteCarlo',plotOutput("monteCarlo"), h5(textOutput("alphaMonte")),h4("Error"), plotOutput("monteCarloError"))
    )
    
  )
))
