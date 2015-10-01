
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
    sliderInput("bins",
                "Numero de categorías:",
                min = 1,
                max = 100,
                value = 50),
    sliderInput("rep",
                "Numero de repeticiones:",
                min = 1,
                max = 5000,
                value = 500),
    numericInput("num", 
                 label = "Numero de muestras:", 
                 min = 1,
                 max = 10000,
                 value = 500)
  ),
  
  mainPanel(
    h3(textOutput("valReal")) ,
    tabsetPanel(
      tabPanel('MonteCarlo crudo', plotOutput("monteCarloCrudo"),h3("Error"), plotOutput("monteCarloCrudoError")),
      tabPanel('MonteCarlo',plotOutput("monteCarlo"), h3("Error"), plotOutput("monteCarloError"))
    )
    
  )
))
