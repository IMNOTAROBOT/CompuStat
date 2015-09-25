
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
                "Número de categorías:",
                min = 1,
                max = 50,
                value = 30),
    sliderInput("rep",
                "Número de repeticiones:",
                min = 1,
                max = 5000,
                value = 1000),
    numericInput("num", 
                 label = "Número de muestras:", 
                 min = 1,
                 max = 100000,
                 value = 3000)
  ),
  
  mainPanel(
    textOutput("valReal"),
    plotOutput("monteCarloCrudo"),
    plotOutput("monteCarlo")
    
  )
))
