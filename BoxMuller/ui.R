
# TAREA 2. BOX MULLER
# USER INTERFACE
# KAREN POBLETE 116452
# 18/AGO/2015
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  headerPanel("Generación de exponenciales"),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(
    sliderInput("bins",
                "Número de categorías:",
                min = 1,
                max = 50,
                value = 30),
    numericInput("num", 
                 label = "Número de muestras:", 
                 min = 1,
                 max = 100000,
                 value = 3000)
  ),
  
  mainPanel(
    plotOutput("boxmuller"),
    plotOutput("boxmullerY"),
    dataTableOutput("tableX"),
    dataTableOutput("tableY")
  )
))
