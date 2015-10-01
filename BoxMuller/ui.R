
# TAREA 2. BOX MULLER
# USER INTERFACE
# KAREN POBLETE 116452
# 18/AGO/2015
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  headerPanel("Generacion de normales"),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(
    sliderInput("bins",
                "Numero de categorias:",
                min = 1,
                max = 50,
                value = 30),
    numericInput("num", 
                 label = "Numero de muestras:", 
                 min = 1,
                 max = 100000,
                 value = 3000)
  ),
  
  mainPanel(
    tabsetPanel(
      
      tabPanel('BoxMullerX', plotOutput("boxmuller"), h2("Ajuste a normal con prueba shapiro-Wilk "), h4("Mientras mas cerca a la linea diagonal mejor"),plotOutput("studentX"), h2("Tabla de datos"),dataTableOutput("tableX")),
      tabPanel('BoxMullerY', plotOutput("boxmullerY"), h2("Ajuste a normal con prueba shapiro-Wilk"), h4("Mientras mas cerca a la linea diagonal mejor"), plotOutput("studentY"),h2("Tabla de datos"), dataTableOutput("tableY"))
    )
  )
))
