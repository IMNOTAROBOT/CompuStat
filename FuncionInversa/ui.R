
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  
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
                 value = 200),
    numericInput("lamda", 
                 label = "Lamda:", 
                 min = 1,
                 max = 10,
                 value = 1),
    textInput("file", label = "Documento con los valores de pesos y lamda para mezcla de exponenciales", value = "C:/Users/karen/Documents/ITAM14-MCC3/Tarea1/exponenciales.txt")
  ),
  
  mainPanel(
    plotOutput("distPlot"),
    dataTableOutput("table"),
    plotOutput("mezcla"),
    dataTableOutput("tableMix")
  )
))
