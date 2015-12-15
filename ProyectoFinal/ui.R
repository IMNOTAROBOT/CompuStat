
# PROJECTO FINAL
# USER INTERFACE
# KAREN POBLETE 116452
# 10/DIC/2015
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  headerPanel("Proyecto final Estadistica Computacional: Corrector ortografico"),
  
  h5("Se utiliza el metodo de Bayes con modelo de canal ruidoso y modelo de lenguaje con unigramas y bigramas."),
  sidebarPanel(
    textInput("texto_mal", label = h3("Texto a corregir"), 
              value = "<s> <coma> ademas de </s>"),
    submitButton("Update View")
  ),
  mainPanel(
    h3("Corrector Ortográfico") ,
    tabsetPanel(
      tabPanel('Unigramas',h5(textOutput("unigrama_res")), h5("Con bigramas"), h5(textOutput("bigrama_res")))
    )
    
  )
))
