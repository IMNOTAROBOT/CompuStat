
# TAREA 6. GIBBS SAMPLING
# USER INTERFACE
# KAREN POBLETE 116452
# 18/NOV/2015
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  headerPanel("Tarea 6: GIBBS SAMPLING"),
  
  h5("Esta aplicacion genera las marginales de mu y sigma^2 por medio del metodo de muestreo de GIBBS para dos ejemplos."),
  
  mainPanel(
    h3("GIBBS SAMPLING") ,
    tabsetPanel(
      tabPanel('IRIS',h5(textOutput("meanSEPALLENGTH")),h5(textOutput("varSEPALLENGTH")),
               h5(textOutput("meanSEPALLENGTHGIBBS")),h5(textOutput("varSEPALLENGTHGIBBS")),plotOutput("IRISPLOT")),
      tabPanel('Datos IRIS', dataTableOutput("DatosIRIS")),
      tabPanel('X NORMAL',h5(textOutput("meanNORM")),h5(textOutput("varNORM")),
               h5(textOutput("meanNORMGIBBS")),h5(textOutput("varNORMGIBBS")),plotOutput("NORMPLOT"))
    )
    
  )
))
