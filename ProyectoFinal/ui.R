
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
              value = ""),
    actionButton("activo_uni", "Corregir Unigrama"),
    actionButton("activo_uni_cand", "Muestra candidatos"),
    h5("El proceso puede tardar un rato."),
    h5("El resultado es:"),
    h5(textOutput("unigrama_res")),
    actionButton("activo_bi", "Corregir Bigrama"),
    #actionButton("activo_bi_cand", "Muestra candidatos"),
    h5("El proceso puede tardar un rato."),
    h5("El resultado es:"),
    h5(textOutput("bigrama_res"))
  ),
  mainPanel(
    h3("Corrector Ortográfico") ,
    tabsetPanel(
      tabPanel('Corrector con unigramas', h5("Los cinco mejores andidatos por palabra x. La ultima columna (x) es la palabra con error y w1 los candidatos"),dataTableOutput("unigrama_cand")),
      tabPanel('Set de Unigramas', dataTableOutput("unigramas_tab")),
      tabPanel('Set de Bigramas', dataTableOutput("bigramas_tab")),
      tabPanel('Set de Trigramas', dataTableOutput("trigramas_tab")),
      tabPanel('Generar texto con modelo de lenguaje', h4('Con unigramas:'), textOutput("unigram_text"),
               h4('Con bigramas:'), textOutput("bigram_text"), h4('Con trigramas:'), textOutput("trigram_text"))
    )
    
  )
))
