# PROYECTO FINAL
# SERVER CODE
# KAREN POBLETE 116452
# 10/DIC/2015
#

library(shiny)
library(RWeka)
library(parallel)
library(dplyr)
library(tidyr)
library(stringr)

shinyServer(function(input, output) {
  #Modelo del lenguaje 
  #Cargando el modelo de lenguaje
  load('Rdata/unigramas.cuenta_corto2.Rdata')
  load('Rdata/bigramas.cuenta_corto2.Rdata')
  load('Rdata/trigramas.cuenta_corto.Rdata')
  
  #Letras y simbolos que se sustituyen cuando se crean candidatos
  letras <- c(letters,'á', 'é','í','ó','ú','ñ')
  
  output$unigrama_res <- renderText({ 
    #Producimos todos los candidatos 
    candidatos <- function(s){
      s.vec <- strsplit(s, split='')[[1]]
      substitute_1 <- Vectorize(function(x,y){
        z <- s.vec
        z[x] <- y
        paste(z, collapse='')
      })
      insert_1 <- Vectorize(function(x,y){
        if(x==0) prev <- '' else prev <-  s.vec[1:x] 
        if(x==length(s.vec)) post <- '' else 
          post <- s.vec[(x+1):length(s.vec)]
        paste(c(prev, y, post), collapse='')
      })
      remove_1 <- function(x){
        paste(s.vec[-x], collapse='')
      }
      transpose_1 <- function(x){
        z <- s.vec
        z[x+1] <- s.vec[x]
        z[x] <- s.vec[x+1]
        paste(z, collapse='')
      }
      lista_1 <- as.character(outer(1:length(s.vec), letras, FUN='substitute_1'))
      lista_2 <- as.character(outer(0:length(s.vec), letras, FUN='insert_1') )
      lista_3 <- sapply(1:length(s.vec), remove_1)
      lista_4 <- sapply(1:(length(s.vec)-1), transpose_1)
      lista.candidatos <- unique(c(s, lista_1, lista_2, lista_3, lista_4))
      lista.candidatos
    }
    
    #Filtrar candidatos por palabras
    vocabulario <- unigramas.cuenta$w1
    
    filtrar.cand <- function(cand){
      cand[cand %in% vocabulario]
    }
    
    #calculando las frecuencias
    frecs.cand <- function(cand){
      cand.df.1 <- data.frame(w1=cand, stringsAsFactors=FALSE)
      res.1 <- inner_join(unigramas.cuenta, cand.df.1) %>% arrange(desc(frec))
      res.1
    }
    
    #Ahora hacemos una función para corregir una oración con unigramas:
    corregirUni <- function(oracion){
      pals <- strsplit(oracion, " ")[[1]]
      corregir.ind <- !(pals %in% vocabulario)
      pals.corregir <- pals[corregir.ind]
      candidatos <- sapply(pals.corregir, function(pal){
        cand.1 <- candidatos(pal)
        cand.2 <- unique(unlist(lapply(cand.1, candidatos)))
        d.1 <- frecs.cand(filtrar.cand(cand.1))
        d.2 <- frecs.cand(filtrar.cand(cand.2))
        d.1$log.canal <- log(999/1000)
        d.2$log.canal <- log(1/1000)
        d <- rbind(d.1, d.2)
        d$log.post <- d$log.p + d$log.canal
        arrange(d, desc(log.post))[1,1]
      })
      candidatos
      pals.1 <- pals
      pals.1[corregir.ind] <- candidatos
      paste(pals.1, collapse = ' ')
    }
    
    corregirUni(input$texto_mal)
  })
  
  output$bigrama_res <- renderText({ 
    #Producimos todos los candidatos 
    candidatos <- function(s){
      s.vec <- strsplit(s, split='')[[1]]
      substitute_1 <- Vectorize(function(x,y){
        z <- s.vec
        z[x] <- y
        paste(z, collapse='')
      })
      insert_1 <- Vectorize(function(x,y){
        if(x==0) prev <- '' else prev <-  s.vec[1:x] 
        if(x==length(s.vec)) post <- '' else 
          post <- s.vec[(x+1):length(s.vec)]
        paste(c(prev, y, post), collapse='')
      })
      remove_1 <- function(x){
        paste(s.vec[-x], collapse='')
      }
      transpose_1 <- function(x){
        z <- s.vec
        z[x+1] <- s.vec[x]
        z[x] <- s.vec[x+1]
        paste(z, collapse='')
      }
      lista_1 <- as.character(outer(1:length(s.vec), letras, FUN='substitute_1'))
      lista_2 <- as.character(outer(0:length(s.vec), letras, FUN='insert_1') )
      lista_3 <- sapply(1:length(s.vec), remove_1)
      lista_4 <- sapply(1:(length(s.vec)-1), transpose_1)
      lista.candidatos <- unique(c(s, lista_1, lista_2, lista_3, lista_4))
      lista.candidatos
    }
    
    #Filtrar candidatos por palabras
    vocabulario <- unigramas.cuenta$w1
    
    filtrar.cand <- function(cand){
      cand[cand %in% vocabulario]
    }
    
    frecs.cand <- function( ant, cand, post){
      posibles1.1 <- subset(bigramas.cuenta, bigramas.cuenta$w1 == ant)
      posibles1.2 <- posibles1.1[posibles1.1$w2 %in% cand,]
      
      posibles2.1 <- bigramas.cuenta[bigramas.cuenta$w1 %in% cand,]
      posibles2.2 <- subset(posibles2.1, posibles2.1$w2 == post)
      posibles2.2$w3 <- posibles2.2$w2
      posibles2.2$w2 <- posibles2.2$w1
    
      posibles <- merge(posibles1.2, posibles2.2, by.x="w2", by.y = "w2")
      posibles.res <- data.frame(w1 = posibles$w1.x, w2 = posibles$w2, w3 = posibles$w3, frec = (posibles$frec.x + posibles$frec.y)/2, 
                                 p = posibles$p.x * posibles$p.x, log.p = posibles$log.p.y + posibles$log.p.x)
      if(dim(posibles.res)[1] == 0){
        posibles.res <- data.frame(w1 = as.character(ant), w2 = as.character(cand[1]), w3 = as.character(post), frec = 1, p = 8e-14, log.p = -100)
      }
      posibles.res
    }
    
    #Ahora hacemos una función para corregir una oración con unigramas:
    corregirBi <- function(oracion){
      pals <- strsplit(oracion, " ")[[1]]
      candidats <- rep("", length(pals))
      for(i in 2:length(pals)-1){
        cand.1 <- candidatos(pals[i])
        cand.2 <- unique(unlist(lapply(cand.1, candidatos)))
        cand1.1 <- filtrar.cand(cand.1)
        cand2.1 <- filtrar.cand(cand.2)
        d.1 <- frecs.cand(pals[i-1], cand1.1, pals[i+1])
        d.1$log.canal <- log(999/1000)
        d.2 <- frecs.cand(pals[i-1], cand2.1, pals[i+1])
        d.2$log.canal <- log(1/1000)
        d <- rbind(d.1, d.2)
        d$log.post <- d$log.p + d$log.canal
        candidos <- arrange(d, desc(log.post))
        candidats[i] <- as.character(candidos$w2[1])
      }
      candidats
      pals.1 <- pals
      pals.1[2:length(pals.1)-1] <- candidats
      paste(pals.1, collapse = ' ')
    }
    corregirBi(input$texto_mal)
  })
  
  
  
})  