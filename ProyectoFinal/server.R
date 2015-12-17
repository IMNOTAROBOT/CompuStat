# PROYECTO FINAL
# SERVER CODE
# KAREN POBLETE 116452
# 10/DIC/2015
#

library(shiny)
library(parallel)
library(dplyr)
library(tidyr)
library(stringr)
library(RWeka)

shinyServer(function(input, output) {
  #Modelo del lenguaje 
  #Cargando el modelo de lenguaje
  load('Rdata/unigramas.cuenta_corto2.Rdata')
  load('Rdata/bigramas.cuenta_corto2.Rdata')
  load('Rdata/trigramas.cuenta_corto.Rdata')
  
  #Letras y simbolos que se sustituyen cuando se crean candidatos
  letras <- c(letters,'á', 'é','í','ó','ú','ñ')
  set.seed(142857)
  
  output$unigrama_res <- renderText({
    corrector_unigram()
  })
  
  corrector_unigram <- eventReactive(input$activo_uni,{ 
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
    aux <- input$texto_mal
    entrada <- paste("<s>", aux , "</s>", collapse = ' ')
    res <- corregirUni(entrada)
    result <- strsplit(res, " ")[[1]]
    res.1 <- result[2:(length(result)-1)]
    res.1
  })
  
  output$bigrama_res <- renderText({
    corrector_bigram()
  })
  
  corrector_bigram <- eventReactive(input$activo_bi,{ 
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
        z <- s.vec
        if(x >0 && x < length(s.vec) && length(s.vec) != 0){
          z <- paste(s.vec[-x], collapse='')
        }
        z
      }
      transpose_1 <- function(x){
        z <- s.vec
        if(x > 0 && x < length(s.vec) && length(s.vec) != 0){
          z[x+1] <- s.vec[x]
          z[x] <- s.vec[x+1]
          z <- paste(z, collapse='')
        }
        z
      }
      lista_1 <- as.character(outer(1:length(s.vec), letras, FUN='substitute_1'))
      lista_2 <- as.character(outer(0:length(s.vec), letras, FUN='insert_1') )
      lista_3 <- sapply(1:length(s.vec), remove_1)
      lista_4 <- sapply(1:(length(s.vec)-1), transpose_1)
      lista.candidatos <- unique(c(s, lista_1, lista_2, lista_3, lista_4))
      lista.candidatos
    }
    
    #Filtrar candidatos por palabras
    vocabulariow1 <- bigramas.cuenta$w1
    
    filtrar.cand <- function(cand){
      cand[cand %in% vocabulariow1]
    }
    
    frecs.cand <- function( ant, cand, post){
      posibles.result <- data.frame(w1 = as.character(), w2 = as.character(), 
                                    w3 = as.character(), frec = numeric(0), 
                                    p = numeric(0), log.p = numeric(0))
      posibles1.1 <- subset(bigramas.cuenta, bigramas.cuenta$w1 == ant)
      posibles1.2 <- posibles1.1[posibles1.1$w2 %in% cand,]
      
      posibles2.1 <- bigramas.cuenta[bigramas.cuenta$w1 %in% cand,]
      posibles2.2 <- subset(posibles2.1, posibles2.1$w2 == post)
      posibles2.2$w3 <- posibles2.2$w2
      posibles2.2$w2 <- posibles2.2$w1
    
      posibles <- merge(posibles1.2, posibles2.2, by.x="w2", by.y = "w2")
      posibles.res <- data.frame(w1 = posibles$w1.x, w2 = posibles$w2, w3 = posibles$w3, frec = (posibles$frec.x + posibles$frec.y)/2, 
                                 p = posibles$p.x * posibles$p.x, log.p = posibles$log.p.y + posibles$log.p.x)
      if(dim(posibles.res)[1] != 0){
        names(posibles.res) <- names(posibles.result) 
        posibles.result <- rbind(posibles.result, posibles.res)
      }else{
        res_aux <- data.frame(w1 = ant, w2 = cand[1], w3 = post, frec = 1, 
                              p = 0.0004, log.p = -10)
        names(res_aux) <- names(posibles.result) 
        posibles.result <- rbind(posibles.result, res_aux)
      }
      posibles.result
    }
    
    #Ahora hacemos una función para corregir una oración con unigramas:
    corregirBi <- function(oracion){
      pals <- strsplit(oracion, " ")[[1]]
      candidats <- rep("", (length(pals)-2))
      
      for(k in 2:(length(pals)-1)){
        cand.1 <- candidatos(pals[k])
        cand.2 <- unique(unlist(lapply(cand.1, candidatos)))
        cand1.1 <- filtrar.cand(cand.1)
        cand2.1 <- filtrar.cand(cand.2)
        d.1<- frecs.cand(pals[(k-1)], cand1.1, pals[(k+1)])
        d.1$log.canal <- log(999/1000)
        d.2 <- frecs.cand(pals[(k-1)], cand2.1, pals[(k+1)])
        d.2$log.canal <- log(1/1000)
        d <- rbind(d.1, d.2)
        d$log.post <- d$log.p + d$log.canal
        candidos <- arrange(d, desc(log.post))
        candidats[k-1] <- as.character(candidos$w2[1])
      }
      candidats
      pals.1 <- pals
      pals.1[2:(length(pals.1)-1)] <- candidats
      regresar <- paste(pals.1, collapse = ' ')
      regresar
    }
    aux <- input$texto_mal
    entrada <- paste("<s>", aux , "</s>", collapse = ' ')
    res <- corregirBi(entrada)
    result <- strsplit(res, " ")[[1]]
    res.1 <- result[2:(length(result)-1)]
    res.1
  })
  
  output$unigramas_tab <- renderDataTable({data.frame(unigramas.cuenta)})
  output$bigramas_tab <- renderDataTable({data.frame(bigramas.cuenta)})
  output$trigramas_tab <- renderDataTable({data.frame(trigramas.cuenta)})
  
  output$unigrama_cand <- renderDataTable({
    candidatos_unigram_reac()
  })
  
  candidatos_unigram_reac <- eventReactive(input$activo_uni_cand,{ 
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
      res_tot <- data.frame(x = character() , w1 = character(), frec = numeric(0), p = numeric(0), log.p = numeric(0) , log.canal = numeric(0), log.post =numeric(0))
      if(length(pals.corregir)>=1){
        for(i in 1:length(pals.corregir)){
          cand.1 <- candidatos(pals.corregir[i])
          cand.2 <- unique(unlist(lapply(cand.1, candidatos)))
          d.1 <- frecs.cand(filtrar.cand(cand.1))
          d.2 <- frecs.cand(filtrar.cand(cand.2))
          d.1$log.canal <- log(999/1000)
          d.2$log.canal <- log(1/1000)
          d <- rbind(d.1, d.2)
          d$log.post <- d$log.p + d$log.canal
          d$x <- pals.corregir[i]
          res <- arrange(d, desc(log.post))[1:5, ]
          res_tot <- rbind(res_tot, res)
        }
      }
      res_tot
    }
    aux <- input$texto_mal
    entrada <- paste("<s>", aux , "</s>", collapse = ' ')
    resi <- corregirUni(entrada)
    data.frame(resi)
  })
  
  output$unigram_text <- renderText({
    muestra <- sample_n(unigramas.cuenta, 50, weight = frec)$w1
    paste(muestra, collapse = " ")
  })
  
  output$bigram_text <- renderText({
    k=50
    comienzo='<s>'
    actual <- comienzo
    frase <- ''
    for(j in 1:k){
      pal <- bigramas.cuenta %>% filter(w1==actual) %>% sample_n(size =1, weight=frec)
      actual <- pal$w2
      frase <- paste(frase, actual, sep=' ')
    }
    frase.1 <- gsub("<punto_coma>", ";", frase)
    frase.2 <- gsub("<coma>", ",", frase.1)
    frase.3 <- gsub("</s> <s> <s>", ".\n", frase.2)
    frase.3.1 <- gsub("</s> <s>", ".", frase.3)
    frase.4 <- gsub("<dos_puntos>", ":", frase.3.1)
    frase.4
  })
  
  output$trigram_text <- renderText({
    k=50
    comienzo1='<s>'
    comienzo2='<s>'
    actual <- comienzo1
    siguiente <- comienzo2
    frase <- ''
    for(j in 1:k){
      pal <- trigramas.cuenta %>% filter(w1==actual & w2==siguiente) %>% sample_n(size =1, weight=frec)
      actual <- pal$w2
      siguiente <- pal$w3
      frase <- paste(frase, actual, sep=' ')
    }
    frase.1 <- gsub("<punto_coma>", ";", frase)
    frase.2 <- gsub("<coma>", ",", frase.1)
    frase.3 <- gsub("</s> <s> <s>", ".\n", frase.2)
    frase.3.1 <- gsub("</s> <s>", ".", frase.3)
    frase.4 <- gsub("<dos_puntos>", ":", frase.3.1)
    frase.4
  })
  
})  