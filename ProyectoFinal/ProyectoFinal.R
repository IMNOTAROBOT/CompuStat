#Codigo para generar el corrector ortográfico

#Corrector ortografico

options(digits=2)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(utils)

### Modelo de lenguaje

texto <- scan(file='datos/Es_Newspapers.txt',  sep="\n", what = character(),encoding="UTF-8")
length(texto)
texto[1]

library(RWeka)
unigramas <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bigramas <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigramas <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3)) 

limpiar <- function(texto){
  texto.1.1 <- tolower(texto)
  texto.1.1 <- gsub("\\.", " </s> <s>", texto.1.1)
  texto.1.2 <- gsub(" <s>$", "", texto.1.1)
  texto.2 <- gsub("[«»]", "", texto.1.2) 
  texto.2.1 <- gsub(";", " <punto_coma> ", texto.2) 
  texto.2.2 <- gsub("\\:", " <dos_puntos> ", texto.2.1) 
  texto.2.2 <- gsub("\\,", " <coma> ", texto.2.2) 
  texto.3 <- gsub("^\\s+|\\s+$", "",texto.2.2) # espacios antes y después
  texto.4 <- gsub("\\\"", "", texto.3)
  paste0('<s> <s> ', texto.4)
}

texto.limpio <- lapply(texto[1:10000], limpiar)
texto.limpio[[2]]

#contar frecuencia de ngramas
contar_ngramas <- function(ngramas, n=2){
  #contar ngramas
  ngrama.2 <- lapply(ngramas, function(x){data.frame(table(x))}) 
  noms <- paste('w',1:n, sep="")
  ngrama.3 <- rbind_all(ngrama.2)
  ngrama.4 <- group_by(as.data.frame(ngrama.3), x) %>% summarise(frec = sum(Freq))
  ngrama.5 <- ngrama.4 %>% arrange(desc(frec))
  ngrama.6 <- ngrama.5 %>% separate(x, noms, ' ')
  ngrama.6
}

textos.unigramas <- lapply(texto.limpio, unigramas)
head(textos.unigramas)
unigramas.cuenta <-contar_ngramas(textos.unigramas, 1)

textos.bigramas <- lapply(texto.limpio, bigramas)
bigramas.cuenta <- contar_ngramas(textos.bigramas, 2)

textos.trigramas <- lapply(texto.limpio, trigramas)
trigramas.cuenta <- contar_ngramas(textos.trigramas, 3)

#Agregando probabilidad y probabilidad log
unigramas.cuenta$p <- unigramas.cuenta$frec/sum(unigramas.cuenta$frec)
unigramas.cuenta$log.p <- log(unigramas.cuenta$p)
head(unigramas.cuenta,10)
#save(unigramas.cuenta, file='Rdata/unigramas.cuenta_corto2.Rdata')
#load('Rdata/unigramas.cuenta_corto2.Rdata')

bigramas.cuenta$p <- bigramas.cuenta$frec/sum(bigramas.cuenta$frec)
bigramas.cuenta$log.p <- log(bigramas.cuenta$p)
head(bigramas.cuenta,10)
#save(bigramas.cuenta, file='Rdata/bigramas.cuenta_corto2.Rdata')
#load('Rdata/bigramas.cuenta_corto2.Rdata')

trigramas.cuenta$p <- trigramas.cuenta$frec/sum(trigramas.cuenta$frec)
trigramas.cuenta$log.p <- log(trigramas.cuenta$p)
head(trigramas.cuenta,10)
#save(trigramas.cuenta, file='Rdata/trigramas.cuenta_corto.Rdata')
#load('Rdata/trigramas.cuenta_corto.Rdata')

#####################################################################
### Producción de candidatos

letras <- c(letters,'-', ' ')

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
head(vocabulario)

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

#ejemplo de uso
corregirUni('<s> eatin kat')

#Con bigramas
vocabularioBi <- data.frame(w1 = bigramas.cuenta$w1, w2 = bigramas.cuenta$w2)
head(vocabularioBi)

filtrar.candBiW1 <- function(cand, pal){
  can_w1 <- cand[cand %in% vocabularioBi$w1]
  can_w2 <- can_w1[cand_w1 %in% vocabularioBi$w2]
  
}

#calculando las frecuencias
frecs.candBi <- function(cand){
  cand.df.1 <- data.frame(w1=cand, stringsAsFactors=FALSE)
  res.1 <- inner_join(unigramas.cuenta, cand.df.1) %>% arrange(desc(frec))
  res.1
}

#Ahora hacemos una función para corregir una oración con bigramas:
corregirBi <- function(oracion){
  pals <- strsplit(oracion, " ")[[1]]
  corregir.ind <- !(pals %in% vocabulario)
  pals.corregir <- c(pals[corregir.ind - 1], pals[corregir.ind], pals[corregir.ind +1])
  candidatos <- sapply(pals.corregir, function(pal){
    cand.1 <- candidatos(pal)
    cand.2 <- unique(unlist(lapply(cand.1, candidatos)))
    d.1 <- frecs.candBi(filtrar.candBi(cand.1, pal))
    d.2 <- frecs.candBi(filtrar.candBi(cand.2, pal))
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

##Evaluación del modelo de lenguaje

crear_frase_uni <- function(k=10){
  muestra <- sample_n(uni.6, k, weight = frec)$w1
  paste(muestra, collapse = " ")
}
crear_frase_bi <- function(k=10, comienzo='<s>'){
  actual <- comienzo
  frase <- ''
  for(j in 1:k){
    pal <- big.6 %>% filter(w1==actual) %>% sample_n(size =1, weight=frec)
    actual <- pal$w2
    frase <- paste(frase, actual, sep=' ')
  }
  frase.1 <- gsub("<punto_coma>", ";", frase)
  frase.2 <- gsub("<coma>", ",", frase.1)
  frase.3 <- gsub("</s> <s> <s>", ".\n", frase.2)
  frase.3.1 <- gsub("</s> <s>", ".", frase.3)
  frase.4 <- gsub("<dos_puntos>", ":", frase.3.1)
  frase.4
}

# Crear texto con trigramas
crear_frase_tri <- function(k=10, comienzo1='<s>', comienzo2='<s>'){
  actual <- comienzo1
  siguiente <- comienzo2
  frase <- ''
  for(j in 1:k){
    pal <- trig.6 %>% filter(w1==actual & w2==siguiente) %>% sample_n(size =1, weight=frec)
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
}


###validar modelos
errores <- read.table("datos/errores_wikipedia.txt", sep="|", header=FALSE, quote="", 
                      stringsAsFactors=FALSE,encoding="UTF-8") 
names(errores) <- c("Bien", "Error")
head(errores)

spelltest <- function(errores,){
  bien <- 0
  mal <- 0
  resp <- rep("", dim(errores)[1])
  for(i in 1:dim(errores)[1]){
    aux <- corregirUni(errores$Error[i])
    distd <- adist(errores$Bien[i], aux)
    if(distd == 0){
      bien <- bien + 1
    }else{
      mal <- mal + 1
    }
  }
  
}

distancias.1 <- apply(errores, 1, function(x) {adist(x[1], x[2])[1,1]})
error.d1 <- errores[distancias.1==1, ]
mat.transf <- apply(error.d1,1, function(x){attr(adist(x[1], x[2], counts=T), 'counts')})
sustituciones <- mat.transf[3,]==1
error.sus <- error.d1[sustituciones,]
