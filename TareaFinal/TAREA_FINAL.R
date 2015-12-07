library(foreign)
library(mvtnorm)

table <- read.dta("datos_politicos.dta") # LEEMOS LOS DATOS DE FORMATO STATA
anno <- 1986 # POR EL MOMENTO ESCOJAMOS UN SOLO AñO PUES NO SABEMOS NADA DE DATOS PANEL
data <- table[table$year==anno, ]
labels <- paste(names(data), attributes(data)$var.labels, sep=": ") # NOMBRES DE LAS VARIABLES

head(data)
names(data)
#Se arreglan los datos
Y <- data$reg # INDICADORA DE SI SE ES O NO UNA DEMOCRACIA
list.depend <- c("level", "open", "g", "strikes", "govpwt", "ls", "invpwt",
                 "fertil", "lfagric", "popg", "femsec", "EnergProd") # VARIABLES ECON?MICAS EXPLICATIVAS

X <- subset(data, select = list.depend)
for(j in 1:ncol(X)){
  X[,j]<-as.numeric(X[,j])
}
row.names(X)<-data$name

###El caso de analisis con los datos completos, que no es tan valido
X.comp <- X[complete.cases(X),]
nrow(X.comp)
View(X.comp)
View(round(cor(X.comp),2))
data.full <- data.frame(Y[complete.cases(X)],X.comp)
names(data.full)[1]<-"Y"
res <- glm(Y~ .,data=data.full,family = "binomial")
guess <- round(predict(res,type="response"))

####################
nrows <- nrow(X)
ncols <- ncol(X)
m = 5 #numero de imputaciones bootstrap
tol <- 1e-3
res <- list()
imputed.sets <- list()
pred.success <- numeric(m)

for(rep in 1:m){
  #BOOTSTRAP
  print(paste("bootstrap:", rep))
  samp <- sample(1:nrows, nrows, replace = TRUE)
  Xb <- X[samp, ]
  #Inicializacion
  M <- is.na(Xb) #queremos saber los datos faltantes
  Sigma<- cov(Xb[complete.cases(Xb), ])
  sd.vec <- sqrt(diag(Sigma))
  mu <- apply(Xb[complete.cases(Xb), ], 2, mean)
  for(i in 1:nrows){
    for(j in 1:ncols){
      if(M[i,j]){ #si falta el dato hace algo
        Xb[i,j]<- rnorm(1,mu[j],sd.vec[j])
      }
    }
  }
  logv <- sum(apply(Xb, 1 , function(row) log(dmvnorm(row,mu,Sigma)))) #por cada individuo se calcula la verosimilitud
  #iteracion
  iter <- 1
  repeat{
    #valor actual de la verosimilitud
    #iteraciones por variables 
    for(j in 1:ncol(Xb)){
      ind<- as.matrix(Xb[ ,j],ncol=1)
      dep<-as.matrix(Xb[ , - j]) #todas menos la j
      mod<-lm(ind ~ dep)
      pred <- predict(mod)
      #for(k in 1:nrows){
      #  if(M[k,j]){
      #    Xb[M[k ,j],j]<- pred[k]
      #  }
      #} #es lo mismo que la linea de abajo
      Xb[M[ ,j],j]<- pred[M[ ,j]]
    }
    #nueva matriz de cov 
    Sigma <- cov(Xb)
    mu <- apply(Xb,2,mean)
    logv[(iter + 1)]<- sum(apply(Xb,1,function(row) log(dmvnorm(row,mu,Sigma))))
    if(abs((logv[(iter + 1)]-logv[iter])) < tol) break
    iter <- iter + 1
  }
  print(paste(" - iteraciones totales:", iter))
  imputed.sets[[rep]]<-Xb
  #grafica
  ##curva que va subiedo concava hacia abajo
  plot(logv[-(1:100)], type="l",col="blue", main=paste("Bootstrap",rep))
  #modelo
  data.full<- data.frame(Y[samp],Xb)
  names(data.full)[1]<-"Y"
  res[[rep]]<-glm(Y~ . , data=data.full, family="binomial") #regression logit
  guess <- round(predict(res[[rep]], type = "response")) #probabilidad estimada de que pertenezca a la clase uno
  pred.success[rep] <- sum(guess==data.full$Y )/nrows
}

#pooling
beta.all<- matrix(0, nrow = ncols, ncol = m)
for(rep in 1:m){
  beta.all[ ,rep] <- coef(res[[rep]])[-1]
}
#promedio de las betas    
beta.estims <- apply(beta.all,2,mean)
#estimacion de las varianzas
beta.var.within <- numeric(ncols)
for(rep in 1:m){
  beta.var.within <- beta.var.within + (summary(res[[rep]])$coefficients[ , 2][-1]^2/m)
}
beta.var.between <- apply(beta.all, 1, var)
beta.var <- beta.var.within + (1+1/m)*beta.var.between

#Z-values finales
table<- data.frame(beta = beta.estims, sd=sqrt(beta.var))
round(table,5)

#falta una prueba de formalidad