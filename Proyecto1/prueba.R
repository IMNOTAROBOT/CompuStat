set.seed(142857)
x <- seq(1, 5, by=0.1)
f <- function(x){
  (2/sqrt(2*pi))*exp(-x^2/2)
}

#Quiero usar la g para generar la f
g <- function(x){
  exp(-x)
}

plot(f(x),ylim=c(0,2), xlim=c(0,5))
plot(g(x),add=TRUE, col= "red", ylim = c(0,2), xlim=c(0.5))

h <- function(x){
f(x)/g(x)
}

plot(h, xlim=c(0.1,4), ylim=c(0,2))

M<-2

genera.una <- function(...){
  MAXITER <- 10000
  iter <- 1
  continuar = TRUE
  while(continuar | iter <= MAXITER){
    #Aceptacion rechazo
    #1)
    Y<-log(1 / ( 1 - runif(1)))
    U<-runif(1)
    #2)
    if( U <= f(Y) / (M * g(Y))){
      X <- Y
      continuar = FALSE
    }
    iter <- iter + 1
  }
  X
}

genera.muchas <- function(n){
  sapply(1:n, FUN=genera.una)
}

vec <- genera.muchas(5)
hist(vec,breaks = 10)

genera.normales <- function(n){
  sample(size = n, c(-1,1), replace = TRUE)*genera.muchas(n)
}

vec1 <- genera.normales(500)
hist(vec1, breaks = 20)