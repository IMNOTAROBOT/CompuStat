f <- function(x){
  (2/sqrt(2*pi))*exp(-x^2/2)
}

#Quiero usar la g para generar la f
g <- function(x){
  exp(-x)
}

plot(f,ylim=c(0,2), xlim=c(0,5))
plot(g,add=TRUE, col= "red", ylim = c(0,2), xlim=c(0.5))

h <- function(x){
f(x)/g(x)
}

plot(h, xlim=c(0.1,4), ylim=c(0,2))
