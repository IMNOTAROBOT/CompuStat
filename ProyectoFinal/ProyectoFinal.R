library(Rcpp)

# Estimación de media
data(iris)
head(iris)

#Para calcular la media de Sepal.length: 5.843333
#La varianza: 0.6856935
mean(iris$Sepal.Length)
var(iris$Sepal.Length)
#Usando GIBBS generamos valores condiciones de sigma^2 y mu usando los datos de iris$Sepal.Length
x<-iris$Sepal.Length
#mu0 = 0, sig0 = 1
mu <- matrix(0,dim(iris)[1])
sig <- matrix(1,dim(iris)[1])
for(i in 2:dim(iris)[1]) {
  sig[i] <- rgamma(1,(length(x)/2),rate=sum((x-mu[i-1])^2)/2)
  sig[i] <- 1/sig[i]
  mu[i] <-rnorm(1,mean=mean(x),sd=sig[i]/(length(x)))
}

#Media y varianza con GIBBS
mean(mu)
mean(sig)

hist(mu)
hist(sig)

hist(mu[10000:30000])
hist(sig[10000:30000])

hist(x,prob=TRUE)
min <- min(x)
max <- max(x)
t <- seq(min, max, length=1000)
hx <- rnorm(10000, mean(mu), sqrt(mean(sig)))
lines(density(hx), col = "blue")

#En el caso de varias X normales
x<- rnorm(30000,5,1)
mean(x) 
var(x)
#mu0 = 0, sig0 = 1
mu <- matrix(0,length(x))
sig <- matrix(1,length(x))
for(i in 2:length(x)) {
  sig[i] <- rgamma(1,(length(x)/2),rate=sum((x-mu[i-1])^2)/2)
  sig[i] <- 1/sig[i]
  mu[i] <-rnorm(1,mean=mean(x),sd=sig[i]/(length(x)))
}

#Media y varianza con GIBBS
mean(mu)
mean(sig)

hist(mu[10000:30000])
hist(sig[10000:30000])

hist(x,prob=TRUE)
min <- min(x)
max <- max(x)
t <- seq(min, max, length=1000)
hx <- rnorm(10000, mean(mu), sqrt(mean(sig)))
lines(density(hx), col = "blue")
