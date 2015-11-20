library(Rcpp)
library(ggplot2)

# Estimación de media
data(iris)
head(iris)

names(iris)

## Regresión hecha por R
mod <- lm(iris$Sepal.Length ~ iris$Sepal.Width + iris$Petal.Length + iris$Petal.Width)
summary(mod)
plot(mod)

##A mano
X<- as.matrix(cbind(1,iris[,2:4]))
Y<- iris[,1]
beta.hat <- solve(t(X) %*% X, t(X) %*% Y)
Y.pred <- X %*% beta.hat
errors <- Y-Y.pred
hist(errors)
qqnorm(errors)
plot(errors,Y.pred)

beta.hat

##BAYES STATISTICS
prior.beta1 <- function(x) dnorm(x, 1, 0.2)
prior.beta2 <- function(x) dnorm(x, 1, 0.2)
prior.beta3 <- function(x) dnorm(x, 1, 0.2)

plot(prior.beta1, col="darkblue", xlim=c(-5,18), lwd="2", main="Prior for beta1", ylab="density")
plot(prior.beta2 , col="darkred", xlim=c(-5,18), lwd="2", main="Prior for beta2", ylab="density")
plot(prior.beta3 , col="darkgreen", xlim=c(-5,18), lwd="2", main="Prior for beta3", ylab="density")

#Variables independientes
data <- data.frame(Sepal.Width=iris$Sepal.Width,Petal.Width=iris$Petal.Width,Petal.Length=iris$Petal.Length)
dataMat <- as.matrix(data)
#La que queremos aproximar
Y <- matrix(iris$Sepal.Length, ncol=1)


cppFunction('
            NumericVector proposal(NumericVector theta,NumericMatrix X){
            int nparam = theta.size();
            int n = X.nrow();
            double jump = 0.25/sqrt(n); 
            NumericVector newtheta(nparam);
            for (int i=0; i<nparam; i++){
            newtheta[i] = R::rnorm(theta[i], jump);
            }
            return newtheta;
            }')
proposal(c(1,1,1,1),dataMat)

#Funcion objetivo
cppFunction('
            double objdens(NumericMatrix x, NumericVector theta, NumericMatrix y){
            int i;
            double lkh, logprior, yhat;
            int m = x.nrow();
            int p =x.ncol();
            NumericVector beta(p-1);
            double sd;
            
            //Primer valor de las betas
            for(i=0;i<p-1;i++){
              beta[i]=theta[i];
            }
            
            sd = theta[p-1]; //El error
            NumericVector aux(m);

            // Compute loglikelihood
            lkh=0;
            for (int i=0; i<m; i++){
              aux= x(i,_) * beta;
              yhat = std::accumulate(aux.begin(),aux.end(),0.0);
              lkh += -0.5*pow((y[i]-yhat)/sd,2)-log(sd);
            }
            // Compute logprior
            logprior = 0.0;
            for(int j = 0; j<p-1;j++){
              logprior += R::dnorm(beta[j],0.0,100,true);
            }
            logprior += R::dgamma(sd, 3.0, 0.5, true);
            // Log of target density
            return lkh + logprior;
}')

objdens(dataMat, c(1,1,1,1), Y)

sourceCpp("BayesianMHlineal.cpp")

nsim <- 10000
init <- c(1,1,1,1)
mh.samp <- MHBayes(nsim, init, objdens, proposal, dataMat, Y)
estims <- mh.samp$theta

#  SOME DIAGNOSTIC IMPORTANT STUFF
#  Exploration graph:
library(calibrate)
pts <- seq(1,100,by=5)
plot(estims[pts, ], type="l", xlab="mean", ylab="sd")
textxy(estims[pts,1], estims[pts,2], pts)
cor(estims)
### 1) REJECTION RATES
rejections <- mh.samp$rejections[-1]
trials <- rejections + 1
rej.rate <- cumsum(rejections)/cumsum(trials)
plot(rej.rate, type="l", ylim=c(0,1), main="Rejection rate")
plot(trials[-1], type="l", main="Number of trials")
### 2) AUTOCORRELATION
acf(estims[ , 1])
acf(estims[ , 2]) 
acf(estims[ , 3])
acf(estims[ , 4])
# burnin and subsampling
burnin <- 100
estim <- estims[-(1:burnin),]
thinning <- 0.75 # meaning we'll keep 75% of observations to reduce autocorrelation
# OBS: thinning is rarely usefull!!!! check that nothing changes
sub <- sample.int(nsim-burnin, size=round(thinning*nsim))
estims <- estims[sub, ]
acf(estims[ , 1])
acf(estims[ , 2])
acf(estims[ , 3])
acf(estims[ , 4])

# LET'S COMPARE PRIORS AND POSTERIORS AND DO INFERENCE

hist(estims[ ,1], prob=TRUE, xlim=c(0.5,2.5), breaks=20, col="lightgreen",
     main="Histogram and Posterior(blue) vs Prior(red) of beta1") # posterior distribution of mean
plot(prior.beta1, xlim=c(0.5,2.5), col="darkred", lwd="2", ylim=c(0,10), add=TRUE)
lines(density(estims[ ,1]), col="darkblue", lwd="2")

hist(estims[ ,2], prob=TRUE, xlim=c(0,1), breaks=40, col="yellow",
     main="Histogram and Posterior(blue) vs Prior(red) of beta2") # posterior distribution of mean
plot(prior.sd, xlim=c(0,1), col="darkred", lwd="2", ylim=c(0,10), add=TRUE)
lines(density(estims[ ,2]), col="darkblue", lwd="2")

hist(estims[ ,3], prob=TRUE, xlim=c(0,1), breaks=40, col="yellow",
     main="Histogram and Posterior(blue) vs Prior(red) of beta2") # posterior distribution of mean
plot(prior.sd, xlim=c(0,1), col="darkred", lwd="2", ylim=c(0,10), add=TRUE)
lines(density(estims[ ,3]), col="darkblue", lwd="2")

hist(estims[ ,4], prob=TRUE, xlim=c(0,1), breaks=40, col="yellow",
     main="Histogram and Posterior(blue) vs Prior(red) of beta2") # posterior distribution of mean
plot(prior.sd, xlim=c(0,1), col="darkred", lwd="2", ylim=c(0,10), add=TRUE)
lines(density(estims[ ,4]), col="darkblue", lwd="2")

mean(estims[ ,1]) # approx. mean-value of the posterior of beta1
mean(estims[ ,2]) # approx. mean-value of the posterior of beta2
mean(estims[ ,3]) # mean-value of the posterior beta3
mean(estims[ ,4]) # mean-value del error
# CERTAINTY INTERVALS
alpha <- 0.05
intervals1 <- quantile(estims[ ,1], c(alpha/2, 1-alpha/2))
intervals1
intervals2 <-quantile(estims[ ,2], c(alpha/2, 1-alpha/2)) 
intervals2
intervals3 <-quantile(estims[ ,3], c(alpha/2, 1-alpha/2)) 
intervals3
intervals4 <-quantile(estims[ ,4], c(alpha/2, 1-alpha/2)) 
intervals4

#Comparacion
estimaciones <- c(mean(estims[ ,4]),mean(estims[ ,1]),mean(estims[ ,2]),mean(estims[ ,3]))
summary(mod)
beta.hat