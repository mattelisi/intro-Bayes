# Metropolis algorithm example

rm(list=ls())
hablar::set_wd_to_script_path()

library(lme4)

# ----
# example subject from lme4 sleepstudy data:

par(mfrow=c(1,1))
with(sleepstudy[sleepstudy$Subject=="308",],
     plot( Days,Reaction,pch=19,main="308",xlim=c(-1,11),ylim=c(200,500))
)

# save data in vectors x and y to use in the below functions
x <- sleepstudy$Days[sleepstudy$Subject=="308"]
y <- sleepstudy$Reaction[sleepstudy$Subject=="308"]

# ----
# log-likelihood function
loglik <- function(par){
  # parameter vector = [intercept, slope, log(SD)]
  pred <- par[1] + par[2]*x
  return(sum(dnorm(y, mean = pred, sd = exp(par[3]), log = TRUE)))  #<<
}

# ----
# plot priors
par(mfrow=c(1,3))
curve(dnorm(x, mean=250, sd=180),from=0, to=1000, xlab="Intercept",ylab="prior density", col="blue")
curve(dnorm(x, mean=20, sd=20),from=-50, to=50, xlab="Slope",ylab="prior density", col="blue")
x_pSD <- seq(-1,6, length.out = 500)
y_pSD <- dnorm(x_pSD , mean=4,sd=1)
plot(exp(x_pSD),y_pSD, type="l", xlab=expression(sigma[epsilon]),ylab="prior density", col="blue")

# log-prior function
logprior <- function(par){
  intercept_prior <- dnorm(par[1], mean=250, sd=180, log=TRUE)
  slope_prior <- dnorm(par[2], mean=20, sd=20, log=TRUE)
  sd_prior <- dnorm(par[3],mean=4, sd=1, log=TRUE)
  return(intercept_prior+slope_prior+sd_prior) #<<
}

# ----
# log-posterior (un-normalized) function
logposterior <- function(par){
  return (loglik(par) + logprior(par))
}


# ----
# Metropolis sampling:

# initial parameters
startvalue <- c(250, 20, 5) #  [intercept, slope, log(SD)]

# proposal density
proposalfunction <- function(par){
  return(rnorm(3, mean = par, sd= c(15,5,0.2))) #<<
}

# sampling function
run_metropolis_MCMC <- function(startvalue, iterations){
  
  # set up an empty array to store smapled values
  chain <- array(dim = c(iterations+1,3))
  
  # put starting values at top of arrays
  chain[1,] <- startvalue
  
  for (i in 1:iterations){
    
    # draw a random proposal
    proposal <- proposalfunction(chain[i,])
    
    # ratio of posterior density between new and old values
    a <- exp(logposterior(proposal) - logposterior(chain[i,]))
    
    # sample random number & accept/reject the parameter values
    if (runif(1) < a){
      chain[i+1,] <- proposal
    }else{
      chain[i+1,] <- chain[i,]
    }
  }
  return(chain)
}

# results 
chain <- run_metropolis_MCMC(startvalue, 20000)


# ----
# visualize results

burnIn <- 5000
acceptance <- 1-mean(duplicated(chain[-(1:burnIn),]))

LSfit <- lm(y~x)
interceptLS <- coef(LSfit)[1]
slopeLS <- coef(LSfit)[2]
sigmaLS <- summary(LSfit)$sigma

par(mfrow = c(2,3))
hist(chain[-(1:burnIn),1],main="Intercept",border="white",col="dark grey", breaks=20)
abline(v = interceptLS, col="red",lwd=2)
hist(chain[-(1:burnIn),2],main="Slope",border="white",col="dark grey", breaks=20)
abline(v = slopeLS , col="red",lwd=2)
hist(exp(chain[-(1:burnIn),3]),main=expression(sigma[epsilon]),border="white",col="dark grey", breaks=20)
abline(v = sigmaLS, col="red" ,lwd=2)

plot(chain[-(1:burnIn),1], type = "l", main = "Chain values")
abline(h = interceptLS, col="red",lwd=2)
plot(chain[-(1:burnIn),2], type = "l" , main = "Chain values")
abline(h = slopeLS, col="red",lwd=2)
plot(exp(chain[-(1:burnIn),3]), type = "l" , main = "Chain values")
abline(h = sigmaLS, col="red",lwd=2)



# ----
# compute credible interval using percentile method

# remove initial 'burn in' samples
burnIn <- 5000
slope_samples <- chain[-(1:burnIn),2]

# mean of posterior distribution
mean(slope_samples)

# 95% Bayesian credible interval
alpha <- 0.05
round(c(quantile(slope_samples, probs = alpha/2),
        quantile(slope_samples, probs = 1-alpha/2)),
      digits=2)