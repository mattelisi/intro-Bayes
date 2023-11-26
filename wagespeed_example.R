
# Bayesian regression in stan example

rm(list=ls())
hablar::set_wd_to_script_path()

#
library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores()) # indicate stan to use multiple cores if available
library(tidybayes)

# ----
# data
d <- read.table ("./data/wagespeed.csv", 
                 header=T, 
                 sep=",")


head(d)

# ----
# prepare data for modelling:

# function to standardize variables
standardize <- function(x){ 
  Z <- (x - mean(x)) / sd(x)
  return(Z)
}

# prepare data to fit in Stan
data_stan <- list(
  N = nrow(d),
  wage = standardize(d$wage),
  wspeed = standardize(d$wspeed)
)

# ----
# run sampling and fit model

wspeed_fit <- stan(file = "wspeed_model.stan", data = data_stan, iter = 2000, chains = 4) 

print(wspeed_fit)


# ----
# examine fit

# traceplot
traceplot(wspeed_fit, pars=c("beta","sigma"))

# pairs plot
pairs(wspeed_fit, pars=c("beta","sigma"))

# visualize model fit and uncertainty drawing samples from posterior

posterior_samples <- wspeed_fit %>%
  spread_draws(beta[parameter]) %>%
  mutate(parameter = ifelse(parameter==1, "intercept", "slope")) %>%
  pivot_wider(names_from=parameter, values_from = beta) 

# Create the scatter plot
plot(standardize(d$wage), standardize(d$wspeed),
     xlab = "Hourly wage (Z-score)",
     ylab = "Mean walking speed (Z-score)",
     pch = 19,       # solid circle points
     col = "blue",   # blue color for the points
     cex.lab = 1.2,  # size of axis labels
     cex.main = 1.3, # size of plot title
     cex=1.3,
     xlim=c(-2.5, 2.5), ylim=c(-2.5,2.5)) 

# Add city labels, tilt only specified cities
text(standardize(d$wage), standardize(d$wspeed), labels = d$city, pos = 4, cex = 0.7, offset = 0.5, srt = 0)

# plot 300 lines from random pairs of posterior samples
plot_samples <- sample(1:4000, size=300)
for(i in plot_samples){
  abline(a=posterior_samples$intercept[i], #-mean(d$wage)*posterior_samples$slope[i],
         b=posterior_samples$slope[i],
         col=rgb(0,0,1,0.1))
}
abline(a=mean(posterior_samples$intercept), 
       b=mean(posterior_samples$slope),
       col="blue", lwd=3)

