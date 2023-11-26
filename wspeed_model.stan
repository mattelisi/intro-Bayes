data {
  int<lower=0> N;
  vector[N] wspeed;
  vector[N] wage;
}
parameters {
  real beta[2];
  real<lower=0> sigma;
}
model {
  // priors
  beta[1] ~ normal(0, 1);
  beta[2] ~ normal(0.5, 1);
  sigma ~ normal(0,1)T[0,];
  
  // likelihood
  wspeed ~ normal(beta[1] + beta[2]*wage, sigma);
}

