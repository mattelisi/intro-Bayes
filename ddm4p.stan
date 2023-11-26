// 4 parameters ddm
data {
    int<lower=0> N;  // Number of observations
    int<lower=0,upper=1> resp[N];  // Response: 1 or 0
    real<lower=0> rt[N];  // Response time
}

parameters {
    real v;  // Drift rate
    real<lower=0> a;  // Boundary separation
    real<lower=0> t0;  // Non-decision time
    real<lower=0, upper=1> z;  // Starting point
}

model {
    // Priors
    v ~ normal(1, 3);
    a ~ normal(1, 1)T[0,2];
    t0 ~ normal(0, 3)T[0,];
    z ~ beta(2, 2);

    // Likelihood
    for (i in 1:N) {
        if (resp[i] == 1) {
            rt[i] ~ wiener(a, t0, z, v);
        } else {
            rt[i] ~ wiener(a, t0, 1 - z, -v);
        }
    }
}
