// This STAN file will resolve the posterior for a mixed prior Bayesian model
data {
  int<lower=0> n; // number of trials
  int<lower=0> y; // number of successes
  real<lower=0,upper=1> p; // mixing proportion
  real<lower=0> alpha1; // parameters for first beta distribution
  real<lower=0> beta1; // parameters for first beta distribution
  real<lower=0> alpha2; // parameters for second beta distribution
  real<lower=0> beta2; // parameters for second beta distribution
}

parameters {
  real<lower=0,upper=1> theta; // probability of success
}

model {
  target += log_mix(p,
                    beta_lpdf(theta | alpha1 + y, n - y + beta1),
                    beta_lpdf(theta | alpha2 + y, n - y + beta2)); // mixture of Beta priors
  y ~ binomial(n, theta); // likelihood
}
