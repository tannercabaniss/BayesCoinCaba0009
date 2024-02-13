#' Run Stan Model
#'
#' @param n number of successes
#' @param y number of attempts
#' @param p proportion of each beta distribution in mixture
#' @param alpha1 alpha hyperparameter for first beta distribution
#' @param alpha2 alpha hyperparameter for second beta distribution
#' @param beta1 beta hyperparameter for first beta distribution
#' @param beta2 beta hyperparameter for first beta distribution
#' @param chains number of models to run
#' @param iter number of iterations for each model
#'
#' @return Histogram of the posterior and tabular summary of the MCMC posterior sample
#' @export
#'
#' @examples run_stan_model(10, 4, 0.5, 2, 8, 8, 2, 3, 10000)
run_stan_model <- function(n, y, p, alpha1, alpha2, beta1, beta2, chains = 3, iter = 10000) {
  # Prepare data for STAN model
  stan_data <- list(n = n, y = y, p = p, alpha1 = alpha1, alpha2 = alpha2, beta1 = beta1, beta2 = beta2)

  # Compile STAN model
  model <- rstan::stan_model(file = system.file("STAN", "mixed_posterior.stan", package = "BayesCoinCaba0009"))

  # Run MCMC
  fit <- rstan::sampling(model, data = stan_data, chains = chains, iter = iter)

  # Extract samples
  samples <- rstan::extract(fit)
  plot <- ggplot2::qplot(samples$theta, main = "Posterior Sample from STAN", ylab="Frequency", xlab="Theta")
  print(plot)
  # Print summary of MCMC posterior sample
  summary(samples$theta)
}
