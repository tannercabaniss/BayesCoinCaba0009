#' Single Beta Function Plot
#'
#' @param n the number of trials run for the given experiment
#' @param y the number of successes
#' @param alpha the alpha hyperparameter for the beta distribution
#' @param beta the beta hyperparameter for the beta distribution
#' @param a the alpha parameter for determining the tail probability (interval)
#'
#' @return a plot of the prior, likelihood, and posterior densities
#' @export
#'
#' @examples single_Beta(n=10, y=4, alpha=2, beta=8, a=0.025)
single_Beta <- function(n, y, alpha, beta, a) {
  theta <- seq(0, 1, length.out = 1000)
  prior <- rbeta(10000, alpha, beta)
  likelihood <- dbinom(y, size = n, prob = theta)
  posterior <- rbeta(10000, alpha + y, beta + n - y)

  plot(density(prior), main = "Bayesian Densities", xlim = c(0, 1), ylim = c(0, max(density(likelihood)$y)), xlab = "Theta", ylab= "Density")
  lines(density(likelihood), col = "blue")
  lines(density(posterior), col = "red")
  legend("topright", legend = c("Prior", "Likelihood", "Posterior"), col = c("black", "blue", "red"), lty = 1)

  point_estimate <- (alpha + y) / (alpha + beta + n)
  interval_estimate <- qbeta(c(a, 1 - a), alpha + y, beta + n - y)
  estimate_list <- list("Point Estimate" = point_estimate, "Interval Estimate" = interval_estimate)
  print(estimate_list)
}
