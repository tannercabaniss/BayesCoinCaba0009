#' Double Beta
#'
#' @param n number of successes
#' @param y number of attempts
#' @param p proportion of each beta distribution in mixture
#' @param alpha1 alpha hyperparameter for first beta distribution
#' @param alpha2 alpha hyperparameter for second beta distribution
#' @param beta1 beta hyperparameter for first beta distribution
#' @param beta2 beta hyperparameter for first beta distribution
#' @param a the alpha value used to compute the tail probability (interval)
#'
#' @return a plot of the prior density, a plot of the posterior density, and a list of the point and interval estimates
#' @export
#'
#' @examples double_Beta(10, 4, 0.5, 2, 8, 8, 2, 0.025)
double_Beta <- function(n, y, p, alpha1, alpha2, beta1, beta2, a) {
  theta <- seq(0, 1, length.out = 1000)
  mixed_prior <- p*dbeta(theta, alpha1, beta1)+(1-p)*dbeta(theta, alpha2, beta2)
  plot(theta, mixed_prior, type = "l", main = "Bayesian Prior", xlim = c(0, 1), ylim = c(0, max(mixed_prior)), xlab = "Theta", ylab= "Density")

  mixed_posterior <- p*dbeta(theta,alpha1+y,n-y+beta1)+(1-p)*dbeta(theta,alpha2+y,n-y+beta2)
  plot(theta, mixed_posterior, type = "l", main = "Bayesian Posterior", xlim = c(0, 1), ylim = c(0, max(mixed_posterior)), xlab = "Theta", ylab= "Density")

  point_estimate <- theta[which.max(mixed_posterior)]

  # Interval estimate
  cdf <- cumsum(mixed_posterior)/sum(mixed_posterior)
  lower_bound <- theta[min(which(cdf > a))]
  upper_bound <- theta[max(which(cdf < 1 - a))]
  interval_estimate <- c(lower_bound, upper_bound)

  estimate_list <- list("Point Estimate" = point_estimate, "Interval Estimate" = interval_estimate)
  print(estimate_list)
}
