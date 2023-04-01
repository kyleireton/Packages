#' Compute the ex-Gaussian distribution
#'
#' @param x a vector of numeric values
#' @param mu the mean of the normal distribution
#' @param sigma the standard deviation of the normal distribution
#' @param tau the mean of the exponential distribution
#' @return a numeric vector of probability density values
#' @export


exgaussian <- function(x, mu, sigma, tau) {
  if (!is.numeric(x) || !is.numeric(mu) || !is.numeric(sigma) || !is.numeric(tau)) {
    stop("All arguments must be numeric")
  }
  if (length(mu) != 1 || length(sigma) != 1 || length(tau) != 1) {
    stop("mu, sigma, and tau must be single numeric values")
  }
  .Call("exgaussian_cpp", x, mu, sigma, tau)
}