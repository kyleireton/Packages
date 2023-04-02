
#include <Rcpp.h>
#include "exgaussian.h"
using namespace Rcpp;

// [[Rcpp::export]]
// Define the ex-Gaussian density function
NumericVector d_exgaussian(NumericVector x, NumericVector mu, NumericVector sigma, NumericVector tau) {
  NumericVector z = (x - mu) / sigma;
  NumericVector lambda = 1.0 / tau;
  NumericVector phi = R::dnorm(z, 0.0, 1.0, false);
  NumericVector Phi = R::pnorm(lambda * z, 0.0, 1.0, true, false);
  NumericVector c = lambda / 2.0 * exp(lambda * (mu + sigma * sigma / (2.0 * tau)) - lambda * x);
  return c * Phi * exp(phi);
}

// Define the Rcpp wrapper function
// [[Rcpp::export]]
NumericVector exgaussian_cpp(NumericVector x, NumericVector mu, NumericVector sigma, NumericVector tau) {
  int n = x.length();
  NumericVector y(n);
  for (int i = 0; i < n; i++) {
    y[i] = d_exgaussian(x[i], mu, sigma, tau);
  }
  return y;
}
