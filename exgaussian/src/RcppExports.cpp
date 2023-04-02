#include <Rcpp.h>
using namespace Rcpp;

// Define the ex-Gaussian density function
double d_exgaussian(double x, double mu, double sigma, double tau) {
  double z = (x - mu) / sigma;
  double lambda = 1.0 / tau;
  double phi = R::dnorm(z, 0.0, 1.0, false);
  double Phi = R::pnorm(lambda * z, 0.0, 1.0, true, false);
  double c = lambda / 2.0 * exp(lambda * (mu + sigma * sigma / (2.0 * tau)) - lambda * x);
  return c * Phi * exp(phi);
}

// Define the Rcpp wrapper function
// [[Rcpp::export]]
NumericVector exgaussian_cpp(double x, double mu, double sigma, double tau) {
  int n = x.length();
  NumericVector y(n);
  for (int i = 0; i < n; i++) {
    y[i] = d_exgaussian(x[i], mu, sigma, tau);
  }
  return y;
}

// Define the registration code
RCPP_MODULE(mod_exgaussian) {
  function("d_exgaussian", &d_exgaussian);
  function("exgaussian_cpp", &exgaussian_cpp);
}
