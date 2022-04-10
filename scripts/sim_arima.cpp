// This file must first be compiled:
//  Rcpp::sourceCpp(file="/Users/jerzy/Develop/lecture_slides/scripts/sim_arima.cpp")

// Rcpp header with information for C++ compiler
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace arma;


////////////////////////////////////////////////////////////
//' Simulate \emph{autoregressive} returns by recursively filtering a
//' \emph{matrix} of innovations through a \emph{matrix} of
//' \emph{autoregressive} coefficients.
//' 
//' @param \code{innov} A single-column \emph{matrix} of innovations.
//' 
//' @param \code{coeff} A single-column \emph{matrix} of \emph{autoregressive}
//'   coefficients.
//'
//' @return A single-column \emph{matrix} of simulated returns, with the same
//'   number of rows as the argument \code{innov}.
//'
//' @details
//'   The function \code{sim_ar()} recursively filters the \emph{matrix} of
//'   innovations \code{innov} through the \emph{matrix} of
//'   \emph{autoregressive} coefficients \code{coeff}, using fast
//'   \code{RcppArmadillo} \code{C++} code.
//'
//'   The function \code{sim_ar()} simulates an \emph{autoregressive} process
//'   \eqn{AR(n)} of order \eqn{n}:
//'   \deqn{
//'     r_i = \varphi_1 r_{i-1} + \varphi_2 r_{i-2} + \ldots + \varphi_n r_{i-n} + \xi_i
//'   }
//'   Where \eqn{r_i} is the simulated output time series, \eqn{\varphi_i} are
//'   the \emph{autoregressive} coefficients, and \eqn{\xi_i} are the standard
//'   normal \emph{innovations}.
//'
//'   The order \eqn{n} of the \emph{autoregressive} process \eqn{AR(n)}, is
//'   equal to the number of rows of the \emph{autoregressive} coefficients
//'   \code{coeff}.
//'
//'   The function \code{sim_ar()} performs the same calculation as the standard
//'   \code{R} function \cr\code{filter(x=innov, filter=coeff,
//'   method="recursive")}, but it's several times faster.
//'   
//' @examples
//' \dontrun{
//' # Define AR coefficients
//' coeff <- matrix(c(0.1, 0.3, 0.5))
//' # Calculate matrix of innovations
//' innov <- matrix(rnorm(1e4, sd=0.01))
//' # Calculate recursive filter using filter()
//' filtered <- filter(innov, filter=coeff, method="recursive")
//' # Calculate recursive filter using RcppArmadillo
//' returns <- HighFreq::sim_ar(coeff, innov)
//' # Compare the two methods
//' all.equal(as.numeric(returns), as.numeric(filtered))
//' # Compare the speed of RcppArmadillo with R code
//' library(microbenchmark)
//' summary(microbenchmark(
//'   Rcpp=HighFreq::sim_ar(coeff, innov),
//'   Rcode=filter(innov, filter=coeff, method="recursive"),
//'   times=10))[, c(1, 4, 5)]  # end microbenchmark summary
//' }
//' 
//' @export
// [[Rcpp::export]]
arma::mat sim_ar(arma::mat& coeff, const arma::mat& innov) {
  
  arma::uword nrows = innov.n_rows;
  arma::uword ncoeff = coeff.n_rows;
  arma::mat coeffr = arma::reverse(coeff);
  arma::mat returns = arma::zeros<mat>(nrows, 1);
  
  // Warmup period
  returns.row(0) = innov.row(0);
  returns.row(1) = innov.row(1) + coeffr.row(ncoeff-1) * returns.row(0);
  for (arma::uword it = 2; it < ncoeff; it++) {
    returns.row(it) = innov.row(it) + arma::dot(coeffr.rows(ncoeff-it, ncoeff-1), returns.rows(0, it-1));
  }  // end for
  
  // Remaining periods
  for (arma::uword it = ncoeff; it < nrows; it++) {
    returns.row(it) = innov.row(it) + arma::dot(coeffr, returns.rows(it-ncoeff, it-1));
  }  // end for
  
  return returns;
  
}  // end sim_ar
