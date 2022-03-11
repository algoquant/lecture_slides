// This file must first be compiled:
//  Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/sim_arima.cpp")

// Rcpp header with information for C++ compiler
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace arma;

// The function sim_arima() simulates an AR proccess using RcppArmadillo.
//' 
//' @param innov A numeric \emph{vector} of innovations (random numbers).
//' @param coeff A numeric \emph{vector} of \emph{ARIMA} coefficients.
//'
//' @return A numeric \emph{vector} of the same length as the argument
//'   \code{innov}.
//'
//' @details The function \code{sim_arima()} calculates the recursive filter of
//'   \emph{ARIMA} coefficients over a vector of innovations, using 
//'   \emph{RcppArmadillo}. It performs the same calculation as the standard 
//'   \emph{R} function \code{filter(x=innov, filter=coeff, 
//'   method="recursive")}, but it's about six times faster.
//'   
//' @examples
//' \dontrun{
//' # Create vector of innovations
//' innov <- rnorm(100)
//' # Create ARIMA coefficients
//' coeff <- c(-0.8, 0.2)
//' # Calculate recursive filter using filter()
//' filter_ed <- filter(innov, filter=coeff, method="recursive")
//' # Calculate recursive filter using sim_arima()
//' arimav <- sim_arima(innov, rev(coeff))
//' # compare the two methods
//' all.equal(as.numeric(arimav), as.numeric(filter_ed))
//' }
//' @export
// [[Rcpp::export]]
arma::vec sim_arima(const arma::vec& innov, const arma::vec& coeff) {
  uword numrows = innov.n_elem;
  uword look_back = coeff.n_elem;
  arma::vec arimav(numrows);

  // Startup period
  arimav(0) = innov(0);
  arimav(1) = innov(1) + coeff(look_back-1) * arimav(0);
  for (uword it = 2; it < look_back-1; it++) {
    arimav(it) = innov(it) + arma::dot(coeff.subvec(look_back-it, look_back-1), arimav.subvec(0, it-1));
  }  // end for
  
  // Remaining periods
  for (uword it = look_back; it < numrows; it++) {
    arimav(it) = innov(it) + arma::dot(coeff, arimav.subvec(it-look_back, it-1));
  }  // end for
  
  return arimav;
}  // end sim_arima


