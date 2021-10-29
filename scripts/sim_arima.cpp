// This file must first be compiled:
//  Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/sim_arima.cpp")

// Rcpp header with information for C++ compiler
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace arma;

// The function sim_arima() simulates an AR proccess using RcppArmadillo.
//' 
//' @param in_nov A numeric \emph{vector} of innovations (random numbers).
//' @param co_eff A numeric \emph{vector} of \emph{ARIMA} coefficients.
//'
//' @return A numeric \emph{vector} of the same length as the argument
//'   \code{in_nov}.
//'
//' @details The function \code{sim_arima()} calculates the recursive filter of
//'   \emph{ARIMA} coefficients over a vector of innovations, using 
//'   \emph{RcppArmadillo}. It performs the same calculation as the standard 
//'   \emph{R} function \code{filter(x=in_nov, filter=co_eff, 
//'   method="recursive")}, but it's about six times faster.
//'   
//' @examples
//' \dontrun{
//' # Create vector of innovations
//' in_nov <- rnorm(100)
//' # Create ARIMA coefficients
//' co_eff <- c(-0.8, 0.2)
//' # Calculate recursive filter using filter()
//' filter_ed <- filter(in_nov, filter=co_eff, method="recursive")
//' # Calculate recursive filter using sim_arima()
//' ari_ma <- sim_arima(in_nov, rev(co_eff))
//' # compare the two methods
//' all.equal(as.numeric(ari_ma), as.numeric(filter_ed))
//' }
//' @export
// [[Rcpp::export]]
arma::vec sim_arima(const arma::vec& in_nov, const arma::vec& co_eff) {
  uword n_rows = in_nov.n_elem;
  uword look_back = co_eff.n_elem;
  arma::vec ari_ma(n_rows);

  // Startup period
  ari_ma(0) = in_nov(0);
  ari_ma(1) = in_nov(1) + co_eff(look_back-1) * ari_ma(0);
  for (uword it = 2; it < look_back-1; it++) {
    ari_ma(it) = in_nov(it) + arma::dot(co_eff.subvec(look_back-it, look_back-1), ari_ma.subvec(0, it-1));
  }  // end for
  
  // Remaining periods
  for (uword it = look_back; it < n_rows; it++) {
    ari_ma(it) = in_nov(it) + arma::dot(co_eff, ari_ma.subvec(it-look_back, it-1));
  }  // end for
  
  return ari_ma;
}  // end sim_arima


