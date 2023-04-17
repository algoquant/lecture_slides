// Rcpp header with information for C++ compiler
// Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/calc_invmat.cpp")

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h> // include RcppArmadillo header file
using namespace arma; // use Armadillo C++ namespace

// [[Rcpp::export]]
arma::mat calc_invmat(arma::mat& matrixv) {
  return arma::inv(matrixv);
} // end calc _ invmat
