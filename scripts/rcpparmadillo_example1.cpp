// This is the first RcppArmadillo example in numerical_analysis.pdf

// Rcpp header with information for C++ compiler
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

// Examples of RcppArmadillo functions below

// vec_in() calculates the inner (dot) product of two vectors.
// It accepts pointers to the two vectors and returns a double.
//' @export
// [[Rcpp::export]]
double vec_in(const arma::vec& vec1, const arma::vec& vec2){
  return arma::dot(vec1, vec2);
}  // end vec_in

// mat_2vec_in() calculates the inner (dot) product of a matrix
// with two vectors.
// It accepts pointers to the matrix and vectors, and returns a double.
//' @export
// [[Rcpp::export]]
double mat_2vec_in(const arma::vec& vec_tor2, const arma::mat& mat_rix, const arma::vec& vec_tor1){
  return arma::as_scalar(trans(vec_tor2) * (mat_rix * vec_tor1));
}  // end mat_2vec_in
