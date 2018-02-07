// This is the second RcppArmadillo example in numerical_analysis.pdf

// Rcpp header with information for C++ compiler
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

// Examples of RcppArmadillo functions below

// demean_mat() calculates a matrix with de-meaned columns.
// It accepts a pointer to a matrix and operates on the matrix in place.
// It returns the number of columns of the input matrix.
//' @export
// [[Rcpp::export]]
double demean_mat(arma::mat& mat_rix){
  for (unsigned int i = 0; i < mat_rix.n_cols; i++) {
    mat_rix.col(i) -= arma::mean(mat_rix.col(i));
  }  // end for
  return mat_rix.n_cols;
}  // end demean_mat

// inv_mat() calculates the inverse of symmetric positive
// definite matrix.
// It accepts a pointer to a matrix and operates on the matrix in place.
// It returns the number of columns of the input matrix.
// It uses RcppArmadillo.
//' @export
// [[Rcpp::export]]
double inv_mat(arma::mat& mat_rix){
  mat_rix = arma::inv_sympd(mat_rix);
  return mat_rix.n_cols;
}  // end inv_mat
