// Rcpp header with information for C++ compiler
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]


// The function inner_vec() calculates the inner (dot) product of two vectors.
// It uses RcppArmadillo.
//' @export
// [[Rcpp::export]]
double inner_vec(arma::vec vec1, arma::vec vec2) {
  
  return arma::dot(vec1, vec2);
  
}  // end inner_vec

// The function inner_mat() calculates the inner (dot) product of a matrix
// with two vectors.
// It accepts pointers to the matrix and vectors, and returns a double.
// It uses RcppArmadillo.
//' @export
// [[Rcpp::export]]
double inner_mat(const arma::vec& vec_tor2, const arma::mat& mat_rix, const arma::vec& vec_tor1) {
  return arma::as_scalar(trans(vec_tor2) * (mat_rix * vec_tor1));
}  // end inner_mat


// The function demean_mat() calculates a matrix with de-meaned columns.
// It accepts a pointer to a matrix and operates on the matrix in place.
// It returns the number of columns of the input matrix.
// It uses RcppArmadillo.
//' @export
// [[Rcpp::export]]
int demean_mat(arma::mat& mat_rix) {
  
  for (uword i = 0; i < mat_rix.n_cols; i++) {
    mat_rix.col(i) -= arma::mean(mat_rix.col(i));
  }  // end for
  
  return mat_rix.n_cols;
  
}  // end demean_mat


// The function inv_mat() calculates the inverse of symmetric positive
// definite matrix.
// It accepts a pointer to a matrix and operates on the matrix in place.
// It returns the number of columns of the input matrix.
// It uses RcppArmadillo.
//' @export
// [[Rcpp::export]]
double inv_mat(arma::mat& mat_rix) {
  
  mat_rix = arma::inv_sympd(mat_rix);
  
  return mat_rix.n_cols;
  
}  // end inv_mat

