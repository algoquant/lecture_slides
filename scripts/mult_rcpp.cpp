// This file must first be compiled:
// Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/mult_rcpp.cpp")

// Rcpp header with information for C++ compiler
#include <Rcpp.h>
using namespace Rcpp;

//' The function mult_rcpp() multiplies two numbers.
//' @export
// [[Rcpp::export]]
double mult_rcpp(double x, double y) {
  return x * y;
}

// The function mult_vec_rcpp() multiplies two vectors
//' @export
// [[Rcpp::export]]
NumericVector mult_vec_rcpp(NumericVector x, NumericVector y) {
  return x * y;
}
