#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. 
// You can source this function into an R session using the 
// function Rcpp::sourceCpp() 
// (or via the Source button on the editor toolbar).
// Learn more about Rcpp at:
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/

// Define function rcpp_mult() to multiply two numbers
// [[Rcpp::export]]
double rcpp_mult(double x, double y) {
  return x * y;
}

// Define function rcpp_mult_vec() to multiply two vectors
// [[Rcpp::export]]
NumericVector rcpp_mult_vec(NumericVector x, NumericVector y) {
  return x * y;
}
