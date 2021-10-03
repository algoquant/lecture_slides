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

// Define function rcpp_mult()
// [[Rcpp::export]]
double rcpp_mult(double x, double y) {
  return x * y;
}
