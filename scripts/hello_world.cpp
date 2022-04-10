// Simple program to test Rcpp
// In R, compile this file as follows:
//  Rcpp::sourceCpp("/Users/jerzy/Develop/lecture_slides/scripts/hello_world.cpp")
// Then in R run: hello()

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

//' @export
// [[Rcpp::export]]
void hello() {
  
  cout << "Hello World!" << endl;
  
}  // end hello
