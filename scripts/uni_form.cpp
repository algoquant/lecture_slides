// This file must first be compiled:
//  Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/uni_form.cpp")

// Rcpp header with information for C++ compiler
#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. 
// You can source this function into an R session using the 
// function Rcpp::sourceCpp() 
// (or via the Source button on the editor toolbar).
// Learn more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/

// function uni_form() produces a vector of 
// uniformly distributed pseudo-random numbers
// [[Rcpp::export]]
NumericVector uniform_rcpp(double see_d, int len_gth) {
// define pi
static const double pi = 3.14159265;
// allocate output vector
  NumericVector out_put(len_gth);
// initialize output vector
  out_put[0] = see_d; 
// perform loop
  for (int i=1; i < len_gth; ++i) {
    out_put[i] = 4*out_put[i-1]*(1-out_put[i-1]);
  }  // end for
// rescale output vector and return it
  return acos(1-2*out_put)/pi;
}

