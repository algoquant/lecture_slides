// This file must first be compiled:
//  Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/unifunc.cpp")

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

// function unifunc() produces a vector of 
// uniformly distributed pseudo-random numbers
// [[Rcpp::export]]
NumericVector unifuncpp(double seedv, int numrows) {
// define pi
static const double pi = 3.14159265;
// allocate output vector
  NumericVector output(numrows);
// initialize output vector
  output[0] = seedv; 
// perform loop
  for (int i=1; i < numrows; ++i) {
    output[i] = 4*output[i-1]*(1-output[i-1]);
  }  // end for
// rescale output vector and return it
  return acos(1-2*output)/pi;
}  // end unifuncpp

