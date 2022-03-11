// This file must first be compiled:
//  Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/sim_ou.cpp")

// Rcpp header with information for C++ compiler
#include <Rcpp.h> // include C++ header files
using namespace Rcpp; // use Rcpp C++ namespace

// The function sim_ou_rcpp() simulates an Ornstein-Uhlenbeck process
// [[Rcpp::export]]
NumericVector sim_ou_rcpp(double eq_price, 
                          double volat, 
                          double thetav, 
                          NumericVector innov) {
  int numrows = innov.size();
  NumericVector prices(numrows);
  NumericVector returns(numrows);
  prices[0] = eq_price;
  for (int it = 1; it < numrows; it++) {
    returns[it] = thetav*(eq_price - prices[it-1]) + volat*innov[it-1];
    prices[it] = prices[it-1] + returns[it];
  }  // end for
  return prices;
}  // end sim_ou_rcpp
