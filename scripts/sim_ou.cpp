// This file must first be compiled:
//  Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/sim_ou.cpp")

// Rcpp header with information for C++ compiler
#include <Rcpp.h> // include C++ header files
using namespace Rcpp; // use Rcpp C++ namespace

// The function sim_ou_rcpp() simulates an Ornstein-Uhlenbeck process
// [[Rcpp::export]]
NumericVector sim_ou_rcpp(double eq_price, 
                          double vol_at, 
                          double the_ta, 
                          NumericVector in_nov) {
  int n_rows = in_nov.size();
  NumericVector price_s(n_rows);
  NumericVector re_turns(n_rows);
  price_s[0] = eq_price;
  for (int it = 1; it < n_rows; it++) {
    re_turns[it] = the_ta*(eq_price - price_s[it-1]) + vol_at*in_nov[it-1];
    price_s[it] = price_s[it-1] + re_turns[it];
  }  // end for
  return price_s;
}  // end sim_ou_rcpp
