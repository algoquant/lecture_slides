// This file must first be compiled:
//  Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/sim_ou.cpp")

// Rcpp header with information for C++ compiler
#include <Rcpp.h> // include C++ header files
using namespace Rcpp; // use Rcpp C++ namespace

// The function bootstrap_rcpp() simulates an Ornstein-Uhlenbeck process
// [[Rcpp::export]]
NumericVector bootstrap_rcpp(double eq_price, 
                             double vol_at, 
                             double the_ta, 
                             NumericVector in_nov) {
  int len_gth = in_nov.size();
  NumericVector price_s(len_gth);
  NumericVector re_turns(len_gth);
  price_s[0] = eq_price;
  for (int it = 1; it < len_gth; it++) {
    re_turns[it] = the_ta*(eq_price - price_s[it-1]) + vol_at*in_nov[it-1];
    price_s[it] = price_s[it-1] * exp(re_turns[it]);
  }  // end for
  return price_s;
}  // end bootstrap_rcpp
