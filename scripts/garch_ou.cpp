#include <Rcpp.h>
using namespace Rcpp;

// The function garch_proc() simulates a GARCH model
//' @export
// [[Rcpp::export]]
NumericMatrix garch_proc(int len_gth, double om_ega, double al_pha, double be_ta, NumericVector r_norm) {
  NumericVector vari_ance(len_gth);
  NumericVector re_turns(len_gth);
  vari_ance[0] = om_ega/(1-al_pha-be_ta);
  re_turns[0] = sqrt(vari_ance[0])*r_norm[0];
  for (int i = 1; i < len_gth; ++i) {
    re_turns[i] = sqrt(vari_ance[i-1])*r_norm[i];
    vari_ance[i] = om_ega + al_pha*pow(re_turns[i], 2) + be_ta*vari_ance[i-1];
  }
  return cbind(re_turns, vari_ance);
}
