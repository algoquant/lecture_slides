// Rcpp header with information for C++ compiler
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>  // include Rcpp C++ header files
using namespace std;
using namespace Rcpp; // use Rcpp C++ namespace
using namespace arma;

// Fast portfolio optimization using matrix algebra and RcppArmadillo
// function Rcpp::sourceCpp()
// (or via the Source button on the editor toolbar).
// Learn more about Rcpp at:
//
//   http://www.rcpp.org/

// function uni_form() produces a vector of
// uniformly distributed pseudo-random numbers

//' @export
// [[Rcpp::export]]
arma::mat calc_inv(const arma::mat& mat_rix, const arma::uword& max_eigen) {
  arma::mat eigen_vec;
  arma::vec eigen_val;
  
  arma::eig_sym(eigen_val, eigen_vec, cov(mat_rix));
  eigen_vec = eigen_vec.cols(eigen_vec.n_cols-max_eigen, eigen_vec.n_cols-1);
  eigen_val = 1/eigen_val.subvec(eigen_val.n_elem-max_eigen, eigen_val.n_elem-1);
  // arma::mat eigen_valmat = diagmat(eigen_val);
  
  return eigen_vec*diagmat(eigen_val)*eigen_vec.t();
  
}  // end calc_inv


//' @export
// [[Rcpp::export]]
arma::vec calc_weights(const arma::mat& re_turns, 
                       const std::string& typ_e = "max_sharpe",
                       int max_eigen = 1,
                       const double& pro_b = 0.1,
                       const double& al_pha = 0.0,
                       const bool scal_e = true) {
  // Initialize
  arma::vec weight_s(re_turns.n_cols);
  if (max_eigen == 1)  max_eigen = re_turns.n_cols;
  
  // Calculate weights depending on typ_e
  if (typ_e == "max_sharpe") {
    // Mean returns by columns
    arma::vec mean_cols = arma::trans(arma::mean(re_turns, 0));
    // Shrink mean_cols to the mean of re_turns
    mean_cols = ((1-al_pha)*mean_cols + al_pha*arma::mean(mean_cols));
    // Apply regularized inverse
    // arma::mat in_verse = calc_inv(re_turns, max_eigen);
    weight_s = calc_inv(re_turns, max_eigen)*mean_cols;
  } else if (typ_e == "max_sharpe_median") {
    // Mean returns by columns
    arma::vec mean_cols = arma::trans(arma::median(re_turns, 0));
    // Shrink mean_cols to the mean of re_turns
    mean_cols = ((1-al_pha)*mean_cols + al_pha*arma::median(mean_cols));
    // Apply regularized inverse
    // arma::mat in_verse = calc_inv(re_turns, max_eigen);
    weight_s = calc_inv(re_turns, max_eigen)*mean_cols;
  } else if (typ_e == "min_var") {
    // Apply regularized inverse to unit vector
    weight_s = calc_inv(re_turns, max_eigen)*arma::ones(re_turns.n_cols);
  } else if (typ_e == "min_varpca") {
    // Calculate highest order principal component
    arma::vec eigen_val;
    arma::mat eigen_vec;
    arma::eig_sym(eigen_val, eigen_vec, cov(re_turns));
    weight_s = eigen_vec.col(0);
  } else if (typ_e == "rank") {
    // Mean returns by columns
    arma::vec mean_cols = arma::trans(arma::mean(re_turns, 0));
    // Standard deviation by columns
    arma::vec sd_cols = arma::trans(arma::stddev(re_turns, 0));
    sd_cols.replace(0, 1);
    mean_cols = mean_cols/sd_cols;
    // Weights equal to ranks of Sharpe
    weight_s = conv_to< vec >::from(arma::sort_index(arma::sort_index(mean_cols)));
    weight_s = (weight_s - arma::mean(weight_s));
  } else if (typ_e == "rankrob") {
    // Median returns by columns
    arma::vec mean_cols = arma::trans(arma::median(re_turns, 0));
    // mean_cols = ((1-al_pha)*mean_cols + al_pha*arma::mean(mean_cols));
    // Standard deviation by columns
    arma::vec sd_cols = arma::trans(arma::stddev(re_turns, 0));
    sd_cols.replace(0, 1);
    mean_cols = mean_cols/sd_cols;
    // Apply regularized inverse
    weight_s = conv_to< vec >::from(arma::sort_index(arma::sort_index(mean_cols)));
    weight_s = (weight_s - arma::mean(weight_s));
  } else if (typ_e == "quan_tile") {
    // Sum of quantiles for columns
    arma::vec prob_s = {pro_b, 1-pro_b};
    weight_s = conv_to< vec >::from(arma::sum(arma::quantile(re_turns, prob_s, 0), 0));
    // Weights equal to ranks
    weight_s = conv_to< vec >::from(arma::sort_index(arma::sort_index(weight_s)));
    weight_s = (weight_s - arma::mean(weight_s));
  } else {
    cout << "Warning: Incorrect typ_e argument: " << typ_e << endl;
    return arma::ones(re_turns.n_cols);
  }  // end if
  
  if (scal_e == TRUE) {
    return weight_s*arma::stddev(arma::mean(re_turns, 1))/arma::stddev(re_turns*weight_s);
  }  // end if
  
  return weight_s;
}  // end calc_weights
