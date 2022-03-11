// Rcpp header with information for C++ compiler
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>  // include Rcpp C++ header files
using namespace std;
using namespace Rcpp; // use Rcpp C++ namespace
using namespace arma;


////////////////////////////////////////////////////////////
// Define C++ enum type for the different methods for regularization,
// calculating variance, skewness, kurtosis, covariance, regression, 
// and matrix inverse.
enum method {moment, least_squares, quantile, nonparametric, regular, ranksharpe, 
              max_sharpe, max_sharpe_median, min_var, min_varpca, rank, rankrob};

// Map string to C++ enum type for switch statement.
// This is needed because Rcpp can't map C++ enum type to R variable SEXP.
method calc_method(std::string method) {
  if (method == "moment" || method == "m") 
    return method::moment;
  else if (method == "least_squares" || method == "l")
    return method::least_squares;
  else if (method == "quantile" || method == "q")
    return method::quantile;
  else if (method == "nonparametric" || method == "n")
    return method::nonparametric;
  else if (method == "regular")
    return method::regular;
  else if (method == "ranksharpe")
    return method::ranksharpe;
  else if (method == "max_sharpe")
    return method::max_sharpe;
  else if (method == "max_sharpe_median")
    return method::max_sharpe_median;
  else if (method == "min_var")
    return method::min_var;
  else if (method == "min_varpca")
    return method::min_varpca;
  else if (method == "rank")
    return method::rank;
  else if (method == "rankrob")
    return method::rankrob;
  else 
    return method::moment;
}  // end calc_method



//' The function calc_inv() calculates the regularized inverse of
//' \code{tseries} using Singular Value Decomposition (\emph{SVD}).
//' @export
// [[Rcpp::export]]
arma::mat calc_inv(const arma::mat& tseries,
                   double eigen_thresh = 0.001, 
                   arma::uword eigen_max = 0) {
  
  if (eigen_max == 0) {
    // Calculate the inverse using arma::pinv()
    return arma::pinv(tseries, eigen_thresh);
  } else {
    // Calculate the regularized inverse using SVD decomposition
    
    // Allocate SVD
    arma::vec svd_val;
    arma::mat svd_u, svd_v;
    
    // Calculate the SVD
    arma::svd(svd_u, svd_val, svd_v, tseries);
    
    // Subset the SVD
    eigen_max = eigen_max - 1;
    // For no regularization: eigen_max = tseries.n_cols
    svd_u = svd_u.cols(0, eigen_max);
    svd_v = svd_v.cols(0, eigen_max);
    svd_val = svd_val.subvec(0, eigen_max);
    
    // Calculate the inverse from the SVD
    return svd_v*arma::diagmat(1/svd_val)*svd_u.t();
    
  }  // end if
  
}  // end calc_inv



//' The function \code{calc_weights()} calculates the optimal portfolio
//' weights for different types of objective functions, using
//' \code{RcppArmadillo} \code{C++} code.
//' @export
// [[Rcpp::export]]
arma::vec calc_weights(const arma::mat& returns, // Portfolio returns
                       std::string method = "ranksharpe",
                       double eigen_thresh = 0.001,
                       arma::uword eigen_max = 0,
                       double confi = 0.1,
                       double alpha = 0.0,
                       bool scale = true,
                       double vol_target = 0.01) {
  // Initialize
  arma::vec weights(returns.n_cols, fill::zeros);
  if (eigen_max == 0)  eigen_max = returns.n_cols;
  
  // Switch for the different methods for weights
  switch(calc_method(method)) {
  case method::ranksharpe: {
    // Mean returns by columns
    arma::vec meancols = arma::trans(arma::mean(returns, 0));
    // Standard deviation by columns
    arma::vec sd_cols = arma::trans(arma::stddev(returns, 0));
    sd_cols.replace(0, 1);
    meancols = meancols/sd_cols;
    // Weights equal to ranks of Sharpe
    weights = conv_to<vec>::from(arma::sort_index(arma::sort_index(meancols)));
    weights = (weights - arma::mean(weights));
    break;
  }  // end ranksharpe
  case method::max_sharpe: {
    // Mean returns by columns
    arma::vec meancols = arma::trans(arma::mean(returns, 0));
    // Shrink meancols to the mean of returns
    meancols = ((1-alpha)*meancols + alpha*arma::mean(meancols));
    // Apply regularized inverse
    // arma::mat inverse = calc_inv(cov(returns), eigen_max);
    // weights = calc_inv(cov(returns), eigen_max)*meancols;
    weights = calc_inv(cov(returns), eigen_thresh, eigen_max)*meancols;
    break;
  }  // end max_sharpe
  case method::max_sharpe_median: {
    // Mean returns by columns
    arma::vec meancols = arma::trans(arma::median(returns, 0));
    // Shrink meancols to the mean of returns
    meancols = ((1-alpha)*meancols + alpha*arma::median(meancols));
    // Apply regularized inverse
    // arma::mat inverse = calc_inv(cov(returns), eigen_max);
    weights = calc_inv(cov(returns), eigen_thresh, eigen_max)*meancols;
    break;
  }  // end max_sharpe_median
  case method::min_var: {
    // Apply regularized inverse to unit vector
    weights = calc_inv(cov(returns), eigen_thresh, eigen_max)*arma::ones(returns.n_cols);
    break;
  }  // end min_var
  case method::min_varpca: {
    // Calculate highest order principal component
    arma::vec eigen_val;
    arma::mat eigen_vec;
    arma::eig_sym(eigen_val, eigen_vec, arma::cov(returns));
    weights = eigen_vec.col(0);
    break;
  }  // end min_varpca
  case method::rank: {
    // Mean returns by columns
    arma::vec meancols = arma::trans(arma::mean(returns, 0));
    // Standard deviation by columns
    arma::vec sd_cols = arma::trans(arma::stddev(returns, 0));
    sd_cols.replace(0, 1);
    meancols = meancols/sd_cols;
    // Weights equal to ranks of Sharpe
    weights = conv_to<vec>::from(arma::sort_index(arma::sort_index(meancols)));
    weights = (weights - arma::mean(weights));
    break;
  }  // end rank
  case method::rankrob: {
    // Median returns by columns
    arma::vec meancols = arma::trans(arma::median(returns, 0));
    // meancols = ((1-alpha)*meancols + alpha*arma::mean(meancols));
    // Standard deviation by columns
    arma::vec sd_cols = arma::trans(arma::stddev(returns, 0));
    sd_cols.replace(0, 1);
    meancols = meancols/sd_cols;
    // Apply regularized inverse
    // arma::mat inverse = calc_inv(cov(returns), eigen_max);
    // weights = calc_inv(cov(returns), eigen_max)*meancols;
    // weights = calc_inv(cov(returns), eigen_max)*meancols;
    // // Standard deviation by columns
    // arma::vec sd_cols = meancols;
    // for (arma::uword it=0; it < returns.n_cols; it++) {
    //   sd_cols(it) = arma::median(arma::abs((returns.col(it) - sd_cols)));
    // }  // end for
    // sd_cols.replace(0, 1);
    // meancols = meancols/sd_cols;
    // Weights equal to ranks of Sharpe
    weights = conv_to<vec>::from(arma::sort_index(arma::sort_index(meancols)));
    // level;
    weights = (weights - arma::mean(weights));
    break;
  }  // end rankrob
  case method::quantile: {
    // Sum of quantiles for columns
    arma::vec levels = {confi, 1-confi};
    weights = conv_to<vec>::from(arma::sum(arma::quantile(returns, levels, 0), 0));
    // Weights equal to ranks
    weights = conv_to<vec>::from(arma::sort_index(arma::sort_index(weights)));
    weights = (weights - arma::mean(weights));
    break;
  }  // end quantile
  default : {
    cout << "Warning: Invalid method parameter: " << method << endl;
    return arma::ones(returns.n_cols);
  }  // end default
  }  // end switch
  
  if (scale == TRUE) {
    // return weights/std::sqrt(sum(square(weights)));
    // return weights/sum(weights);
    // Returns of equally weighted portfolio
    // arma::vec meanumrows = arma::mean(returns, 1);
    // Returns of weighted portfolio
    // arma::vec returns_portf = returns*weights;
    // Scale weights to equally weighted portfolio and return them
    // return weights*arma::stddev(arma::mean(returns, 1))/arma::stddev(returns*weights);
    // Scale weights so the resulting portfolio has a volatility equal to vol_target
    return weights*vol_target/arma::stddev(returns*weights);
  }  // end if
  
  return weights;
  
}  // end calc_weights


//' @export
// [[Rcpp::export]]
arma::mat back_test(const arma::mat& excess, // Portfolio excess returns
                    const arma::mat& returns, // Portfolio returns
                    arma::uvec startp, 
                    arma::uvec endp, 
                    std::string method = "ranksharpe",
                    double eigen_thresh = 0.001,
                    arma::uword eigen_max = 0,
                    double confi = 0.1,
                    double alpha = 0.0,
                    bool scale = true,
                    double vol_target = 0.01,
                    double coeff = 1.0,
                    double bid_offer = 0.0) {
  
  arma::vec weights(returns.n_cols, fill::zeros);
  arma::vec weights_past = zeros(returns.n_cols);
  arma::mat pnls = zeros(returns.n_rows, 1);
  
  // Perform loop over the end points
  for (arma::uword it = 1; it < endp.size(); it++) {
    // cout << "it: " << it << endl;
    // Calculate portfolio weights
    weights = coeff*calc_weights(excess.rows(startp(it-1), endp(it-1)), method, eigen_thresh, eigen_max, confi, alpha, scale, vol_target);
    // Calculate out-of-sample returns
    pnls.rows(endp(it-1)+1, endp(it)) = returns.rows(endp(it-1)+1, endp(it))*weights;
    // Add transaction costs
    pnls.row(endp(it-1)+1) -= bid_offer*sum(abs(weights - weights_past))/2;
    weights_past = weights;
  }  // end for
  
  // Return the strategy pnls
  return pnls;
  
}  // end back_test
