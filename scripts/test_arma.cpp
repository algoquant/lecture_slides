////////////////////////////
// Functions to test C++ syntax with Armadillo
////////////////////////////

// Compile this file in R by running this command:
// Rcpp::sourceCpp(file="/Users/jerzy/Develop/Rcpp/test_arma.cpp")

// #include <Rcpp.h>
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// Use STL
using namespace std;
// [[Rcpp::depends(RcppArmadillo)]]


////////////////////////////////////////////////////////////
//' The function order_index() calculates the order index of a vector
//' using RcppArmadillo.
//' @export
// [[Rcpp::export]]
arma::uvec order_index(const arma::vec& vectorv) {
  return arma::sort_index(vectorv);
}  // end order_index



//' @export
// [[Rcpp::export]]
arma::mat calc_inv(const arma::mat& matrixv, const arma::uword& max_eigen) {
  arma::mat eigen_vec;
  arma::vec eigen_val;
  
  arma::eig_sym(eigen_val, eigen_vec, cov(matrixv));
  eigen_vec = eigen_vec.cols(eigen_vec.n_cols-max_eigen, eigen_vec.n_cols-1);
  eigen_val = 1/eigen_val.subvec(eigen_val.n_elem-max_eigen, eigen_val.n_elem-1);
  // arma::mat eigen_valmat = diagmat(eigen_val);
  
  return eigen_vec*diagmat(eigen_val)*eigen_vec.t();
  
}  // end calc_inv



//' @export
// [[Rcpp::export]]
Rcpp::List calc_lm(const arma::vec& response, const arma::mat& design) {
  // Add column for intercept to explanatory matrix
  arma::mat designp = join_rows(ones(design.n_rows), design);
  int nrows = design.n_rows, ncols = designp.n_cols;
  int deg_free = (nrows - ncols);
  
  // fit the model response ~ design, and calculate alpha and beta coefficients
  arma::colvec coeff = arma::solve(designp, response);
  // Calculate residuals
  arma::colvec residuals = response - designp*coeff;
  
  // Calculate TSS, RSS, and ESS
  double tot_sumsq = (nrows-1)*arma::var(response);
  double res_sumsq = arma::dot(residuals, residuals);
  double exp_sumsq = tot_sumsq - res_sumsq;
  
  // Calculate R-squared and F-statistic
  double rsquared = exp_sumsq/tot_sumsq;
  double fstat = (exp_sumsq*deg_free)/(res_sumsq*(ncols-1));
  // arma::rowvec stats=join_horiz(rsquared, fstat);
  Rcpp::NumericVector stats(2);
  stats(0) = rsquared;
  stats(1) = fstat;
  stats.attr("names") = Rcpp::CharacterVector::create("R-squared", "F-statistic");
  
  // Calculate standard errors of beta coefficients
  arma::colvec stderr = arma::sqrt(res_sumsq/deg_free*arma::diagvec(arma::pinv(arma::trans(designp)*designp)));
  // Calculate t-values and p-values of beta coefficients
  arma::colvec beta_tvals = coeff/stderr;
  arma::colvec beta_pvals = 2*Rcpp::pt(-abs(wrap(beta_tvals)), deg_free);
  NumericMatrix coeffmat = wrap(join_rows(join_rows(join_rows(coeff, stderr), beta_tvals), beta_pvals));
  Rcpp::colnames(coeffmat) = Rcpp::CharacterVector::create("coeff", "stderr", "tvals", "pvals");
  
  return Rcpp::List::create(Named("coefficients") = coeffmat,
                            // Named("residuals") = residuals,
                            Named("z_score") = residuals(nrows-1)/arma::stddev(residuals),
                            Named("stats") = stats);
}  // end calc_lm



//' @export
// [[Rcpp::export]]
double calc_alpha(arma::mat& returns, 
                  const arma::mat& indeks, 
                  const std::string& typev = "jensen",
                  const double& betav = 1.0) {
  // Initialize
  double alpha = 0;

  // Calculate alpha depending on typev
  if (typev == "jensen") {
    // Mean returns by columns
    Rcpp::NumericMatrix coeff = calc_lm(returns, indeks)["coefficients"];
    return coeff(0, 0);
  } else if (typev == "wilcoxon") {
    returns = (returns - indeks);
    // arma::uword nrows = (returns.n_rows);
    arma::uvec ranks = (arma::sort_index(arma::sort_index(returns)) + 1);
    return  dot(sign(returns), ranks);
  } else if (typev == "kruskal_wallis") {
    arma::uword nrows = (returns.n_rows);
    arma::mat combined = join_cols(returns, indeks);
    arma::uvec ranks = (arma::sort_index(arma::sort_index(combined)) + 1);
    // Apply regularized inverse to unit vector
    // weights = calc_inv(returns, max_eigen)*arma::ones(returns.n_cols);
    return  (0.0 + sum(ranks.subvec(0, nrows-1)) - sum(ranks.subvec(nrows, 2*nrows-1)));
  } else if (typev == "rank") {
    // Mean returns by columns
    // arma::vec meancols = arma::trans(arma::mean(returns, 0));
    // Standard deviation by columns
    // arma::vec sd_cols = arma::trans(arma::stddev(returns, 0));
    // sd_cols.replace(0, 1);
    // meancols = meancols/sd_cols;
    // alpha equal to ranks of Sharpe
    // weights = conv_to< vec >::from(arma::sort_index(arma::sort_index(meancols)));
    // weights = (weights - arma::mean(weights));
    return alpha;
  } else if (typev == "rankrob") {
    // Mean returns by columns
    // arma::vec meancols = arma::trans(arma::median(returns, 0));
    // meancols = ((1-alpha)*meancols + alpha*arma::mean(meancols));
    // Apply regularized inverse
    // arma::mat inverse = calc_inv(returns, max_eigen);
    // weights = calc_inv(returns, max_eigen)*meancols;
    // weights = calc_inv(returns, max_eigen)*meancols;
    // // Standard deviation by columns
    // arma::vec sd_cols = meancols;
    // for (arma::uword it=0; it < returns.n_cols; it++) {
    //   sd_cols(it) = arma::median(arma::abs((returns.col(it) - sd_cols)));
    // }  // end for
    // sd_cols.replace(0, 1);
    // meancols = meancols/sd_cols;
    // alpha equal to ranks of Sharpe
    // weights = conv_to< vec >::from(arma::sort_index(arma::sort_index(meancols)));
    // weights = conv_to< vec >::from(arma::sort_index(meancols));
    // probv;
    // weights = (weights - arma::mean(weights));
    return alpha;
  } else {
    cout << "Warning: Incorrect typev argument: " << typev << endl;
    return alpha;
  }  // end if
  
  return alpha;
  
}  // end calc_alpha


// Calculate the top and bottom quantiles of the columns of a matrix
//' @export
// [[Rcpp::export]]
arma::vec calc_quant(const arma::mat& returns, 
                     const double& probv = 0.1) {
  
  arma::vec probs = {probv, 1-probv};
  // return arma::quantile(returns, probs, 0);
  return conv_to< colvec >::from(arma::sum(arma::quantile(returns, probs, 0), 0));
  
}  // end calc_quant



// Calculate the top and bottom quantiles of a vector,
// and return a vector of zeros, ones, and minus ones, 
// with the top quantile elements equal to 1, and the 
// bottom equal to -1.
//' @export
// [[Rcpp::export]]
arma::vec calc_top_bottom(const arma::vec& returns, 
                          const double& probv = 0.1) {
  
  // arma::uword ndataem = returns.n_elem;
  arma::vec weights = zeros(returns.n_elem);
  arma::vec probs = {probv, 1-probv};
  arma::vec quantiles = arma::quantile(returns, probs);
  
  // Bottom quantile
  arma::uvec indeks = find(returns <= quantiles(0));
  weights(indeks).fill(-1);
  
  // Top quantile
  // arma::rowvec quantilev = arma::quantile(returns, probv);
  indeks = find(returns >= quantiles(1));
  // weights.elem(indeks).ones();
  weights(indeks).fill(1);
  
  return weights;
  
}  // end calc_top_bottom



// Calculate the top and bottom quantiles of the columns of a matrix.
// Add the quantiles for each column, and rank the vector of sums.
// Return a vector of zeros, ones, and minus ones, with the top 
// ranks equal to 1, and the bottom equal to -1.
//' @export
// [[Rcpp::export]]
arma::vec calc_top_bottom_columns(const arma::vec& returns, 
                      const double& probv = 0.1) {
  
  arma::uword ncols = returns.n_cols;
  arma::uword threshold = round(probv*ncols);
  arma::vec probs = {probv, 1-probv};
  arma::vec weights = zeros(ncols);

  // return arma::quantile(returns, probs, 0);
  arma::rowvec sum_quant = arma::sum(arma::quantile(returns, probs, 0), 0);
  arma::uvec ranks = (arma::sort_index(arma::sort_index(sum_quant)));
  // arma::uvec indeks = find((ranks <= threshold) || (ranks >= (ncols - threshold)));
  arma::uvec indeks = find(ranks >= (ncols - threshold));
  // weights.elem(indeks).ones();
  weights(indeks).fill(1);
  indeks = find(ranks <= threshold);
  weights(indeks).fill(-1);
  
  // indeks = find((ranks > threshold) || (ranks < (ncols - threshold)));
  // weights.elem(indeks) = 0;
  // weights.head(threshold) = 1;
  // weights.tail(threshold) = 1;
  // return conv_to< colvec >::from(weights);
  return weights;
  
}  // end calc_top_bottom_columns



//' @export
// [[Rcpp::export]]
arma::vec calc_weights(const arma::mat& returns, 
                       const std::string& typev = "quantilev",
                       int max_eigen = 1,
                       const double& probv = 0.1,
                       const double& alpha = 0.0,
                       const bool scalit = true) {
  // Initialize
  arma::vec weights(returns.n_cols);
  if (max_eigen == 1)  max_eigen = returns.n_cols;
  
  // Calculate weights depending on typev
  if (typev == "max_sharpe") {
    // Mean returns by columns
    arma::vec meancols = arma::trans(arma::mean(returns, 0));
    // Shrink meancols to the mean of returns
    meancols = ((1-alpha)*meancols + alpha*arma::mean(meancols));
    // Apply regularized inverse
    // arma::mat inverse = calc_inv(returns, max_eigen);
    weights = calc_inv(returns, max_eigen)*meancols;
  } else if (typev == "quantilev") {
    // Sum of quantiles by columns
    arma::vec probs = {probv, 1-probv};
    weights = conv_to< vec >::from(arma::sum(arma::quantile(returns, probs, 0), 0));
    // Weights equal to ranks
    weights = conv_to< vec >::from(arma::sort_index(arma::sort_index(weights)));
    weights = (weights - arma::mean(weights));
  } else if (typev == "min_var") {
    // Apply regularized inverse to unit vector
    weights = calc_inv(returns, max_eigen)*arma::ones(returns.n_cols);
  } else if (typev == "min_varpca") {
    // Calculate highest order principal component
    arma::vec eigen_val;
    arma::mat eigen_vec;
    arma::eig_sym(eigen_val, eigen_vec, cov(returns));
    weights = eigen_vec.col(0);
  } else if (typev == "rank") {
    // Mean returns by columns
    arma::vec meancols = arma::trans(arma::mean(returns, 0));
    // Standard deviation by columns
    arma::vec sd_cols = arma::trans(arma::stddev(returns, 0));
    sd_cols.replace(0, 1);
    meancols = meancols/sd_cols;
    // Weights equal to ranks of Sharpe
    weights = conv_to< vec >::from(arma::sort_index(arma::sort_index(meancols)));
    weights = (weights - arma::mean(weights));
  } else if (typev == "rankrob") {
    // Median returns by columns
    arma::vec meancols = arma::trans(arma::median(returns, 0));
    // meancols = ((1-alpha)*meancols + alpha*arma::mean(meancols));
    // Standard deviation by columns
    arma::vec sd_cols = arma::trans(arma::stddev(returns, 0));
    sd_cols.replace(0, 1);
    meancols = meancols/sd_cols;
    // Apply regularized inverse
    // arma::mat inverse = calc_inv(returns, max_eigen);
    // weights = calc_inv(returns, max_eigen)*meancols;
    // weights = calc_inv(returns, max_eigen)*meancols;
    // // Standard deviation by columns
    // arma::vec sd_cols = meancols;
    // for (arma::uword it=0; it < returns.n_cols; it++) {
    //   sd_cols(it) = arma::median(arma::abs((returns.col(it) - sd_cols)));
    // }  // end for
    // sd_cols.replace(0, 1);
    // meancols = meancols/sd_cols;
    // Weights equal to ranks of Sharpe
    weights = conv_to< vec >::from(arma::sort_index(arma::sort_index(meancols)));
    // probv;
    weights = (weights - arma::mean(weights));
  } else {
    cout << "Warning: Incorrect typev argument: " << typev << endl;
    return arma::ones(returns.n_cols);
  }  // end if
  
  if (scalit == TRUE) {
    // Returns of equally weighted portfolio
    // arma::vec meanrows = arma::mean(returns, 1);
    // Returns of weighted portfolio
    // arma::vec returns_portf = returns*weights;
    // Scale weights to equally weighted portfolio and return them
    // Return weights/sqrt(sum(square(weights)));
    // Return weights/sum(weights);
    return weights*arma::stddev(arma::mean(returns, 1))/arma::stddev(returns*weights);
  }  // end if
  
  return weights;
}  // end calc_weights


