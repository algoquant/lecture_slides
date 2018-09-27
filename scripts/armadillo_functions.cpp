// #include <Rcpp.h>
#include <RcppArmadillo.h>
#include <vector>
using namespace std;
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

////////////////////////////
// RcppArmadillo functions for manipulating vectors and matrices
////////////////////////////

// Conditional sums using Rcpp

// The function sum_na() performs sum(is.na(vec_tor)) using Rcpp.
// Adapted from:
// https://stackoverflow.com/questions/46892399/fast-checking-of-missing-values-in-rcpp
//' @export
// [[Rcpp::export]]
int sum_na(const NumericVector& vec_tor) {
  int count_ = 0;
  for (int i = 0; i < vec_tor.size(); i++)
    count_ += NumericVector::is_na(vec_tor[i]);
  return count_;
}  // end sum_na


// The function sum_na_stl() performs sum(is.na(vec_tor)) using Rcpp and STL.
//' @export
// [[Rcpp::export]]
int sum_na_stl(const NumericVector& vec_tor) {
  return std::count_if(vec_tor.begin(), vec_tor.end(), NumericVector::is_na);
}  // end sum_na_stl


// The function sum_if_cpp() performs sum(ivec_tor<fi_nd) using Rcpp.
//' @export
// [[Rcpp::export]]
int sum_if_cpp(NumericVector& vec_tor, double fi_nd) {
  int count_ = 0;
  for (int i = 0; i < vec_tor.size(); i++)
    count_ += (vec_tor[i]<fi_nd);
  return count_;
}  // end sum_if_cpp


// The function sum_if() performs sum(ivec_tor<fi_nd) using Rcpp.
//' @export
// [[Rcpp::export]]
int sum_if(NumericVector& vec_tor, double fi_nd) {
  int count_ = 0;
  NumericVector::iterator it;
  for (it = vec_tor.begin(); it != vec_tor.end(); it++) {
    if (*it < fi_nd) count_++;
  }  // end for
  return count_;
}  // end sum_if


// The function sum_if_stl() performs sum(ivec_tor<fi_nd) using Rcpp and STL.
//' @export
// [[Rcpp::export]]
int sum_if_stl(const NumericVector& vec_tor, double fi_nd) {
  return std::count_if(vec_tor.begin(), vec_tor.end(), bind2nd(std::less<double>(), fi_nd));
}  // end sum_if_stl


// The function whi_ch() performs which() using Rcpp with Sugar and RcppArmadillo
//' @export
// [[Rcpp::export]]
arma::uvec whi_ch(arma::uvec logic_al) {
  // arma::uvec logic_al;
  // logic_al = arma::find(logic_al);
  return arma::find(logic_al);
}  // end whi_ch


// The function whi_ch2() performs which() using Rcpp and arma::find() from
// RcppArmadillo
//' @export
// [[Rcpp::export]]
arma::uvec whi_ch2(LogicalVector& vec_tor) {
  arma::uvec logic_al;
  logic_al = arma::find(Rcpp::as<uvec>(vec_tor));
  return logic_al+1;
}  // end whi_ch2


// The function whi_ch3() performs which() using Rcpp and STL
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector whi_ch3(const LogicalVector& vec_tor) {
  // arma::uvec logic_al;
  // logic_al = arma::find(Rcpp::as<uvec>(vec_tor));
  int n_row = vec_tor.size();
  std::vector<int> in_dex;
  in_dex.reserve(n_row);
  for (int i=0; i<n_row; i++) {
    if (vec_tor[i]) in_dex.push_back(i+1);
  }  // end for
  return Rcpp::wrap(in_dex);
  // return logic_al;
}  // end whi_ch3


// The function whi_ch32() performs which() using Rcpp.
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector whi_ch32(const LogicalVector& vec_tor) {
  int n_row = vec_tor.size();
  IntegerVector in_dex(sum(vec_tor));
  int j = 0;
  for (int i=0; i<n_row; i++) {
    if (vec_tor[i]) 
      in_dex(j++) = i+1;
  }  // end for
  return in_dex;
}  // end whi_ch32


// The function whi_ch34() performs which() using Rcpp.
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector whi_ch34(LogicalVector& vec_tor) {
  // int n_row = vec_tor.size();
  IntegerVector in_dex(sum(vec_tor));
  int i=0, j=0;
  LogicalVector::iterator it;
  for (it = vec_tor.begin(); it != vec_tor.end(); it++, i++) {
    if (*it) 
      in_dex(j++) = i+1;
  }  // end for
  return in_dex;
}  // end whi_ch34


// The function whi_ch35() performs which() using Rcpp and STL.
// Currently it doesn't work.
// int whi_ch35(LogicalVector& vec_tor) {
//   return std::find(std::begin(vec_tor), std::end(vec_tor), TRUE);
// }  // end whi_ch35


// The function whi_ch33() performs which() using RcppArmadillo.
//' @export
// [[Rcpp::export]]
arma::uvec whi_ch33(arma::uvec& vec_tor) {
  int n_row = vec_tor.size();
  arma::uvec in_dex(accu(vec_tor));
  int j = 0;
  for (int i=0; i<n_row; i++) {
    if (vec_tor[i]) 
      in_dex(j++) = i+1;
  }  // end for
  return in_dex;
}  // end whi_ch33


// The function whi_ch4() performs which() using Rcpp
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector whi_ch4(LogicalVector& vec_tor) {
  // arma::uvec logic_al;
  // logic_al = arma::find(Rcpp::as<uvec>(vec_tor));
  int n_row = vec_tor.size();
  IntegerVector in_dex(n_row);
  int j=0;
  for (int i=0; i<n_row; i++) {
    if (vec_tor[i]) {
      in_dex(j)=i+1;
      j++;
    }  // end if
  }  // end for
  vector<int> sub_vector(in_dex.begin(), in_dex.begin() + j);
  return wrap(sub_vector);
  // return logic_al;
}  // end whi_ch4


// The function whi_ch5() calls R function which().
// It's very slow.
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector whi_ch5(LogicalVector& vec_tor) {
  // Obtain environment containing the function
  Rcpp::Environment base("package:base"); 
  // Define function which() callable from Rcpp
  Rcpp::Function which = base["which"];
  return which(vec_tor);
  // return logic_al;
}  // end whi_ch5


// The function tapply_arma() performs aggregations over a vector using a
// factor.
// It produces the same result as the R code: 
//    tapply(X=vec_tor, INDEX=fac_tor, FUN=NROW)
// It uses RcppArmadillo.
//' @export
// [[Rcpp::export]]
arma::vec tapply_arma(const arma::vec& vec_tor, const arma::vec& fac_tor) {
  Function whi_ch3("whi_ch3");
  // int n_row = vec_tor.size();
  arma::vec uniq_ue = arma::unique(fac_tor);
  int n_unique = uniq_ue.size();
  arma::vec agg_s(n_unique);
  arma::uvec in_dex;
  arma::vec sub_vec;
  for (int i=0; i<n_unique; i++) {
    in_dex = find(fac_tor == uniq_ue(i));
    sub_vec = vec_tor(in_dex);
    agg_s(i) = sub_vec.n_elem;
  }  // end for
  return agg_s;
  // return find_unique(vec_tor);
}  // end tapply_arma



// The function cbind_rcpp() binds the columns of two matrices.
// It uses Rcpp.
//' @export
// [[Rcpp::export]]
NumericMatrix cbind_rcpp(NumericMatrix mat_rix1, NumericMatrix mat_rix2) {
  int n_col1 = mat_rix1.ncol();
  int n_col2 = mat_rix2.ncol();
  NumericMatrix out_put = Rcpp::no_init_matrix(mat_rix1.nrow(), n_col1 + n_col2);
  for (int j = 0; j < n_col1 + n_col2; j++) {
    if (j < n_col1) {
      out_put(_, j) = mat_rix1(_, j);
    } else {
      out_put(_, j) = mat_rix2(_, j - n_col1);
    }
  }
  return out_put;
}  // end cbind_rcpp


// The function cbind_arma() binds the columns of two matrices.
// It uses RcppArmadillo.
//' @export
// [[Rcpp::export]]
arma::mat cbind_arma(const arma::mat& mat_rix1, const arma::mat& mat_rix2) {
  return arma::join_rows(mat_rix1, mat_rix2);
}  // end cbind_arma


// The function sub_mat() selects a sub-matrix using RcppArmadillo
//' @export
// [[Rcpp::export]]
arma::mat sub_mat(const arma::mat& mat_rix, arma::uvec row_num, arma::uvec col_num) {
  // arma::mat sub_matrix = mat_rix.cols(col_num);
  arma::mat sub_matrix = mat_rix.submat(row_num, col_num);
  return sub_matrix;
}  // end sub_mat


// The function sub_mat_cast() selects a sub-matrix using RcppArmadillo with Rcpp type arguments
// It casts Rcpp arguments into RcppArmadillo structures
//' @export
// [[Rcpp::export]]
arma::mat sub_mat_cast(NumericMatrix& mat_rix, IntegerVector& row_num, IntegerVector col_num=0) {
  // arma::mat sub_matrix = mat_rix.cols(row_num);
  // arma::mat sub_matrix = mat_rix.cols(col_num);
  arma::mat sub_matrix = as<mat>(mat_rix);
  // arma::uvec urow_num = as<uvec>(row_num);
  // arma::uvec ucol_num = as<uvec>(col_num);
  // sub_matrix = sub_matrix.submat(as<uvec>(row_num), as<uvec>(col_num));
  sub_matrix = sub_matrix.submat(as<uvec>(row_num), as<uvec>(col_num));
  return sub_matrix;
  // return Rcpp::wrap(sub_matrix);
}  // end sub_mat_cast


// The function sub_assign() assigns values to selected vector elements using
// RcppArmadillo.
// Adapted from:
// http://gallery.rcpp.org/articles/armadillo-subsetting/index.html
//' @export
// [[Rcpp::export]]
arma::rowvec sub_assign(arma::rowvec vec_tor, arma::uvec in_dex, arma::vec da_ta) {
  vec_tor.elem(in_dex) = da_ta;
  return vec_tor;
}  // end sub_assign


// The function find_assign_vec() assigns values to selected vector elements
// using RcppArmadillo.
// Adapted from:
// http://gallery.rcpp.org/articles/armadillo-subsetting/index.html
//' @export
// [[Rcpp::export]]
arma::rowvec find_assign_vec(arma::rowvec& vec_tor, double fi_nd, double da_ta) {
  arma::uvec in_dex = find(vec_tor == fi_nd);
  vec_tor(in_dex).fill(da_ta);
  return vec_tor;
}  // end find_assign_vec


// The function find_assign_vec_point() assigns values to selected vector
// elements using RcppArmadillo.
// It accepts a pointer to the vector, assigns values in place without copying
// the input vector, and returns an integer with the number of assignments
// performed. 
// Adapted from:
// http://gallery.rcpp.org/articles/armadillo-subsetting/index.html
//' @export
// [[Rcpp::export]]
double find_assign_vec_point(arma::rowvec& vec_tor, double fi_nd, double da_ta) {
  arma::uvec in_dex = find(vec_tor > fi_nd);
  vec_tor(in_dex).fill(da_ta);
  return arma::accu(in_dex);
}  // end find_assign_vec_point


// The function find_assign_mat() assigns values to selected matrix elements
// using RcppArmadillo.
// Adapted from:
// http://gallery.rcpp.org/articles/armadillo-subsetting/index.html
//' @export
// [[Rcpp::export]]
arma::mat find_assign_mat(arma::mat mat_rix, double fi_nd, double da_ta) {
  arma::uvec in_dex = find(mat_rix >= fi_nd);
  mat_rix.elem(in_dex).fill(da_ta);
  return mat_rix;
}  // end find_assign_mat


// The function compare_col() selects a matrix column and compares it 
// to an input value, using Rcpp with Sugar.
//' @export
// [[Rcpp::export]]
LogicalVector compare_col(NumericMatrix& mat_rix, double fi_nd, int col_num=0) {
  // NumericVector col_umn = mat_rix(_, col_num);
  // LogicalVector bar;
  // bar = (col_umn > fi_nd);
  return (mat_rix(_, col_num) > fi_nd);
}  // end compare_col


// The function compare_col_arma() selects a matrix column and compares 
// it to an input value, using Rcpp with Sugar and RcppArmadillo.
//' @export
// [[Rcpp::export]]
arma::uvec compare_col_arma(NumericMatrix& mat_rix, double fi_nd, int col_num=0) {
  NumericVector col_umn = mat_rix(_, col_num);
  LogicalVector bar;
  bar = (col_umn > fi_nd);
  return Rcpp::as<uvec>(bar);
}  // end compare_col_arma


// The function compare_col_armaa() selects a matrix column and compares 
// it to an input value, using Rcpp with Sugar and RcppArmadillo.
//' @export
// [[Rcpp::export]]
arma::uvec compare_col_armaa(const arma::mat& mat_rix, double fi_nd, int col_num=0) {
  arma::vec col_umn = mat_rix.cols(as<uvec>(wrap(col_num)));
  // NumericVector col_umn = mat_rix(_, col_num);
  // LogicalVector bar;
  // arma::uvec bar;
  // bar = (col_umn > fi_nd);
  // for (int i=0; i<col_umn.n_rows; i++) {
  //   bar[i] = (col_umn[i] > fi_nd);
  // }  // end for
  return (col_umn > fi_nd);
}  // end compare_col_armaa


// The function which_col() selects a matrix column and performs 
// which() on it.
// It uses Rcpp with Sugar and RcppArmadillo.
//' @export
// [[Rcpp::export]]
arma::uvec which_col(NumericMatrix& mat_rix, double fi_nd, int col_num=0) {
  NumericVector col_umn = mat_rix(_, col_num);
  LogicalVector vec_tor = (col_umn > fi_nd);
  arma::uvec whi_ch;
  whi_ch = arma::find(Rcpp::as<uvec>(vec_tor));
  return whi_ch;
}  // end which_col


// The function find_extract_mat() extracts matrix elements using RcppArmadillo.
// Adapted from:
// http://gallery.rcpp.org/articles/armadillo-subsetting/index.html
//' @export
// [[Rcpp::export]]
arma::vec find_extract_mat(arma::mat mat_rix, double fi_nd) {
  arma::uvec in_dex = find(mat_rix >= fi_nd);
  return mat_rix.elem(in_dex);
}  // end find_extract_mat


// The function find_sub_mat() selects a matrix column, performs 
// find() on it, and then selects the corresponding matrix rows.
// It uses Rcpp with Sugar and RcppArmadillo function find().
//' @export
// [[Rcpp::export]]
NumericMatrix find_sub_mat(NumericMatrix& mat_rix, double fi_nd, int col_num=0) {
  NumericVector col_umn = mat_rix(_, col_num);
  LogicalVector vec_tor = (col_umn > fi_nd);
  arma::uvec whi_ch;
  whi_ch = arma::find(Rcpp::as<uvec>(vec_tor));
  arma::mat sub_matrix = as<mat>(mat_rix);
  sub_matrix = sub_matrix.rows(whi_ch);
  return wrap(sub_matrix);
}  // end find_sub_mat


// The function select_sub_mat() selects a matrix column, performs 
// which on it, and then selects the corresponding matrix rows.
// It uses Rcpp with Sugar and RcppArmadillo, but without function find().
//' @export
// [[Rcpp::export]]
NumericMatrix select_sub_mat(NumericMatrix& mat_rix, double fi_nd, int col_num=0) {
  NumericVector col_umn = mat_rix(_, col_num);
  LogicalVector vec_tor = (col_umn > fi_nd);
  int n_row = vec_tor.size();
  // perform which
  std::vector<int> in_dex;
  in_dex.reserve(n_row);
  for (int i=0; i<n_row; i++) {
    if (vec_tor[i]) in_dex.push_back(i);
  }  // end for
  // perform which
  arma::mat sub_matrix = as<mat>(mat_rix);
  sub_matrix = sub_matrix.rows(as<uvec>(wrap(in_dex)));
  return wrap(sub_matrix);
}  // end select_sub_mat


// The function apply_agg() performs aggregations over a matrix using its
// first column as a factor.
// It produces the same result as the R code: 
//      sapply(X=unique(mat_rix[, 1]), FUN=function(mat_rix[, -1]))
// It uses RcppArmadillo.
//' @export
// [[Rcpp::export]]
arma::vec apply_agg(const arma::mat& mat_rix) {
  // Function whi_ch3("whi_ch3");
  // int n_row = vec_tor.size();
  arma::vec uniq_ue = arma::unique(mat_rix.col(0));
  int n_unique = uniq_ue.size();
  arma::vec agg_s(n_unique);
  arma::uvec in_dex;
  arma::mat sub_mat = mat_rix.cols(1, mat_rix.n_cols-1);
  for (int i=0; i<n_unique; i++) {
    in_dex = find(mat_rix.col(0) == uniq_ue(i));
    agg_s(i) = as_scalar(sum(prod(sub_mat.rows(in_dex), 1)));
    // agg_s(i) = as_scalar(accu(sub_mat.rows(in_dex)));
  }  // end for
  return agg_s;
  // return find_unique(mat_rix);
}  // end apply_agg


//' @export
// [[Rcpp::export]]
double  agg_mat(const arma::mat& mat_rix) {
  return  arma::as_scalar(sum(prod(mat_rix, 1)));
  // return find_unique(mat_rix);
}  // end agg_mat


// The function vari_ance() calculates the variance of a vector using Rcpp
//' @export
// [[Rcpp::export]]
double vari_ance(NumericVector vec_tor) {
  return sum(pow(vec_tor - sum(vec_tor)/vec_tor.size(), 2));
}  // end vari_ance



////////////////////////////
// RcppArmadillo functions for matrix algebra
////////////////////////////


// The function demean_arma() calculates a matrix with de-meaned columns.
// It accepts a pointer to a matrix and returns a copy of the de-meaned matrix.
// It uses RcppArmadillo.
//' @export
// [[Rcpp::export]]
arma::mat demean_arma(const arma::mat& mat_rix) {
  // de-mean response and explanatory variables
  arma::mat mat_demean(mat_rix.n_rows, mat_rix.n_cols);
  for (unsigned int i = 0; i < mat_rix.n_cols; i++) {
    mat_demean.col(i) = mat_rix.col(i) - arma::mean(mat_rix.col(i));
    // mat_demean(i) = arma::mean(mat_rix.col(i));
  }  // end for
  return mat_demean;
}  // end demean_arma


// The function demean_mat() calculates a matrix with de-meaned columns.
// It accepts a pointer to a matrix and operates on the matrix in place.
// It returns the number of columns of the input matrix.
// It uses RcppArmadillo.
//' @export
// [[Rcpp::export]]
double demean_mat(arma::mat& mat_rix) {
  // de-mean response and explanatory variables
  // arma::mat mat_demean(mat_rix.n_cols);
  for (unsigned int i = 0; i < mat_rix.n_cols; i++) {
    mat_rix.col(i) -= arma::mean(mat_rix.col(i));
    // mat_demean(i) = arma::mean(mat_rix.col(i));
  }  // end for
  return mat_rix.n_cols;
}  // end demean_mat


// The function inner_arma() calculates the inner (dot) product of two vectors.
// It accepts pointers to the two vectors and returns a double.
// It uses RcppArmadillo.
//' @export
// [[Rcpp::export]]
double vec_in(const arma::vec& vec1, const arma::vec& vec2) {
  return arma::dot(vec1, vec2);
}  // end vec_in


// The function mat_vec_in() calculates the product of a matrix times a vector.
// It accepts pointers to the matrix and vector, and returns a vector.
// It uses RcppArmadillo.
//' @export
// [[Rcpp::export]]
arma::vec mat_vec_in(const arma::vec& vec_tor, const arma::mat& mat_rix) {
  return mat_rix * vec_tor;
}  // end mat_vec_in


// The function mat_2vec_in() calculates the inner (dot) product of a matrix
// with two vectors.
// It accepts pointers to the matrix and vectors, and returns a double.
// It uses RcppArmadillo.
//' @export
// [[Rcpp::export]]
double mat_2vec_in(const arma::vec& vec_tor2, const arma::mat& mat_rix, const arma::vec& vec_tor1) {
  return arma::as_scalar(trans(vec_tor2) * (mat_rix * vec_tor1));
}  // end mat_2vec_in


// The function mat_vec_by() calculates the product of a matrix times a vector.
// It accepts pointers to the matrix and vector, and returns a vector.
// It accepts a pointer to a matrix and operates on the matrix in place.
// It returns the number of columns of the input matrix.
// It uses RcppArmadillo.
//' @export
// [[Rcpp::export]]
double mat_vec_by(arma::vec& vec_tor, arma::mat& mat_rix) {
  // unsigned int mn_rows = mat_rix.n_rows;
  unsigned int mn_cols = mat_rix.n_cols;
  arma::mat vec_mat = repmat(vec_tor, 1, mn_cols);
  mat_rix = mat_rix % vec_mat;
  return mat_rix.n_cols;
}  // end mat_vec_by


// The function mat_2vec_by() calculates the product of a matrix times
// two vectors.
// It multiplies the rows of the matrix by vec_tor1, and the columns by
// vec_tor2.
// It produces the same result as the R code: t(t(vec_tor2*mat_rix)*vec_tor1)
// It accepts pointers to the matrix and vectors, assigns the matrix values in
// place without copying the input vector, and returns a double.
// It uses RcppArmadillo.
// Adapted from:
// https://stackoverflow.com/questions/24933290/elementwise-matrix-multiplication-r-versus-rcpp-how-to-speed-this-code-up
//' @export
// [[Rcpp::export]]
double mat_2vec_by(arma::vec& vec_tor2, arma::mat& mat_rix, arma::vec& vec_tor1) {
  unsigned int mn_rows = mat_rix.n_rows;
  unsigned int mn_cols = mat_rix.n_cols;
  arma::mat vec_mat = repmat(vec_tor2, 1, mn_cols);
  mat_rix = mat_rix % vec_mat;
  vec_mat = repmat(trans(vec_tor1), mn_rows, 1);
  mat_rix = mat_rix % vec_mat;
  return mat_rix.n_cols;
}  // end mat_2vec_by


// The function mat_2vec_rcpp_by() calculates the product of a matrix times
// two vectors.
// It multiplies the rows of the matrix by vec_tor1, and the columns by
// vec_tor2.
// It produces the same result as the R code: t(t(vec_tor2*mat_rix)*vec_tor1)
// It accepts pointers to the matrix and vectors, assigns the matrix values in
// place without copying the input vector, and returns a double.
// It uses Rcpp.
//' @export
// [[Rcpp::export]]
int mat_2vec_rcpp_by(NumericVector& vec_tor2, NumericMatrix& mat_rix, NumericVector& vec_tor1) {
  unsigned int mn_rows = mat_rix.nrow();
  unsigned int mn_cols = mat_rix.ncol();
  if (!(mn_rows == vec_tor2.size())) stop("vec_tor2 length not equal to number of rows of mat_rix");
  if (!(mn_cols == vec_tor1.size())) stop("vec_tor1 length not equal to number of columns of mat_rix");
  for (unsigned int i = 0; i < mn_rows; i++) {
    mat_rix(i, _) = vec_tor1 * mat_rix(i, _);
  }  // end for
  for (unsigned int i = 0; i < mn_cols; i++) {
    mat_rix(_, i) = vec_tor2 * mat_rix(_, i);
  }  // end for
  return mn_cols;
}  // end mat_2vec_rcpp_by


// The function mat_2vec_rcpp_by2() is similar to mat_2vec_rcpp_by(), 
// but uses a simple loop without Rcpp, and is much slower.
//' @export
// [[Rcpp::export]]
int mat_2vec_rcpp_by2(NumericVector& vec_tor2, NumericMatrix& mat_rix, NumericVector& vec_tor1) {
  for (int i = 0; i < mat_rix.nrow(); i++) {
    for (int j = 0; j < mat_rix.ncol(); j++) {
      mat_rix(i, j) = vec_tor1(j) * vec_tor2(i) * mat_rix(i, j);
    }  // end for
  }  // end for
  return mat_rix.ncol();
}  // end mat_2vec_rcpp_by2


// The function get_cor() calculates the correlation of the matrix re_turns.
//' @export
// [[Rcpp::export]]
arma::mat get_cor(const arma::mat& re_turns) {
  return arma::cor(re_turns);
}  // end get_cor


// homework
// The function get_eigenvals() calculates the eigen_values 
// of the matrix cov_mat.
//' @export
// [[Rcpp::export]]
arma::vec get_eigenvals(const arma::mat& cov_mat) {
  arma::vec eigen_vals = arma::eig_sym(cov_mat);
  return eigen_vals;
}  // end get_eigenvals


// The function get_eigen() calculates the eigen decomposition 
// of the matrix re_turns.
//' @export
// [[Rcpp::export]]
List get_eigen(const arma::mat& re_turns) {
  arma::mat eigen_vec;
  arma::vec eigen_val;
  arma::eig_sym(eigen_val, eigen_vec, cor(re_turns));
  return List::create(Named("eigval") = eigen_val,
                      Named("eigvec") = eigen_vec);
}  // end get_eigen


// homework
// The function get_pca() calculates the PCA 
// of the matrix re_turns.
//' @export
// [[Rcpp::export]]
List get_pca(const arma::mat& re_turns) {
  arma::mat co_eff;
  arma::mat sco_re;
  arma::vec la_tent;
  arma::vec t_squared;
  
  arma::princomp(co_eff, sco_re, la_tent, t_squared, re_turns);
  
  return List::create(Named("coefficients") = co_eff,
                      Named("score") = sco_re,
                      Named("latent") = la_tent,
                      Named("tsquared") = t_squared);
  
}  // end get_pca


// The function invspd_rcpp() calculates the inverse of symmetric positive
// definite matrix.
// It uses Rcpp and RcppArmadillo.
//' @export
// [[Rcpp::export]]
SEXP invspd_rcpp(SEXP X_) {
  arma::mat X    = Rcpp::as<arma::mat>(X_);
  arma::mat Xinv = arma::inv_sympd(X);
  return(Rcpp::wrap(Xinv));
}  // end invspd_rcpp


// The function invspd_arma() calculates the inverse of symmetric positive 
// definite matrix.
// It uses RcppArmadillo.
//' @export
// [[Rcpp::export]]
arma::mat invspd_arma(const arma::mat& mat_rix) {
  arma::mat mat_inv = arma::inv_sympd(mat_rix);
  return mat_inv;
}  // end invspd_arma


// The function inv_mat() calculates the inverse of symmetric positive
// definite matrix.
// It accepts a pointer to a matrix and operates on the matrix in place.
// It returns the number of columns of the input matrix.
// It uses RcppArmadillo.
//' @export
// [[Rcpp::export]]
double inv_mat(arma::mat& mat_rix) {
  mat_rix = arma::inv_sympd(mat_rix);
  return mat_rix.n_cols;
}  // end inv_mat


// The function lm_arma() performs multivariate linear regression, and 
// calculates the alpha and beta coefficients and their t-values and p-values, 
// and the R-squared and F-statistic.
// It uses RcppArmadillo.
// Adapted from:
// http://gallery.rcpp.org/articles/fast-linear-model-with-armadillo/
//' @export
// [[Rcpp::export]]
Rcpp::List lm_arma(const arma::colvec& res_ponse, const arma::mat& de_sign) {
  // add column for intercept to explanatory matrix
  arma::mat design_p = join_rows(ones(de_sign.n_rows), de_sign);
  int num_rows = de_sign.n_rows, num_cols = design_p.n_cols;
  int deg_free = (num_rows - num_cols);
  
  // fit the model res_ponse ~ de_sign, and calculate alpha and beta coefficients
  arma::colvec co_eff = arma::solve(design_p, res_ponse);
  // calculate residuals
  arma::colvec resid_uals = res_ponse - design_p*co_eff;
  
  // calculate TSS, RSS, and ESS
  double tot_sumsq = (num_rows-1)*arma::var(res_ponse);
  double res_sumsq = arma::dot(resid_uals, resid_uals);
  double exp_sumsq = tot_sumsq - res_sumsq;
  
  // calculate R-squared and F-statistic
  double r_squared = exp_sumsq/tot_sumsq;
  double f_stat = (exp_sumsq*deg_free)/(res_sumsq*(num_cols-1));
  // arma::rowvec stat_s=join_horiz(r_squared, f_stat);
  Rcpp::NumericVector stat_s(2);
  stat_s(0) = r_squared;
  stat_s(1) = f_stat;
  stat_s.attr("names") = Rcpp::CharacterVector::create("R-squared", "F-statistic");
  
  // calculate standard errors of beta coefficients
  arma::colvec std_err = arma::sqrt(res_sumsq/deg_free*arma::diagvec(arma::pinv(arma::trans(design_p)*design_p)));
  // calculate t-values and p-values of beta coefficients
  arma::colvec beta_tvals = co_eff/std_err;
  arma::colvec beta_pvals = 2*Rcpp::pt(-abs(wrap(beta_tvals)), deg_free);
  NumericMatrix coeff_mat = wrap(join_rows(join_rows(join_rows(co_eff, std_err), beta_tvals), beta_pvals));
  Rcpp::colnames(coeff_mat) = Rcpp::CharacterVector::create("coeff", "std_err", "tvals", "pvals");
  
  return Rcpp::List::create(Named("coefficients") = coeff_mat,
                            // Named("residuals") = resid_uals,
                            Named("z_score") = resid_uals(num_rows-1)/arma::stddev(resid_uals),
                            Named("stats") = stat_s);
}  // end lm_arma



////////////////////////////
// RcppArmadillo test functions
////////////////////////////


// The function test_rcpp() is for testing some Rcpp code snippets.
//' @export
// [[Rcpp::export]]
LogicalVector test_rcpp(NumericVector& vec_tor2, NumericMatrix& mat_rix, NumericVector& vec_tor1) {
  Rcpp::IntegerVector stat_s(4);
  stat_s(0) = mat_rix.nrow();
  stat_s(1) = mat_rix.ncol();
  stat_s(2) = vec_tor1.size();
  stat_s(3) = vec_tor2.size();
  stat_s.attr("names") = Rcpp::CharacterVector::create("n_rows", "n_cols", "v_siz1", "v_siz2");
  // unsigned int mn_rows = mat_rix.nrow();
  unsigned int mn_cols = mat_rix.ncol();
  // unsigned int v_siz1 = vec_tor1.size();
  // unsigned int v_siz2 = vec_tor2.size();
  // return stat_s;
  return !(mn_cols == vec_tor2.size());
}  // end test_rcpp


// The function test_arma() is for testing some RcppArmadillo code snippets.
//' @export
// [[Rcpp::export]]
LogicalVector test_arma(arma::mat& mat_rix) {
  // Rcout << "Num rows: " << mat_rix.n_rows << std::endl;
  // Rcout << "Num cols: " << mat_rix.n_cols << std::endl;
  mat_rix.print("This is the input matrix:");
  mat_rix(0, 1) = 111;
  mat_rix.print("This is the output matrix:");
  return !(mat_rix.n_cols == mat_rix.n_rows);
}  // end test_arma

//' @export
// [[Rcpp::export]]
arma::uvec test_more_arma(const LogicalVector& vec_tor) {
  Function whi_ch3("whi_ch3");
  // Rcout << "Num rows: " << mat_rix.n_rows << std::endl;
  // Rcout << "Num cols: " << mat_rix.n_cols << std::endl;
  // mat_rix.print("This is the input matrix:");
  // mat_rix(0, 1) = 111;
  // mat_rix.print("This is the output matrix:");
  arma::uvec in_dex = Rcpp::as<uvec>(whi_ch3(vec_tor));
  return in_dex;
}  // end test_more_arma

