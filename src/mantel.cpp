
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat ape_perm(arma::mat& m) {
  // cout << "m nrows = " << m.n_rows << std::endl;
  arma::uvec rand_u = arma::randperm(m.n_rows);
  // cout << "rand_u is" << std::endl << rand_u << std::endl;
  return m.submat(rand_u, rand_u);
}

// [[Rcpp::export]]
double ape_zstat(arma::mat& m1, arma::mat& m2) {
  // m1.diag() = zeros<uvec>(m1.n_rows);
  // m2.diag() = zeros<uvec>(m2.n_rows);
  return accu(m1 % m2)/2.0;
  // accu(A % B) is a "multiply-and-accumulate" operation
  // as operator % performs element-wise multiplication
}
