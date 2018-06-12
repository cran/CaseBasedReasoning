// [[Rcpp::depends(RcppArmadillo)]]
#include<RcppArmadillo.h>

#include "distanceAPI.h"

// [[Rcpp::export]]
arma::vec cpp_weightedDistance(arma::mat& x, arma::rowvec& weights) {
  weightedDistanceAPI dist;
  dist.init(x, weights);
  return dist.get();
}

// [[Rcpp::export]]
arma::mat cpp_weightedDistanceXY(arma::mat& x, arma::mat& y, arma::rowvec& weights) {
  weightedXYDistanceAPI dist;
  dist.init(x, y, weights);
  return dist.get();
}

/**
 * Ranger RandomForest related distances
 */
// [[Rcpp::export]]
Rcpp::DataFrame cpp_TerminalNodeDistance(arma::umat& terminalNodeIDs) {
  rfTerminalNodeDistanceAPI dist;
  dist.init(terminalNodeIDs);
  return dist.get();
}

// [[Rcpp::export]]
arma::vec cpp_proximityMatrix(arma::mat& nodeIDs) {
  rfProximityDistanceAPI dist;
  dist.init(nodeIDs);
  return dist.get();
}

// [[Rcpp::export]]
arma::mat cpp_proximityMatrixRangerXY(arma::mat& xNodeIDs, arma::mat& yNodeIDs) {
  rfProximityXYDistanceAPI dist;
  dist.init(xNodeIDs, yNodeIDs);
  return dist.get();
}

// [[Rcpp::export]]
arma::vec cpp_depthMatrix(arma::mat& xNodeIDs, arma::umat& terminalNodeIDs) {
  rfDepthDistanceAPI dist;
  dist.init(xNodeIDs, terminalNodeIDs);
  return dist.get();
}

// [[Rcpp::export]]
arma::mat cpp_depthMatrixRangerXY(arma::mat& xNodeIDs, arma::mat& yNodeIDs, arma::umat& terminalNodeIDs) {
  rfDepthXYDistanceAPI dist;
  dist.init(xNodeIDs, yNodeIDs, terminalNodeIDs);
  return dist.get();
}
