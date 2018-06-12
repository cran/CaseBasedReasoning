#include "distanceAPI.h"

void distanceAPI::init(arma::mat& x) {
  this->calc(x);
};


void distanceAPI::calc(arma::mat& x) {
  int nrow = x.n_rows;
  output_ = arma::vec(nrow * (nrow - 1) / 2);
  parallelDistance parallelDistance(x, dist_, nrow, output_);
  parallelFor(0, nrow, parallelDistance);
};


/**
 * Weighted Distance Calculation
 */
void weightedDistanceAPI::init(arma::mat& x, arma::rowvec& weights) {
  this->set_distance(weights);
  this->calc(x);
}

void weightedDistanceAPI::set_distance(arma::rowvec& weights) {
  weightedDistance dist;
  dist.set_parameters(weights);
  dist_ = std::make_shared<weightedDistance>(dist);
};


/**
 * XY Distance Calculation
 */
void xyDistanceAPI::init(arma::mat& x, arma::mat& y) {
  this->calc(x, y);
};

void xyDistanceAPI::calc(arma::mat& x, arma::mat& y) {
  int nrow = x.n_rows;
  arma::mat output(nrow, y.n_rows);
  output_ = output;
  parallelDistanceNM parallelDistanceNM(x, y, dist_, nrow, output_);
  parallelFor(0, nrow, parallelDistanceNM);
};


/**
 * Weighted XY Distance Calculation
 */
void weightedXYDistanceAPI::init(arma::mat& x, arma::mat& y, arma::rowvec& weights) {
  this->set_distance(weights);
  this->calc(x, y);
}

void weightedXYDistanceAPI::set_distance(arma::rowvec& weights) {
  weightedDistance dist;
  dist.set_parameters(weights);
  dist_ = std::make_shared<weightedDistance>(dist);
};

void weightedXYDistanceAPI::calc(arma::mat& x, arma::mat& y) {
  int nrow = x.n_rows;
  arma::mat output(nrow, y.n_rows);
  output_= output;
  parallelDistanceNM parallelDistanceNM(x, y, dist_, nrow, output_);
  parallelFor(0, nrow, parallelDistanceNM);
};

/**
 * RandomForests Terminal Node Distance
 */
void rfTerminalNodeDistanceAPI::init(arma::umat& nodeIDs) {
  rangerForest rf(nodeIDs);
  output_ = rf.nodeDistance();
}


/**
 * RandomForests Proximity Matrix
 */
void rfProximityDistanceAPI::init(arma::mat& x) {
  this->set_distance(x);
  this->calc(x);
}

void rfProximityDistanceAPI::set_distance(arma::mat& x) {
  rangerProximity dist;
  dist.set_parameters(x.n_cols);
  dist_ = std::make_shared<rangerProximity>(dist);
}


/**
 * RandomForests XY Proximity Matrix
 */
void rfProximityXYDistanceAPI::init(arma::mat& x, arma::mat& y) {
  this->set_distance(x);
  this->calc(x, y);
};

void rfProximityXYDistanceAPI::calc(arma::mat& x, arma::mat& y) {
  int nrow = x.n_rows;
  arma::mat output(nrow, y.n_rows);
  output_ = output;
  parallelDistanceNM parallelDistanceNM(x, y, dist_, nrow, output_);
  parallelFor(0, nrow, parallelDistanceNM);
};


/**
 * RandomForests Depth Distance
 */
void rfDepthDistanceAPI::init(arma::mat& xNodeIDs, arma::umat& terminalNodeIDs) {
  // calculate terminal node edge length
  rangerForest rf(terminalNodeIDs);
  RfDistContainer nodeDists = rf.nodeDistance();
  this->set_distance(nodeDists);
  this->calc(xNodeIDs);
}

void rfDepthDistanceAPI::set_distance(RfDistContainer& nodeDists) {
  rfDepthDistance dist;
  dist.set_parameters(nodeDists);
  dist_ = std::make_shared<rfDepthDistance>(dist);
}

void rfDepthDistanceAPI::calc(arma::mat& xNodeIDs) {
  int nrow = xNodeIDs.n_rows;
  arma::vec output(nrow * (nrow - 1) / 2);
  output_ = output;
  parallelDistance parallelDistance(xNodeIDs, dist_, nrow, output_);
  parallelFor(0, nrow, parallelDistance);
};


/**
 * RandomForests XY Depth Distance
 */
void rfDepthXYDistanceAPI::init(arma::mat& xNodeIDs, arma::mat& yNodeIDs, arma::umat& terminalNodeIDs) {
  // calculate terminal node edge length
  rangerForest rf(terminalNodeIDs);
  RfDistContainer nodeDists = rf.nodeDistance();
  this->set_distance(nodeDists);
  this->calc(xNodeIDs, yNodeIDs);
}

void rfDepthXYDistanceAPI::calc(arma::mat& xNodeIDs, arma::mat& yNodeIDs) {
  int nrow = xNodeIDs.n_rows;
  arma::mat output(nrow, yNodeIDs.n_rows);
  output_ = output;
  parallelDistanceNM parallelDistanceNM(xNodeIDs, yNodeIDs, dist_, nrow, output_);
  parallelFor(0, nrow, parallelDistanceNM);
};
