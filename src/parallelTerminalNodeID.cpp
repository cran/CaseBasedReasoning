// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppParallel.h>
#include <RcppArmadillo.h>

// get the terminal for each observation
struct ParallelTerminalNodes : public RcppParallel::Worker {
  const arma::mat input_;
  const arma::vec childNodes1_;
  const arma::vec childNodes2_;
  const arma::vec splitValues_;
  const arma::vec splitVarIds_;
  arma::vec& output_;
  
  ParallelTerminalNodes(
    const arma::mat& input,
    const arma::vec childNodes1,
    const arma::vec childNodes2,
    const arma::vec splitValues,
    const arma::vec splitVarIds,
    arma::vec& output
  ) : input_(input), childNodes1_(childNodes1), childNodes2_(childNodes2), 
  splitValues_(splitValues), splitVarIds_(splitVarIds), output_(output)  {}
  void operator() (std::size_t begin, std::size_t end) {
    for (std::size_t i=begin; i<end; ++i) {
      int nodeId = 1;
      double value = 0;
      while (true) {
        if ((childNodes1_(nodeId - 1) == 0 && childNodes2_(nodeId - 1) == 0)) {
          break;
        }
        int splitVarID = splitVarIds_(nodeId - 1);
        value = input_(i, splitVarID - 1);
        if (value <= splitValues_(nodeId - 1)) {
          nodeId = childNodes1_(nodeId - 1) + 1;
        } else {
          nodeId = childNodes2_(nodeId - 1) + 1;
        }
      }
      output_(i) = nodeId - 1;
    }
  }
};

// [[Rcpp::export]]
arma::vec cpp_terminalNodeID(arma::mat& x,
                             arma::vec& childNodes1, 
                             arma::vec& childNodes2, 
                             arma::vec& splitValues, 
                             arma::vec& splitVarIds) {
  int nrow = x.n_rows;
  arma::vec output(nrow);
  output.fill(0);
  ParallelTerminalNodes parallelTerminalNodes(x, childNodes1, childNodes2, splitValues, splitVarIds, output);
  parallelFor(0, nrow, parallelTerminalNodes);
  return output;
}
