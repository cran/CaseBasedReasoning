#' Distance calculation based on RandomForest Proximity or Depth
#' 
#' @param x a data.frame
#' @param y a second data.frame
#' @param rfObject \code{ranger} object
#' @param method distance calculation method
#' @param threads number of threads to use
#' 
#' @return a \code{dist} or a matrix object with pairwise distance of 
#' observations in x vs y (if not null)
#' 
#' @examples
#' library(ranger)
#' # proximity pairwise distances
#' rf.fit <- ranger(Species ~ ., data = iris, num.trees = 500, write.forest = TRUE)
#' distanceRandomForest(x = iris[, -5], rfObject = rf.fit, method = "Proximity", threads = 1)
#' 
#' # depth distance for train versus test subset
#' set.seed(1234L)
#' learn <- sample(1:150, 100)
#' test <- (1:150)[-learn]
#' rf.fit <- ranger(Species ~ ., data = iris[learn, ], num.trees = 500, write.forest = TRUE)
#' distanceRandomForest(x = iris[learn, -5], y = iris[test, -5], rfObject = rf.fit, method = "Depth")
#' 
#' @export
distanceRandomForest <- function(x, y = NULL, rfObject, method = "Proximity", threads = NULL) {
  method <- match.arg(method, c("Proximity", "Depth"))
  testthat::expect_is(rfObject, "ranger")
  testthat::expect_false(object = is.null(rfObject$forest), 
                         info   = "Ranger object does not contain a forest.")
  
  # set number of threads
  if (!is.null(threads)) {
    RcppParallel::setThreadOptions(numThreads = threads)
  }
  
  # Distance calculation
  if (method == "Proximity") {
    proximityMatrix(x = x, y = y, rfObject = rfObject)
  } else if (method == "Depth") {
    depthMatrix(x = x, y = y, rfObject = rfObject)
  }
}



#' Get proximity matrix of an ranger object
#'
#' @param x a new dataset
#' @param y a second new dataset (Default: NULL)
#' @param rfObject \code{ranger} object
#' 
#' @return a \code{dist} or a matrix object with pairwise proximity of 
#' observations in x vs y (if not null)
#'      
#' @examples
#' require(ranger)
#' rf <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
#' d <- proximityMatrix(x = iris[, -5], rfObject = rf)
#' 
#' set.seed(1234L)
#' learn <- sample(1:150, 100)
#' test <- (1:150)[-learn]
#' rf <- ranger(Species ~ ., data = iris[learn, ], num.trees = 500, write.forest = TRUE)
#' d <- proximityMatrix(x = iris[learn, -5], y = iris[test, -5], rfObject = rf)
#' 
#' @export
proximityMatrix <- function(x, y = NULL, rfObject) {
  x %>% 
    as.matrix() %>% 
    terminalNodeIDs(rfObject) -> xNodes
  if (is.null(y)) {
    d <- cpp_proximityMatrix(xNodes)
    n <- nrow(x)
    # convert to dist object
    asDistObject(d, n, "RFProximity")
  } else {
    y %>% 
      as.matrix() %>% 
      terminalNodeIDs(rfObject) -> yNodes
    cpp_proximityMatrixRangerXY(xNodes, yNodes)
  }
}


#' Get depth distance matrix
#' 
#' @param x a new dataset
#' @param y a new dataset
#' @param rfObject \code{ranger} object
#' 
#' @examples
#' require(ranger)
#' rf <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
#' d <- depthMatrix(x=iris[, -5], rfObject=rf)
#' 
#' @export
depthMatrix <- function(x, y=NULL, rfObject) {
  x %>% 
    as.matrix() %>% 
    terminalNodeIDs(rfObject) -> xNodes
  rfObject %>% 
    forestToMatrix() -> rfTrees
  if (is.null(y)) {
    d <- cpp_depthMatrix(xNodes, rfTrees) 
    n <- nrow(x)
    # convert to dist object
    asDistObject(d, n, "RFDepth")
  } else {
    y %>% 
      as.matrix() %>% 
      terminalNodeIDs(rfObject) -> yNodes
    cpp_depthMatrixRangerXY(xNodes, yNodes, rfTrees)
  }
}


#' @title Calculate terminal node distance for each tree and terminal
#' 
#' @description first two columns are terminal node IDs; If an ID pair do not 
#' appear in a tree -1 is inserted
#'   
#' @param rfObject \code{ranger} object
#' 
#' @return a \code{matrix} object with pairwise terminal node edge length
#'    
#' @examples
#' require(ranger)
#' rf.fit <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
#' d <- distanceTerminalNodes(rf.fit)
#' 
#' @export
distanceTerminalNodes <- function(rfObject) {
  testthat::expect_is(rfObject, "ranger")
  testthat::expect_false(object = is.null(rfObject$forest), 
                         info   = "Ranger object does not contain a forest.")
  rfObject %>% 
    forestToMatrix() %>% 
    cpp_TerminalNodeDistance()
}


#' Weighted Distance calculation
#' 
#' @param x a new dataset
#' @param y a second new dataset
#' @param weights a vector of weights
#' 
#' @return a \code{dist} or \code{matrix} object
#' 
#' @examples
#' require(ranger)
#' rf <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
#' nodeID <- terminalNodeIDs(iris[, -5], rf)
#' 
#' @export
weightedDistance <- function(x, y=NULL, weights=NULL) {
  if (is.null(weights)) {
    weights <- seq(1, ncol(x))
  }
  if (is.null(y)) {
    d <- cpp_weightedDistance(x, weights) 
    return(asDistObject(d, nrow(x), "weightedDistance"))
  } else {
    return(cpp_weightedDistanceXY(x, y, weights))
  }
}
