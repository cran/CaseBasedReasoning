#' Get the terminal node id of a RandomForest Object
#'
#' Extracts for each observation and for each tree in the forest the terminal
#' node id. The index of terminal nodes are starting with 1, e.g., the root node has id 1
#'
#' @param x a data.frame
#' @param rfObject \code{ranger} object
#'
#' @return Matrix with terminal node IDs for all observations in x (rows) and
#'         trees (columns)
#'
#' @examples
#' library(ranger)
#' rf.fit <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
#' dfNodes <- terminal_nodes(iris[, -5], rf.fit)
#'
#' @export
terminal_nodes <- function(x, rfObject) {
  validate_ranger(rfObject)
  res <- predict(rfObject, x, type = "terminalNodes")
  res$predictions
}


#' @title Forest2Matrix
#'
#' @description Transform trees of a \code{ranger}-object to a matrix
#'
#' @param rfObject \code{ranger} object
#'
#' @return a \code{matrix} object with
#' Column 1: tree ID
#' Column 2: node ID
#' Column 3: child node ID 1
#' Column 4: child node ID 2
#'
#' @examples
#' \donttest{
#' library(ranger)
#' rf.fit <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
#' forest_matrix <- ranger_forests_to_matrix(rf.fit)
#' }
#'
#' @export
ranger_forests_to_matrix <- function(rfObject) {
  res <- sapply(1:rfObject$num.trees, function(t) {
    len <- length(rfObject$forest$child.nodeIDs[[t]][[1]])
    data.frame(t   = rep(t, len),
               n   = seq_len(len),
               id1 = rfObject$forest$child.nodeIDs[[t]][[1]],
               id2 = rfObject$forest$child.nodeIDs[[t]][[2]],
               split_id = rfObject$forest$split.varIDs[[t]])
  }, simplify = FALSE)
  res <- do.call(rbind, res)
  as.matrix(res)
}


#' Converts a distance vector into an object of class \code{dist}
#'
#' @param x data vector
#' @param n length of x
#' @param method method description
#'
#' @export
as_dist_object <- function(x, n, method) {
  structure(.Data  = x,
            Size   = n,
            Labels = 1:n,
            Diag   = FALSE,
            Upper  = FALSE,
            method = method,
            class  = "dist")
}
