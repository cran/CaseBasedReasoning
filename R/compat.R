# Deprecated function wrappers ------------------------------------------------
# Old names are kept for backward compatibility but emit deprecation warnings.

#' @rdname distance_random_forest
#' @export
distanceRandomForest <- function(x, y = NULL, rfObject, method = "Proximity", threads = NULL) {
  .Deprecated("distance_random_forest")
  distance_random_forest(x = x, y = y, rfObject = rfObject, method = method, threads = threads)
}

#' @rdname weighted_distance
#' @export
weightedDistance <- function(x, y = NULL, weights = NULL) {
  .Deprecated("weighted_distance")
  weighted_distance(x = x, y = y, weights = weights)
}

#' @rdname terminal_nodes
#' @export
terminalNodes <- function(x, rfObject) {
  .Deprecated("terminal_nodes")
  terminal_nodes(x = x, rfObject = rfObject)
}

#' @rdname as_dist_object
#' @export
asDistObject <- function(x, n, method) {
  .Deprecated("as_dist_object")
  as_dist_object(x = x, n = n, method = method)
}
