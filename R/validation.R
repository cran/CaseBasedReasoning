#' Validate that an object is a trained ranger model with a forest
#'
#' @param rfObject Object to validate
#' @param need_forest Whether to check for a forest (default: TRUE)
#' @return Invisible NULL. Raises an error if validation fails.
#' @noRd
validate_ranger <- function(rfObject, need_forest = TRUE) {
  if (!inherits(rfObject, "ranger")) {
    stop("Expected a 'ranger' object, got '", paste(class(rfObject), collapse = "/"), "'.")
  }
  if (need_forest && is.null(rfObject$forest)) {
    stop("Ranger object does not contain a forest.")
  }
  invisible(NULL)
}
