# S3 methods for R6 model classes ----------------------------------------------
# These provide the standard R interface: predict(model, newdata, k = 5)

#' Predict method for CoxModel
#'
#' @param object A \code{CoxModel} R6 object
#' @param newdata Query data of class data.frame
#' @param k Number of similar cases to return
#' @param ... Additional arguments (currently unused)
#' @return A data.frame of similar cases
#' @export
predict.CoxModel <- function(object, newdata, k = 1, ...) {
  object$get_similar_cases(query = newdata, k = k, ...)
}

#' Predict method for LinearModel
#'
#' @param object A \code{LinearModel} R6 object
#' @param newdata Query data of class data.frame
#' @param k Number of similar cases to return
#' @param ... Additional arguments (currently unused)
#' @return A data.frame of similar cases
#' @export
predict.LinearModel <- function(object, newdata, k = 1, ...) {
  object$get_similar_cases(query = newdata, k = k, ...)
}

#' Predict method for LogisticModel
#'
#' @param object A \code{LogisticModel} R6 object
#' @param newdata Query data of class data.frame
#' @param k Number of similar cases to return
#' @param ... Additional arguments (currently unused)
#' @return A data.frame of similar cases
#' @export
predict.LogisticModel <- function(object, newdata, k = 1, ...) {
  object$get_similar_cases(query = newdata, k = k, ...)
}

#' Predict method for RFModel
#'
#' @param object An \code{RFModel} R6 object
#' @param newdata Query data of class data.frame
#' @param k Number of similar cases to return
#' @param ... Additional arguments (currently unused)
#' @return A data.frame of similar cases
#' @export
predict.RFModel <- function(object, newdata, k = 1, ...) {
  object$get_similar_cases(query = newdata, k = k, ...)
}

#' Print method for CoxModel
#' @param x A \code{CoxModel} R6 object
#' @param ... Additional arguments (currently unused)
#' @export
print.CoxModel <- function(x, ...) x$print()

#' Print method for LinearModel
#' @param x A \code{LinearModel} R6 object
#' @param ... Additional arguments (currently unused)
#' @export
print.LinearModel <- function(x, ...) x$print()

#' Print method for LogisticModel
#' @param x A \code{LogisticModel} R6 object
#' @param ... Additional arguments (currently unused)
#' @export
print.LogisticModel <- function(x, ...) x$print()

#' Print method for RFModel
#' @param x An \code{RFModel} R6 object
#' @param ... Additional arguments (currently unused)
#' @export
print.RFModel <- function(x, ...) x$print()

#' Summary method for CoxModel
#' @param object A \code{CoxModel} R6 object
#' @param ... Additional arguments (currently unused)
#' @export
summary.CoxModel <- function(object, ...) {
  private_summary_regression(object, "CoxModel (cph)")
}

#' Summary method for LinearModel
#' @param object A \code{LinearModel} R6 object
#' @param ... Additional arguments (currently unused)
#' @export
summary.LinearModel <- function(object, ...) {
  private_summary_regression(object, "LinearModel (ols)")
}

#' Summary method for LogisticModel
#' @param object A \code{LogisticModel} R6 object
#' @param ... Additional arguments (currently unused)
#' @export
summary.LogisticModel <- function(object, ...) {
  private_summary_regression(object, "LogisticModel (lrm)")
}

#' Summary method for RFModel
#' @param object An \code{RFModel} R6 object
#' @param ... Additional arguments (currently unused)
#' @export
summary.RFModel <- function(object, ...) {
  cat("CaseBasedReasoning Model Summary\n")
  cat("================================\n")
  cat("Type          : RFModel (ranger)\n")
  cat("Endpoint      : ", paste(object$endpoint, collapse = ", "), "\n")
  cat("Variables     : ", paste(object$terms, collapse = ", "), "\n")
  cat("Distance      : ", object$dist_method, "\n")
  cat("Trained       : ", !is.null(object$model_fit), "\n")
  if (!is.null(object$model_fit)) {
    cat("Num. trees    : ", object$model_fit$num.trees, "\n")
  }
  cat("Training data : ", nrow(object$data), " x ", ncol(object$data), "\n")
  invisible(object)
}

#' @noRd
private_summary_regression <- function(object, type_label) {
  cat("CaseBasedReasoning Model Summary\n")
  cat("================================\n")
  cat("Type          : ", type_label, "\n")
  cat("Endpoint      : ", paste(object$endpoint, collapse = ", "), "\n")
  cat("Variables     : ", paste(object$terms, collapse = ", "), "\n")
  cat("Trained       : ", !is.null(object$weights), "\n")
  if (!is.null(object$weights)) {
    cat("Coefficients  : ", length(unlist(object$weights)), "\n")
  }
  cat("Training data : ", nrow(object$data), " x ", ncol(object$data), "\n")
  invisible(object)
}
