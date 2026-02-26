#' RandomForest Model for Searching Similar Cases
#'
#' This class uses the proximity or depth matrix of the RandomForest algorithm
#' as a similarity matrix of training and query
#' observations. By default all cases with at least one missing values are dropped
#' from learning, calculating the distance matrix and searching for similar
#' cases.
#'
#' @references
#' Englund and Verikas. A novel approach to estimate proximity in a random
#' forest: An exploratory study.
#' @export
RFModel <- R6Class(
  classname = "RFModel",
  inherit = CBRBase,
  public = list(
    #' @field model the statistical model
    model        = 'ranger',
    #' @field model_params model arguments
    model_params = list(write.forest = TRUE, verbose = TRUE),
    #' @field  dist_method Distance method
    dist_method  = "Depth",
    #' @description
    #' Prints information of the initialized object
    print = function() {
      cat("Case-Based-Reasoning with RandomForests\n")
      cat("---------------------------------------\n")
      cat("Model     : ", paste(self$model, collapse = ", "), "\n")
      cat("Endpoints : ", paste(self$endpoint, collapse = ", "), "\n")
      cat("Variables : ", paste(self$terms, collapse = ", "), "\n")
      cat("Method    : ", paste(self$dist_method, collapse = ", "), "\n")
      cat("Trained   : ", ifelse(is.null(self$model_fit), FALSE, TRUE), "\n")
    },
    #' @description
    #' Initialize a RandomForest object for searching similar cases.
    #'
    #' @param formula Object of class formula or character describing the model fit.
    #' @param data Training data of class data.frame
    #' @param ... ranger RandomForest arguments
    initialize = function(formula, data, ...) {
      super$initialize(formula, data)
      self$model_params <- list(...)
      self$model_params$write.forest <- TRUE
      self$model_params$verbose <- TRUE
    },
    #' @description
    #' Fit the RandomForest
    fit = function() {
      train_tbl <- self$data[, c(self$endpoint, self$terms), drop = FALSE]
      train_tbl <- private$check_data(train_tbl)

      # train model
      params <- self$model_params
      params$data <- train_tbl
      params$formula <- self$formula
      self$model_fit <- do.call(ranger::ranger, params)
    },
    #' @description
    #' Set the distance method. Available are Proximity and Depth
    #'
    #' @param method Distance calculation method (default: Proximity)
    set_distance_method = function(method = "Depth") {
      method <- match.arg(method, c("Proximity", "Depth"))
      self$dist_method <- method
    }
  ),
  private = list(
    get_distance_matrix = function(query = NULL) {
      validate_ranger(self$model_fit)
      self$dist_matrix <- distance_random_forest(
        x        = private$to_int(self$data[, self$terms, drop = FALSE]),
        y        = private$to_int(query),
        method   = self$dist_method,
        rfObject = self$model_fit
      )
    }
  )
)
