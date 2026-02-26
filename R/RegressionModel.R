#' Root class for Regression Models, e.g., CPH, logistic, and linear regression
#'
#' @keywords data-preparation
RegressionModel <- R6Class(
  classname = "RegressionModel",
  inherit = CBRBase,
  public = list(
    #' @field model_params rms arguments
    model_params = list(x = TRUE, y = TRUE),
    #' @field weights Weights for distance calculation
    weights     = NULL,
    #' @description
    #' Prints information of the initialized object
    print = function() {
      cat("Case-Based-Reasoning based Regression Coefficients\n")
      cat("---------------------------------------\n")
      cat("Model     : ", paste(self$model, collapse = ", "), "\n")
      cat("Endpoint  : ", paste(self$endpoint, collapse = ", "), "\n")
      cat("Variables : ", paste(self$terms, collapse = ", "), "\n")
      cat("Trained   : ", ifelse(is.null(self$weights), FALSE, TRUE), "\n")
    },
    #' @description
    #' Fast backward variable selection with penalization
    variable_selection = function() {
      train_tbl <- self$data[, c(self$endpoint, self$terms), drop = FALSE]
      train_tbl <- private$check_data(train_tbl)

      # train regression model
      func <- utils::getFromNamespace(self$model, "rms")
      params <- self$model_params
      params$data <- train_tbl
      params$formula <- self$formula
      self$model_fit <- do.call(func, params)

      # Variable Selection
      vars <- rms::fastbw(fit = self$model_fit, type = "i")
      selected_vars <- c(self$endpoint, self$terms)
      selected_vars
    },
    #' @description
    #' Fit the regression model
    fit = function() {
      train_tbl <- self$data[, c(self$endpoint, self$terms), drop = FALSE]
      train_tbl <- private$check_data(train_tbl)

      # train regression model
      func <- utils::getFromNamespace(self$model, "rms")
      params <- self$model_params
      params$data <- train_tbl
      params$formula <- self$formula
      self$model_fit <- do.call(func, params)

      nVars <- length(self$terms)
      weights <- vector("list", nVars)
      names(weights) <- self$terms

      # get weights
      for (i in 1:nVars) {
        if (is.factor(train_tbl[[self$terms[i]]])) {
          nLev <- nlevels(train_tbl[[self$terms[i]]])
          weightsTmp <- rep(NA, times = nLev)
          names(weightsTmp) <- levels(train_tbl[[self$terms[i]]])
          for (j in 1:nLev) {
            myLevel <- paste(self$terms[i], "=", levels(train_tbl[[self$terms[i]]])[j], sep = "")
            if (j == 1) {
              weightsTmp[j] <- 0
            } else {
              weightsTmp[j] <- self$model_fit$coefficients[myLevel]
            }
          }
          weights[[i]] <- weightsTmp
        } else {  # else numeric
          myLevel <- paste(self$terms[i])
          weights[[i]] <- self$model_fit$coefficients[myLevel]
        }
      }
      self$weights <- weights
    }
  ),
  private = list(
    # check weights on NA
    check_weights = function() {
      wNA <- unlist(lapply(self$weights, function(x) any(is.na(x))))
      if (any(wNA)) {
        warning(paste0("Variables: ", names(wNA)[which(wNA)], " have NA weights.\n"))
        return(TRUE)
      }
      return(FALSE)
    },
    # transform_data:
    # we transform all factors to their corresponding
    # weights and set weight equal to 1 for factor variables
    transform_data = function(queryData, dtData, learnVars, weights) {
      nVars <- length(learnVars)
      trafoWeights <- rep(0, nVars)
      for (j in 1:nVars) {
        if (is.factor(dtData[[learnVars[j]]])) {
          if (!is.null(queryData)) {
            queryData[[learnVars[j]]] <- weights[[learnVars[j]]][queryData[[learnVars[j]]]]
          }
          dtData[[learnVars[j]]] <- weights[[learnVars[j]]][dtData[[learnVars[j]]]]
          trafoWeights[j] <- 1
        } else { # else keep weights
          trafoWeights[j] <- weights[[learnVars[j]]]
        }
      }
      names(trafoWeights) <- NULL

      if (is.null(queryData)) {
        queryData <- NULL
      } else {
        queryData <- unname(as.matrix(queryData[, learnVars, drop = FALSE]))
      }
      return(list(queryData    = queryData,
                  data         = unname(as.matrix(dtData[, learnVars, drop = FALSE])),
                  trafoWeights = trafoWeights))
    },
    # calculate weighted absolute distance
    get_distance_matrix = function(query = NULL) {
      if (!is.list(self$weights)) {
        stop("Model not trained: call $fit() first.")
      }
      if (private$check_weights()) {
        stop("NA values in regression beta coefficients!")
      }

      # transform for weighted distance calculations
      training_data_list <- private$transform_data(queryData = query,
                                                   dtData    = self$data,
                                                   learnVars = self$terms,
                                                   weights   = self$weights)

      # calculate distance matrix
      self$dist_matrix <- as.matrix(weighted_distance(
        x       = training_data_list$data,
        y       = training_data_list$queryData,
        weights = training_data_list$trafoWeights
      ))
    }
  )
)


#' Cox-Beta Model for Case-Based-Reasoning
#'
#' Regression beta coefficients obtained from a CPH regression model fitted on the
#' training data are used for building a weighted distance measure between
#' train and test data. Afterwards, we will use these weights for calculating a
#' (n x m)-distance matrix, where n is the number of observations in the training data,
#' and m is the number of observations of the test data. The user can use this
#' distance matrix for further cluster analysis or for extracting for each test observation
#' k (= 1,...,l) similar cases from the train data. We use the rms-package for model fitting,
#' variable selection, and checking model assumptions.
#' If the user omits the test data, this functions returns a n x n-distance matrix.
#'
#' @export
CoxModel <- R6Class(
  classname = "CoxModel",
  inherit   = RegressionModel,
  public    = list(
    #' @field model the statistical model
    model        = 'cph',
    #' @field model_params rms arguments
    model_params = list(x = TRUE, y = TRUE, surv = TRUE),
    #' @description
    #' Check proportional hazard assumption graphically
    check_ph = function() {
      if (!is.list(self$weights)) {
        stop("The model is not trained: call $fit() first.")
      }
      if (!requireNamespace("patchwork", quietly = TRUE)) {
        stop("Package 'patchwork' is required for check_ph(). Install it with install.packages('patchwork').")
      }
      n <- length(self$terms)
      ggPlot <- list()
      zph <- survival::cox.zph(self$model_fit, "rank")
      for (i in 1:n) {
        df <- data.frame(x = zph$x, y = zph$y[, i])
        g <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_hline(yintercept = 0, colour = "grey") +
          ggplot2::geom_point() +
          ggplot2::geom_smooth(color = "#18BC9C", fill = "#18BC9C") +
          ggplot2::ylab(paste0("Beta(t) of ", self$terms[i])) +
          ggplot2::xlab("Time to Event") +
          ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey90"),
                         panel.grid.minor = ggplot2::element_line(colour = "grey95"))
        ggPlot <- c(ggPlot, list(g))
      }
      return(patchwork::wrap_plots(ggPlot, ncol = 2))
    }
  )
)


#' Linear Regression Model for Case-Based-Reasoning
#'
#' @export
LinearModel <- R6Class(
  classname = "LinearModel",
  inherit   = RegressionModel,
  public    = list(
    #' @field model the statistical model
    model       = 'ols'
  )
)


#' Logistic Regression Model for Case-Based-Reasoning
#'
#' @export
LogisticModel <- R6Class(
  classname = "LogisticModel",
  inherit   = RegressionModel,
  public    = list(
    #' @field model the statistical model
    model       = 'lrm'
  )
)
