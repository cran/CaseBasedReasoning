#' Root class for common functionality of this package
#'
#' @keywords datapreparation
CBRBase <- R6Class("CBRBase",
                   public = list(
                     #' @field model the statistical model
                     model     = '',
                     #' @field data training data
                     data      = NULL,
                     #' @field model_fit trained object
                     model_fit = NULL,
                     #' @field formula Object of class formula or character describing the model fit
                     formula   = NULL,
                     #' @field terms terms of the formula
                     terms     = NULL,
                     #' @field endpoint Target variable
                     endpoint  = NULL,
                     #' @field dist_matrix A matrix with distances
                     dist_matrix   = NULL,
                     #' @field order_matrix A matrix with the order indices for similar cases search
                     order_matrix  = NULL,
                     #' @description
                     #' Initialize object for searching similar cases
                     #'
                     #' @param formula Object of class formula or character describing the model fit
                     #' @param data Training data of class data.frame
                     initialize = function(formula, data) {
                       formula <- as.formula(formula)
                       if (!inherits(formula, "formula")) {
                         stop("Invalid formula: expected a formula object.")
                       }
                       self$formula <- formula
                       self$terms <- labels(terms(formula, data = data))
                       self$endpoint <- setdiff(all.vars(formula), '.')
                       self$endpoint <- setdiff(self$endpoint, self$terms)
                       # validate that formula terms exist in data
                       missing_vars <- setdiff(c(self$endpoint, self$terms), names(data))
                       if (length(missing_vars) > 0) {
                         stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
                       }
                       self$data <- data
                     },
                     #' @description
                     #' Fit the Model
                     fit = function() {
                       # virtual function
                     },
                     #' @description
                     #' Calculates the distance matrix
                     #'
                     #' @param query Query data of class data.frame
                     calc_distance_matrix = function(query = NULL) {
                       private$get_distance_matrix(query = query)
                     },
                     #' @description
                     #' Extracts similar cases
                     #'
                     #' @param query Query data of class data.frame
                     #' @param k number of similar cases
                     #' @param add_distance Add distance to result data.frame
                     #' @param merge Add query data to matched cases data.frame
                     get_similar_cases = function(query, k = 1, add_distance = TRUE, merge = FALSE) {
                       if (!is.numeric(k)) {
                         stop("'k' must be numeric.")
                       }
                       if (k < 1) {
                         stop("'k' must be >= 1.")
                       }
                       k <- as.integer(k)

                       if (missing(query)) {
                         stop("'query' is required. Pass the data for which you want to find similar cases.")
                       }

                       # calculate distance matrix
                       distance_matrix <- private$get_distance_matrix(query = query)

                       # calculate distance and order of cases based on distance calculation
                       similar_cases_tbl <- private$extract_similar_cases(
                         x              = self$data,
                         query          = query,
                         distanceMatrix = distance_matrix,
                         k              = k,
                         add_distance   = add_distance,
                         merge          = merge
                       )
                       similar_cases_tbl
                     }
                   ),
                   active = list(
                     #' @field endPoint Deprecated: use \code{endpoint} instead.
                     endPoint = function(value) {
                       if (missing(value)) {
                         .Deprecated("endpoint", old = "endPoint")
                         self$endpoint
                       } else {
                         .Deprecated("endpoint", old = "endPoint")
                         self$endpoint <- value
                       }
                     },
                     #' @field distMat Deprecated: use \code{dist_matrix} instead.
                     distMat = function(value) {
                       if (missing(value)) {
                         .Deprecated("dist_matrix", old = "distMat")
                         self$dist_matrix
                       } else {
                         .Deprecated("dist_matrix", old = "distMat")
                         self$dist_matrix <- value
                       }
                     },
                     #' @field orderMat Deprecated: use \code{order_matrix} instead.
                     orderMat = function(value) {
                       if (missing(value)) {
                         .Deprecated("order_matrix", old = "orderMat")
                         self$order_matrix
                       } else {
                         .Deprecated("order_matrix", old = "orderMat")
                         self$order_matrix <- value
                       }
                     }
                   ),
                   private = list(
                     check_data = function(x, isLearning = TRUE) {
                       # drop cases with missing values in the relevant variables
                       x <- private$drop_missing(x, isLearning)
                       if (nrow(x) == 0) {
                         if (isLearning) {
                           stop("Learning data is empty after NA elimination.")
                         } else {
                           stop("Query is empty after NA elimination.")
                         }
                       }
                       # check character variables: need factors
                       x <- private$check_factor(x)
                       return(x)
                     },
                     drop_missing = function(df, isLearning = FALSE) {
                       cols <- c(self$endpoint, self$terms)
                       df <- df[, cols, drop = FALSE]
                       rs <- rowSums(is.na(df))
                       idDrop <- which(rs > 0)
                       if (length(idDrop) > 0) {
                         df <- df[-idDrop, ]
                         warning(paste0("Dropped cases with missing values: ", length(idDrop)))
                       }
                       df
                     },
                     # transform character variables to factor
                     check_factor = function(x) {
                       trf <- c()
                       for (var in self$terms) {
                         if (is.character(x[[var]])) {
                           trf <- c(trf, var)
                           x[[var]] <- factor(x[[var]])
                         }
                       }
                       if (length(trf) > 0) {
                         warning(paste0("Following variables are transformed to class factor: ", paste(trf, collapse = ", ")))
                       }
                       return(x)
                     },
                     # transforms data to integer representation;
                     # necessary for c++ functions
                     to_int = function(x) {
                       if (is.null(x))
                         return(x)

                       for (i in 1:ncol(x)) {
                         if (!is.numeric(x[[i]])) {
                           x[[i]] <- as.numeric(as.factor(x[[i]]))
                         }
                       }
                       return(x)
                     },
                     get_distance_matrix = function(query = NULL) {
                       # virtual function
                     },
                     extract_similar_cases = function(x, query, distanceMatrix, k = 1, add_distance = TRUE, merge = TRUE) {
                       m <- ncol(distanceMatrix)

                       # get closest elements
                       orderedMatrix <- cpp_orderMatrix(
                         as.matrix(distanceMatrix),
                         sortDirection = 0,
                         k             = k
                       )

                       colnames(orderedMatrix) <- paste0("c", 1:ncol(orderedMatrix))

                       # replace purrr::map2 + purrr::reduce with base R
                       df_list <- lapply(seq_len(ncol(orderedMatrix)), function(colID) {
                         rowIDs <- orderedMatrix[, colID]
                         dtTmp <- x[rowIDs, ]
                         if (add_distance) {
                           dtTmp$scDist <- distanceMatrix[rowIDs, colID]
                         }
                         dtTmp
                       })
                       df_sc <- do.call(rbind, df_list)

                       # mark similar cases: 1:n ids
                       df_sc$caseId <- rep(1:k, m)

                       if (merge) {
                         df_sc <- private$merge_matched_data(
                           query = query,
                           df_sc = df_sc,
                           k     = k
                         )
                       }
                       df_sc
                     },
                     merge_matched_data = function(query, df_sc, k) {
                       # scCaseId: finally sort data.frame such that matched cases are close
                       query$scCaseId <- 1:nrow(query)
                       query$group <- "Query"
                       query$scDist <- 0.0
                       query$caseId <- 0
                       matchedData <- df_sc
                       matchedData$scCaseId <- rep(1:nrow(query), each = k)
                       matchedData$group <- "Matched"
                       query <- query[, names(matchedData), drop = FALSE]
                       result <- rbind(query, matchedData)
                       result[order(result$scCaseId), ]
                     }
                   )
)
