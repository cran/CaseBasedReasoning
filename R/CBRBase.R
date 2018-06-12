#' Root class for common functionality of this package
#' 
#' @keywords data-preparation
CBRBase <- R6Class("CBRBase",
                   public = list(
                     # variables
                     formula   = NULL,
                     terms     = NULL,
                     endPoint  = NULL,
                     # distance matrix
                     distMat   = NULL,
                     orderMat  = NULL,
                     # initialize class
                     initialize = function(formula) {
                       formula <- formula(formula)
                       testthat::expect_is(formula, "formula", "Invalid formula.")
                       self$formula <- formula
                       self$terms <- attr(terms(formula, data=self$data), which = "term.labels")
                       self$endPoint <- all.vars(formula)[1:2]
                     },
                     fit = function(dtData) {
                       # virtual function
                     },
                     # calculate distance matrix
                     calc_distance_matrix = function(dtData, queryData = NULL) {
                       # Start calculation
                       start <- Sys.time()
                       cat("Start calculating distance matrix...\n")
                       # get distance matrix
                       dtData %>% 
                         private$get_distance_matrix(dtData = ., queryData = queryData) -> distanceMatrix
                       end <- Sys.time()
                       duration <- round(as.numeric(end - start), 2)
                       cat(paste0("Distance matrix calculation finished in: ", duration, " seconds.\n"))
                       distanceMatrix
                     },
                     # get similar cases from reference data
                     get_similar_cases = function(dtData, queryData, k = 1, addDistance = T, merge = T) { 
                       
                       # check nCases input 
                       testthat::expect_is(k, "numeric")
                       testthat::expect_true(k >= 0, "numeric")
                       # catch floating numbers
                       k <- as.integer(k)
                       
                       if (!is(dtData, "data.table")) {
                         dtData <- data.table::as.data.table(dtData)
                       }
                       
                       if (missing(queryData)) {
                         cat("No query data.\n") 
                         queryData <- data.table::copy(dtData)
                       } else {
                         queryData <- data.table::as.data.table(queryData)
                       }
                       
                       start <- Sys.time()
                       cat("Start caclulating similar cases...\n")
                       
                       # calculate distance matrix
                       dtData %>% 
                         private$get_distance_matrix(queryData = queryData) -> distanceMatrix
                       
                       # calculate distance and order of cases based on distance calculation
                       dtData %>% 
                         private$extract_similar_cases(queryData      = queryData,
                                                       distanceMatrix = distanceMatrix, 
                                                       k              = k, 
                                                       addDistance    = addDistance, 
                                                       merge          = merge) -> similarCases
                       end <- Sys.time()
                       duration <- round(as.numeric(end - start), 2)
                       cat(paste0("Similar cases calculation finished in: ", duration, " seconds.\n"))
                       
                       similarCases
                     }
                   ),
                   private = list(
                     # check data sets
                     check_data = function(x, isLearning=T) {
                       if (is(x, "data.table")) {
                         x <- data.table::copy(x)
                       } else {
                         x <- data.table::copy(data.table::as.data.table(x))
                       }
                       # drop cases with missing values in the relevant variables
                       x <- private$drop_missing(x, isLearning)
                       if (nrow(x) == 0) {
                         if (isLearning) {
                           stop("Error: Learning data is empty after NA elimination.")
                         } else {
                           stop("Error: Query data is empty after NA elimination.")
                         }
                       }
                       # check character variables: need factors
                       x <- private$check_factor(x)
                       # check levels of factor variables
                       # more tests
                       return(x)
                     },
                     # drop missing values from data
                     drop_missing = function(x, isLearning=F) {
                       dtData <- x %>% 
                         dplyr::select_(.dots = c(self$endPoint, self$terms))
                       rs <- rowSums(is.na(dtData))
                       idDrop <- which(rs > 0)
                       cat(paste0("Dropped cases with missing values: ", length(idDrop), "\n"))
                       if (length(idDrop) > 0)
                         x <- x[-idDrop, ]
                       return(x)
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
                         cat(paste0("Following variables are transformed to class factor: ", paste(trf, collapse=", "), "\n"))
                       }
                       return(x)
                     },
                     #' transforms data to integer representation;
                     #' necessary for c++ functions
                     to_int = function(x) {
                       if (is.null(x))
                         return(x)
                       
                       for (i in 1:ncol(x)) {
                         if (is(x, "data.table")) {
                           x[[i]] <- as.numeric(as.factor(x[[i]]))
                         } else {
                           x[, i] <- as.numeric(as.factor(x[, i]))
                         }
                       }
                       return(x)
                     },
                     # calculate distance 
                     get_distance_matrix=function(dtData, queryData = NULL) {
                       # model specific
                     },
                     # get similar cases
                     extract_similar_cases=function(dtData, queryData, distanceMatrix, k = 1, addDistance = T, merge = T) {
                       m <- ncol(distanceMatrix)
                       
                       # get closest elements
                       distanceMatrix %>% 
                         as.matrix() %>% 
                         cpp_orderMatrix(sortDirection = 0,
                                         k             = k) -> orderedMatrix
                       
                       colID <- 1:ncol(orderedMatrix)
                       orderedMatrix %>% 
                         as.data.frame() %>% 
                         purrr::map2(.y = colID, .f = function(rowIDs, colID, dtData, distanceMatrix) {
                           dtTmp <- dtData[rowIDs, ]
                           if (addDistance) {
                             dtTmp$scDist <- distanceMatrix[rowIDs, colID]
                           }
                           dtTmp
                         }, dtData = dtData, distanceMatrix = distanceMatrix) -> similarCases
                       similarCases <- data.table::rbindlist(similarCases)
                       
                       # mark similar cases: 1:n ids
                       similarCases$caseId <- rep(1:k, m)
                       
                       if (merge) {
                         queryData %>% 
                           private$merge_matched_data(similarCases = similarCases, k = k) -> similarCases
                       }
                       similarCases
                     },
                     # return query + matched data
                     merge_matched_data = function(queryData, similarCases, k) {
                       # scCaseId: finally sort data.frame such that matched cases are close
                       queryData$scCaseId <- 1:nrow(queryData)
                       queryData$group <- "Query Data"
                       queryData$scDist <- 0.0
                       queryData$caseId <- 0
                       matchedData <- similarCases
                       matchedData$scCaseId <- rep(1:nrow(queryData), each = k)
                       matchedData$group <- "Matched Data"
                       queryData %>% 
                         dplyr::select_(.dots = names(matchedData)) -> queryData
                       rbind(queryData, matchedData) %>% 
                         dplyr::arrange(scCaseId)
                     }
                   )
)