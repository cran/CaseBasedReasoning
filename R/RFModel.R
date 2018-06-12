#' RandomForest Proximity
#' 
#' This class uses the proximity matrix of the random survival forest algorithm 
#' as a similarity matrix (sqrt(1 - proximity matrix)) of learning and verum 
#' cases. By default all cases with at least one missing values are dropped 
#' from learning, calculating the distance matrix, and searching for similar
#' cases. 
#' 
#' @references 
#' Englund and Verikas. A novel approach to estimate proximity in a random 
#' forest: An exploratory study.

#' @format \code{\link[R6]{R6Class}} object
#'
#' @section Usage:
#' For usage details see \bold{Methods, Arguments, and Examples} sections.
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{new(formula, ...)}}{This method is used to create an
#'   object of this class \code{RFModel}. Formula for analysis has to be 
#'   provided. Further parameters for the ranger call can be provided here.}
#'   \item{\code{fit(dtData)}}{Fits the RandomForest model.}
#'   \item{\code{set_dist(method)}}{Set the proximity measure, 'Depth' (Default)
#'   and 'Proximity' are allowed.}
#'   \item{...}{See \link{CBRBase} class.}
#'   }
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
RFModel <- R6Class(classname = "RFModel",
                   inherit = CBRBase,
                   public=list(
                     rangerObj  = NULL,
                     methodArgs = NULL,
                     distMat    = NULL,
                     distMethod = "Depth",
                     print = function() {
                       cat("Case-Based-Reasoning with RandomForests\n")
                       cat("---------------------------------------\n")
                       cat("Endpoints : ", paste(self$endPoint, collapse = ", "))
                       cat("Variables : ", paste(self$terms, collapse = ", "))
                       cat("Trained   : ", ifelse(is.null(self$rangerObj), FALSE, TRUE))
                     },
                     initialize = function(formula, ntree = 500, mtry = NULL, splitrule="maxstat", minprop=.35, ...) {
                       # split rule
                       if (missing(splitrule)) {
                         splitrule <- "logrank"
                       }
                       splitrule <- match.arg(splitrule, c("logrank", "maxstat", "C"))
                       
                       super$initialize(formula)
                       args <- list(
                         ntree = ntree,
                         mtry = mtry,
                         splitrule = splitrule,
                         minprop = minprop
                       )
                       self$methodArgs <- args
                     },
                     fit = function(dtData) {
                       dtData %>%
                         dplyr::select_(.dots = c(self$endPoint, self$terms)) -> dtData
                       dtData <- private$check_data(dtData)
                       
                       # Timing
                       start <- Sys.time()
                       cat("Start learning...\n")
                       
                       # Learning
                       self$rangerObj <- ranger::ranger(formula      = self$formula,
                                                        data         = dtData,
                                                        num.trees    = self$methodArgs$ntree,
                                                        mtry         = self$methodArgs$mtry,
                                                        splitrule    = self$methodArgs$splitrule, 
                                                        num.threads  = NULL, # self$methodArgs$nCores,
                                                        write.forest = T,
                                                        verbose      = T)
                       end <- Sys.time()
                       duration <- round(as.numeric(end - start), 2)
                       cat(paste0("Random Forest for Survival calculation finished in: ", duration, " seconds.\n"))
                     },
                     set_dist=function(distMethod = "Depth") {
                       distMethod <- match.arg(distMethod, c("Proximity", "Depth"))
                       self$distMethod <- distMethod
                     }
                   ),
                   private = list(
                     get_distance_matrix = function(dtData, queryData = NULL) {
                       testthat::expect_is(self$rangerObj, "ranger")
                       self$distMat <- distanceRandomForest(x        = private$to_int(dtData),
                                                            y        = private$to_int(queryData), 
                                                            method   = self$distMethod,
                                                            rfObject = self$rangerObj)
                     }
                   )
)