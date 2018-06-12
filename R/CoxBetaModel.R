#' Cox-Beta Model
#'
#' Regression beta coefficients are use for building a weighted distance measure between
#' the learning and verum data set. The learning data set is used for learning the Cox
#' model and use the obtained weights for calculating a (n x m)-distance
#' matrix, where n is the number of cases in the learning data set and m is the
#' number of cases of the query data. This distance matrix can then be used for
#' cluster analysis or for getting for each case in the query data k (=1,...,l)
#' smilar cases from the learning data. The rms-package is used for model fitting,
#' variable selection, and checking the assumptions.
#' If query data is ommitted, a n x n- distance matrix is returned.
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
#'   \item{...}{See \link{CBRBase} class.}
#'   }
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @format An \code{\link{R6Class}} generator object
#' 
CoxBetaModel <- R6Class(classname = "CoxBetaModel",
                        inherit = CBRBase,
                        public=list(
                          weights    = NULL,
                          coxFit     = NULL,
                          cph        = NULL,
                          modelValid = NULL,
                          # fast backward variable selection with penalization
                          variable_selection = function(dtData) {
                            dtData %>%
                              dplyr::select_(.dots = c(self$endPoint, self$terms)) -> dtData
                            dtData <- private$check_data(dtData)
                            
                            # Timing
                            start <- Sys.time()
                            cat("Start learning...\n")
                            #  datadist scoping
                            cbrCoxModel_data <<- rms::datadist(dtData)
                            options(datadist="cbrCoxModel_data")
                            
                            # Cox Regression
                            dtData %>% 
                              rms::cph(formula = self$formula, data = ., x = TRUE, y = TRUE, surv = T) -> self$coxFit
                            
                            # Variable Selection
                            vars <- rms::fastbw(fit = coxFit, type = "i")
                            cat(paste0("Initial variable set: ", paste(c(self$endPoint, self$terms), collapse = ", "), "\n"))
                            cat(paste0("Selected variable set: ", paste(vars$names.kept, collapse = ", "), "\n"))
                            vars <- c(self$endPoint, self$terms)
                            self$formula <- as.formula(paste0("Surv(", vars[1], ", ", vars[2], "~", paste(vars$names.kept, collapse = "+")))
                            
                            # end timing
                            options(datadist=NULL)
                            end <- Sys.time()
                            duration <- round(as.numeric(end - start), 2)
                            cat(paste0("Learning finished in: ", duration, " seconds.\n"))
                          },
                          # fit model
                          fit = function(dtData) {
                            dtData %>%
                              dplyr::select_(.dots = c(self$endPoint, self$terms)) -> dtData
                            dtData <- private$check_data(dtData)
                            
                            # Timing
                            start <- Sys.time()
                            cat("Start learning...\n")
                            #  datadist scoping
                            # attach(list(), name="design.options")
                            # on.exit(detach("design.options"))
                            # assign('cbrCoxModel_data', rms::datadist(dtData), pos='design.options')
                            cbrCoxModel_data <<- rms::datadist(dtData)
                            options(datadist="cbrCoxModel_data")
                            # Cox Regression
                            dtData %>% 
                              rms::cph(formula = self$formula, data = ., x = TRUE, y = TRUE, surv = T) -> self$coxFit
                            self$cph <- survival::cox.zph(self$coxFit, "rank")
                            
                            nVars <- length(self$terms) 
                            weights <- vector("list", nVars)
                            names(weights) <- self$terms
                            # get weights
                            for (i in 1:nVars) {
                              if (is.factor(dtData[[self$terms[i]]])) {
                                nLev <- nlevels(dtData[[self$terms[i]]])
                                weightsTmp <- rep(NA, times = nLev)
                                names(weightsTmp) <- levels(dtData[[self$terms[i]]])
                                for (j in 1:nLev) {
                                  myLevel <- paste(self$terms[i], "=", levels(dtData[[self$terms[i]]])[j], sep="")
                                  if (j==1) {
                                    weightsTmp[j] <- 0
                                  } else {
                                    weightsTmp[j] <- self$coxFit$coefficients[myLevel]
                                  }
                                }
                                weights[[i]] <- weightsTmp
                              } else {  # else numeric
                                myLevel <- paste(self$terms[i])
                                weights[[i]] <- self$coxFit$coefficients[myLevel]
                              }
                            }
                            self$weights <- weights
                            # end timing
                            options(datadist=NULL)
                            end <- Sys.time()
                            duration <- round(as.numeric(end - start), 2)
                            cat(paste0("Learning finished in: ", duration, " seconds.\n"))
                          },
                          # check proportional hazard
                          check_ph=function() {
                            # learn if weights are empty
                            testthat::expect_is(self$weights, "list", info = "Model not trained")
                            n <- length(self$terms)
                            ggPlot <- list()
                            for (i in 1:n) {
                              df <- data.frame(x=self$cph$x, y=self$cph$y[, i])
                              g <- ggplot2::ggplot(df, aes(x=x, y=y)) +
                                ggplot2::geom_hline(yintercept=0, colour="grey") +
                                ggplot2::geom_point() +
                                ggplot2::geom_smooth(color="#2773ae", fill="#2773ae") +
                                ggplot2::ylab(paste0("Beta(t) of ", self$terms[i])) +
                                ggplot2::xlab("Time to Event") +
                                cowplot::background_grid(major="xy", minor="xy")
                              ggPlot <- c(ggPlot, list(g))
                            }
                            return(cowplot::plot_grid(plotlist = ggPlot,
                                                      ncol     = 2))
                          }
                        ),
                        private = list(
                          # check weights on NA
                          check_weights = function() {
                            wNA <- unlist(lapply(self$weights, function(x) any(is.na(x))))
                            if (any(wNA)) {
                              cat(paste0("Variables: ", names(wNA)[which(wNA)], " have NA weights.\n"))
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
                            
                            if(is.null(queryData)) {
                              queryData <- NULL
                            } else {
                              queryData <- unname(as.matrix(queryData[, learnVars, with=F]))
                            }
                            return(list(queryData    = queryData,
                                        data         = unname(as.matrix(dtData[, learnVars, with=F])),
                                        trafoWeights = trafoWeights))
                          },
                          # calculate weighted absolute distance 
                          get_distance_matrix=function(dtData, queryData = NULL) {
                            if (is(dtData, "data.table")) {
                              dtData <- data.table::copy(dtData)
                            } else {
                              dtData <- data.table::copy(data.table::as.data.table(dtData))
                            }
                            # learn if weights are empty
                            testthat::expect_is(self$weights, "list", info = "Model not trained")
                            testthat::expect_false(private$check_weights(), info = "NA values in regression beta coefficients!")
                            
                            if (is.null(queryData)) {
                              queryData <- data.table::copy(dtData)
                            } 
                            
                            # transform for weighted distance calculations
                            trData <- private$transform_data(queryData = queryData,  
                                                             dtData    = dtData, 
                                                             learnVars = self$terms, 
                                                             weights   = self$weights)
                            
                            # calculate distance matrix
                            self$distMat <- weightedDistance(x       = trData$data, 
                                                             y       = trData$queryData, 
                                                             weights = trData$trafoWeights) %>% 
                              as.matrix()
                          }
                        )
)