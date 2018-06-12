#' R6 Validation Class for case based reasoning
#'
#' @docType class
#' @importFrom R6 R6Class
#' @format An \code{\link{R6Class}} generator object
#' @keywords validation
Validate <- R6Class("Validate",
                    public=list(
                      validate=function(queryData, similarCases, learnVars, plots=FALSE) {
                        # get data.frame
                        sc <- similarCases %>%
                          dplyr::select_(.dots = learnVars)
                        sc$similarCases <- "Similar\nCases"
                        nc <- queryData %>%
                          dplyr::select_(.dots = learnVars)
                        nc$similarCases <- "Reference\nCases"
                        sc <- rbind(sc, nc)
                        stat.color <- c("#0A71B4", "#61636B", "#13235B", "#E36929", "#C9D30F", "66B8DC")
                        
                        ggPlot <- list()
                        tit <- c()
                        for (var in learnVars) {
                          formel <- as.formula(paste(var, "~ similarCases"))
                          if (is.numeric(similarCases[[var]])) {
                            w <- wilcox.test(formel, data=sc, exact = F)
                            tit <- c(tit, paste0(var, " (p = ", round(w$p.value, 3), ")"))
                            cat(paste0("Two-sample Wilcoxon test for variable: ", var, "; p = ", round(w$p.value, 3), "\n"))
                            if (plots)
                              ggPlot <- c(ggPlot, list(ggplot2::ggplot(sc) +
                                                         ggplot2::geom_boxplot(aes_string(x="similarCases", y=var), alpha=0) +
                                                         ggplot2::xlab("") + 
                                                         ggplot2::ylab("") + 
                                                         cowplot::background_grid(major="y", minor="y")))
                          } else if (is.factor(similarCases[[var]])) {
                            w <- chisq.test(sc$similarCases, sc[[var]])
                            tit <- c(tit, paste0(var, " (p = ", round(w$p.value, 3), ")"))
                            cat(paste0("Chi-Square test for variable: ", var, "; p = ", round(w$p.value, 3), "\n"))
                            df <- data.frame(prop.table(table(sc$similarCases, sc[[var]]), 1))
                            if (plots)
                              ggPlot <- c(ggPlot, list(ggplot2::ggplot(df) +
                                                         ggplot2::geom_bar(aes_string(x="Var2", y="Freq", fill="Var1"), stat = "identity", position="dodge", alpha=.7) +
                                                         ggplot2::scale_fill_manual(name="", values = stat.color) +
                                                         ggplot2::xlab("") + 
                                                         ggplot2::ylab("") + 
                                                         ggplot2::theme(legend.position="top") + 
                                                         cowplot::background_grid(major="y", minor="y")))
                          }
                        }
                        if (plots) {
                          ggplot2::theme_set(cowplot::theme_cowplot())
                          return(cowplot::plot_grid(plotlist = ggPlot, labels=tit, ncol=2))
                        }
                        return()
                      }
                    )
)