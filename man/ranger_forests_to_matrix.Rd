% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{ranger_forests_to_matrix}
\alias{ranger_forests_to_matrix}
\title{Forest2Matrix}
\usage{
ranger_forests_to_matrix(rfObject)
}
\arguments{
\item{rfObject}{\code{ranger} object}
}
\value{
a \code{matrix} object with 
Column 1: tree ID
Column 2: node ID
Column 3: child node ID 1
Column 4: child node ID 2
}
\description{
Transform trees of a \code{ranger}-object to a matrix
}
\examples{
\donttest{
library(ranger)
rf.fit <- ranger(Species ~ ., data = iris, num.trees = 5, write.forest = TRUE)
forest_matrix <- ranger_forests_to_matrix(rf.fit)
}

}
