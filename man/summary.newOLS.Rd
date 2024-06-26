% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcOLS.R
\name{summary.newOLS}
\alias{summary.newOLS}
\title{Summary function for newOLS objects}
\usage{
\method{summary}{newOLS}(obj)
}
\arguments{
\item{obj}{summary method for class "newOLS"}
}
\value{
a dataframe with all main results of the newOLS object
}
\description{
Summary function for newOLS objects
}
\examples{
X1 <- rnorm(100)
X2 <- rnorm(100)
y <- 1*X1+rnorm(100)
ols <- olsFunc(y~X1+X2)
summary(ols)
}
