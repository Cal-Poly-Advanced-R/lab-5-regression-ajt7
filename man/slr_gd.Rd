% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linear_regression_alt.R
\name{slr_gd}
\alias{slr_gd}
\title{Implements simple linear regression by gradient descent}
\usage{
slr_gd(dat, response, explanatory, iterations = 30000)
}
\arguments{
\item{dat}{A data frame}

\item{response}{The name of a response variable in the data frame (unquoted)}

\item{explanatory}{The name of the explanatory variable in the data frame (unquoted)}

\item{iterations}{Number of iterations for gradient descent to run}
}
\value{
A data frame of coefficients
}
\description{
Implements simple linear regression by gradient descent
}
