#' Implements simple linear regression by gradient descent
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
slr_gd <- function(dat, response, explanatory, iterations){

  for(i in length({{iterations}})) {

    results <- derivate_wrt_mc({{dat}}, {{response}}, {{explanatory}})

  }

  ### Compute coefficients by gradient descent
  ### Return a data frame of the same form as in the `simple_linear_regression`

  return(results)

}


#' Implements linear regression with many predictors by gradient descent
#'
#' This function computes coefficients for multiple regression by gradient descent
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export
mlr_gd <- function(dat, response) {



  ### Compute coefficients by gradient descent
  ### Return a data frame of the same form as in the `multiple_linear_regression`

  return(results)

}

#' Implements linear regression with many predictors by matrix decomposition
#'
#' This function computes coefficients for multiple regression by QR matrix decomposition
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export
mlr_qr <- function(dat, response) {



  ### Compute coefficients by QR decomposition
  ### Return a data frame of the same form as in the `multiple_linear_regression`

  return(results)

}

loss <- function(y, y_pred) {

  mean((y - y_pred) ^ 2)

}


#500 iterations is magic number right now
derivate_wrt_mc <- function(dat, response, explanatory, iterations) {

  x <- dat %>% pull({{explanatory}})
  y <- dat %>% pull({{response}})

  m <- 0
  c <- 0
  n <- length(x)
  L <- 0.01

  for(i in 1:iterations) {

    y_pred <- (m * x) + c

    Deriv_m = (-2 / n) * sum(x * (y - y_pred))
    Deriv_c = (-2 / n) * sum(y - y_pred)

    m <- m - L * Deriv_m
    c <- c - L * Deriv_c

  }

  return(c(m, c))

}
