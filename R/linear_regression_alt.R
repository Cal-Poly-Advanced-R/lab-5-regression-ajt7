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
slr_gd <- function(dat, response, explanatory){

  x <- dat %>% pull({{explanatory}})
  y <- dat %>% pull({{response}})

  explan_name <- dat %>%
    select({{explanatory}}) %>%
    names()


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

derivate_wrt_mc <- function(dat, response, explanatory) {

  browser()

  x <- dat %>% pull({{explanatory}})
  y <- dat %>% pull({{response}})

  m <- 3
  c <- 2.5
  n <- length(x)
  L <- 0.001

  y_pred <- (m * x) + c

  Deriv_m = (-2 / n) * sum(x * (y - y_pred))
  Deriv_c = (-2 / n) * sum(y - y_pred)

  m <- m - L * Deriv_m
  c <- c - L * Deriv_c

  return(c(m, c))

}
