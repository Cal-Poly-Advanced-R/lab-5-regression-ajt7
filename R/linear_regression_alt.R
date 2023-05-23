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
slr_gd <- function(dat, response, explanatory, iterations = 20000){

  x <- dat %>% pull({{explanatory}})
  y <- dat %>% pull({{response}})

  m <- 0
  c <- 0
  n <- length(x)
  L <- 0.05

  for(i in 1:iterations) {

    y_pred <- (m * x) + c

    Deriv_m = (-2 / n) * sum(x * (y - y_pred))
    Deriv_c = (-2 / n) * sum(y - y_pred)

    m <- m - L * Deriv_m
    c <- c - L * Deriv_c

  }

  results <- data.frame(
    Intercept = c,
    Coefficient = m
    )

  return(results)

  ### Compute coefficients by gradient descent
  ### Return a data frame of the same form as in the `simple_linear_regression`

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
mlr_gd <- function(dat, response, learning_rate = 0.001, iterations = 20000) {

  browser()

  y <- dat %>% pull({{response}})
  x <- dat %>% select(-{{response}})

  #Names of explanatory variables
  explan_name <- dat %>%
    select(-{{response}}) %>%
    names()

  #Adding intercept column
  x <- x %>%
    mutate(intercept = 1, .before = 1)

  #Getting number of explanatory variables
  num_explanatory <- ncol(x)

  #Converting to matrices
  x <- as.matrix(x)
  y <- as.matrix(y)

  #Initialize a of vector of coefficient estimates (zeros)
  thetas <- rep(0, num_explanatory)

  for (i in 1:iterations) {

    y_pred <- (x %*% thetas)

    deriv_thetas <- (-2 / num_explanatory) * t(x) %*% (y - y_pred)

    #Updating thetas based on derivative of slope
    thetas <- thetas - (learning_rate * deriv_thetas)

  }


  ### Compute coefficients by gradient descent
  ### Return a data frame of the same form as in the `multiple_linear_regression`

  return(thetas)

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


#' Loss/Cost Function for multivariate gradient descent
#'

loss_function <- function(X, y, m, theta){

  #Calculate cost
  sum((X %*% theta - y) ^ 2) / (2 * m)

}






