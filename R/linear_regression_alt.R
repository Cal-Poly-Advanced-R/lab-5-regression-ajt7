#' Implements simple linear regression by gradient descent
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#' @param iterations Number of iterations for gradient descent to run
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
slr_gd <- function(dat, response, explanatory, iterations = 30000) {

  x <- dat %>% pull({{explanatory}})
  y <- dat %>% pull({{response}})

  x <- scale(x)


  explan_name <- dat %>%
    select({{explanatory}}) %>%
    names()

  m <- 0
  c <- 0
  n <- length(x)
  L <- 0.005

  for(i in 1:iterations) {

    y_pred <- (m * x) + c

    Deriv_m = (-2 / n) * sum(x * (y - y_pred))
    Deriv_c = (-2 / n) * sum(y - y_pred)



    m <- m - L * Deriv_m
    c <- c - L * Deriv_c

    if (all((Deriv_m < 0.0000001)) & all((Deriv_c < 0.000000)) ) {
      break
    }

  }

  results <- data.frame(
    Intercept = c,
    Coefficient = m
    )

  ### Compute coefficients by gradient descent
  ### Return a data frame of the same form as in the `simple_linear_regression`
  names(results)[2] <- explan_name


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
#' @param learning_rate The learning rate controls how quickly the model is adapted to the problem
#' @param iterations Number of iterations for gradient descent to run
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export
mlr_gd <- function(dat, response, learning_rate = 0.005, iterations = 30000) {

  y <- dat %>% pull({{response}})
  x <- dat %>% select(-{{response}})

  x <- x %>%
    mutate_all(scale)

  #Names of explanatory variables
  explan_name <- dat %>%
    select(-{{response}}) %>%
    names() %>%

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
    step_size <- learning_rate * deriv_thetas

    thetas <- thetas - step_size

    if (all(step_size < 0.0000001)) {
      break
    }

  }

  ### Compute coefficients by gradient descent
  ### Return a data frame of the same form as in the `multiple_linear_regression`
  results <- as.data.frame(thetas)
  results <- results %>%
    rownames_to_column("Var") %>%
    pivot_wider(names_from = Var, values_from = V1) %>%
    rename("Intercept" = "intercept")

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
#' @import tidyverse
#'
#'@export
mlr_qr <- function(dat, response) {

  y <- dat %>% pull({{response}})
  x <- dat %>% select(-{{response}})

  x <- x %>%
    mutate(intercept = 1, .before = 1)

  x <- as.matrix(x)
  y <- as.matrix(y)

  #Names
  explan_name <- dat %>%
    select(-{{response}}) %>%
    names()

  QR_X <- qr(x)

  Q <- qr.Q(QR_X)
  R <- qr.R(QR_X)

  results <- solve(R) %*% t(Q) %*% y

  results <- as.data.frame(results)
  results <- results %>%
    rownames_to_column("Var") %>%
    pivot_wider(names_from = Var, values_from = V1) %>%
    rename("Intercept" = "intercept")

  ### Compute coefficients by QR decomposition
  ### Return a data frame of the same form as in the `multiple_linear_regression`

  return(results)

}

