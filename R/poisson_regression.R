#' Implements Poisson regression with many predictors by gradient descent
#'
#' This function computes coefficients for poisson regression by gradient descent
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
poisson_regression <- function(dat, response, learning_rate = 0.005, iterations = 30000) {

  dat <- dat %>%
    mutate_all(scale)

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

    deriv_thetas <- colMeans(t(x) * y - (exp(y_pred) * x))

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


