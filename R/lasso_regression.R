#' Implements lasso linear regression by gradient descent
#'
#' This function computes coefficients for multiple regression by lasso l1 penalization and gradient descent
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param learning_rate The learning rate controls how quickly the model is adapted to the problem (default 0.05)
#' @param lambda Tuning Parameter that controls the bias-variance trade off (default 0.8)
#' @param iterations Number of iterations for gradient descent to run (default 50000)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export

lasso_gd <- function(dat, response, learning_rate = 0.001, lambda = 0.5, iterations = 10000) {

  y <- dat %>% pull({{response}})
  x <- dat %>% select(-{{response}})

  #Scaling explanatory
  x <- x %>%
    mutate_all(scale)

  #Names of explanatory variables
  explan_name <- dat %>%
    select(-{{response}}) %>%
    names()

  explan_name <- c("Intercept", explan_name)

  #Adding intercept column
  x <- x %>%
    mutate(intercept = 1, .before = 1)

  #Getting number of explanatory variables
  num_explanatory <- ncol(x)

  #Converting to matrices
  x <- as.matrix(x)
  y <- as.matrix(y)

  #Initialize a of vector of coefficient estimates (zeros)
  thetas <- rep(1, num_explanatory)

  for(i in 1:iterations) {

    #Predicting fitted values each iteration
    y_pred <- (x %*% thetas)

    #If weight/theta is greater than zero subtract, else add
    deriv_thetas <- lasso_loss(num_explanatory, x, y, y_pred, {{lambda}}, thetas)

    #Updating thetas based on derivative of slope
    step_size <- learning_rate * deriv_thetas

    thetas <- thetas - step_size

    if (all(step_size < 0.0000001)) {
      break
    }

  }

  #Returning data frame of named coefficients
  results <- as.data.frame(thetas)
  results <- results %>%
    tibble::add_column(Var = explan_name, .before = "thetas") %>%
    pivot_wider(names_from = Var, values_from = thetas)

  return(results)

}

#' Computes derivate of MSE loss function with additional l1 regularization term
#'
#' @param num_explanatory Number of explanatory variables in given data set
#' @param x Values of explanatory variables
#' @param y Values of response variable from data set
#' @param y_pred Fitted values predicted from current thetas
#' @param lambda Tuning Parameter that controls the bias-variance trade off
#' @param thetas Coefficients/Weights of multivariate regression
#'
#' @return A vector of the derivatives of the MSE loss function

lasso_loss <- function(num_explanatory, x, y, y_pred, lambda, thetas) {

  deriv_thetas <- rep(1, num_explanatory)

  for (w in 1:length(thetas)) {

    if(w > 0) {

           deriv_thetas[w] <- as.data.frame((-2 / num_explanatory) * (t(x) %*% (y - y_pred)) + lambda)[w , ]

    } else {

           deriv_thetas[w] <- as.data.frame((-2 / num_explanatory) * (t(x) %*% (y - y_pred)) - lambda)[w , ]

    }

  }

  return(deriv_thetas)

}
