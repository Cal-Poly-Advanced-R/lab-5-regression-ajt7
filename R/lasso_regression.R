#' Implements lasso linear regression by gradient descent
#'
#' This function computes coefficients for multiple regression by lasso l1 penalization and gradient descent
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'

lasso_gd <- function(dat, response, lambda = 0.1) {

  #Changed to if, else
  ifelse(w > 0,

         #True
         results <- (-2 / num_explanatory) * (t(x) %*% (y - y_pred) + lambda),
         #False
         results <- (-2 / num_explanatory) * (t(x) %*% (y - y_pred) - lambda)

         )

  step_size <- learning_rate * deriv_thetas

  thetas <- thetas - step_size

}

lasso_loss_function <- function(y, y_predictions, x, number_explanatory_vars) {

  results <- ((-2 / num_explanatory) * t(x) %*% (y - y_pred))

}
