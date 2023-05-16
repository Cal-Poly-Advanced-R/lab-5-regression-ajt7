#' Implements ridge regression with many predictors
#'
#' This function computes coefficients for ridge regression
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
#'
ridge_regression <- function(dat, response, lambda) {

  #Converting the response variable and the predictor variables into matrices and splitting them
  response_col <- as.matrix(as.numeric(dat[[deparse(substitute(response))]]), ncol = 1)#ChatGPT referenced to help change the response from needing quotations
  predictors <- as.matrix(select(dat, -{{response}}))

  #Create the results data frame
  results <- data.frame(matrix(0, ncol = ncol(predictors) + 2, nrow = length(lambda)))

  #Set the column and row names within the results data frame
  colnames(results)[1] <- "Intercept"
  x <- ncol(predictors) + 1
  colnames(results)[2:x] <- colnames(predictors)
  colnames(results)[ncol(results)] <- "lambda"


  #For-loop (oops sorry Ms. Bodwin)
  for (i in 1:length(lambda)) {
    ridge_penalty <- lambda[i] * diag(ncol(predictors))

    coefficients <- solve(t(predictors) %*% predictors + ridge_penalty) %*% t(predictors) %*% response_col

    results[i, 1] <- coefficients[1]  #Assigning intercept value

    results[i, 2:(ncol(results) - 1)] <- coefficients[2:(ncol(coefficients))]  #Assigning remaining coefficients

    results[i, ncol(results)] <- lambda[i]  #Assigning lambda value
  }

  return(results)
}

#' Determines the best penalty term from a set of options
#'
#' This function uses a randomly chosen test and training set
#'
#' No interaction terms are included.
#'
#'
#' @param train_dat A data frame to construct the model from
#' @param test_dat A data frame to test the model on
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of penalty terms and resulting errors
#'
#' @import dplyr
#'
#' @export
find_best_lambda <- function(train_dat, test_dat, response, lambdas) {

  lambda_errors <- data.frame(lambda = lambdas, error = numeric(length(lambdas)))

  for (i in seq(lambdas)) {

    lambda <- lambdas[i]

    ridge_model <- ridge_regression(dat = train_dat, mpg, lambda)

    predicted_values <- predict(as.matrix(ridge_model), newdata = as.matrix(test_dat))


  }

  ### lambda_errors should be a data frame with two columns: "lambda" and "error"
  ### For each lambda, you should record the resulting Sum of Squared error
  ### (i.e., the predicted value minus the real value squared) from prediction
  ### on the test dataset.

  return(lambda_errors)
}


train_dat = mtcars
test_dat = mtcars
