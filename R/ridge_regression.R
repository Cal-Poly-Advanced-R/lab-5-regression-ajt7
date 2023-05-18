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
#' @import purrr
#'
#'
#' @export
#'


ridge_regression <- function(dat, response, lambda) {

  results <- map(lambda, ~find_one_lambda(dat, {{response}}, .x), .id = "lambda")

  results <- bind_rows(results)

  return(results)
}


library(purrr)
library(tidyverse)
library(dplyr)
x = ridge_regression(mtcars, mpg, c(0.1, 0.2, 0.3))

#' Implements ridge regression with many predictors for one value of lambda (a helper function)
#'
#' This function computes coefficients for ridge regression with one lambda
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A singular value to try as a penalty term
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#' @import tidyverse
#'
#'

find_one_lambda <- function(dat, response, lambda) {

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


  #Transpose X, find XtX
  x_t <- t(x)
  xtx <- x_t %*% x + (lambda * diag(x))

  #Inverse
  inverse_xtx <- solve(xtx)

  #Find XtY
  xty <- x_t %*% y

  #Find coefficients
  ### This should be a data frame, with columns named
  ### "Intercept" and the same variable names as dat.
  results <- inverse_xtx %*% xty
  results <- as.data.frame(results)
  results <- results %>%
    rownames_to_column("Var") %>%
    pivot_wider(names_from = Var, values_from = V1) %>%
    rename("Intercept" = "intercept") %>%
    mutate("lambda" = lambda)

  results <- as.data.frame(results)

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



  ### lambda_errors should be a data frame with two columns: "lambda" and "error"
  ### For each lambda, you should record the resulting Sum of Squared error
  ### (i.e., the predicted value minus the real value squared) from prediction
  ### on the test dataset.

  return(lambda_errors)
}

#' Computes ridge regression on the training and testing data sets
#'
#' Returns coeffecients
#'
#' No interaction terms are included.
#'
#'
#' @param train_dat A data frame to construct the model from
#' @param test_dat A data frame to test the model on
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return Two data frames of coeffecients - one for test one for train
#'
#'

get_betas <- function(train_dat, test_dat, response, lambda){

  train_results <- ridge_regression(train_dat, response, lambda)
  train_results <- subset(train_results, select = -c(Intercept, lambda))

  test_results <- ridge_regression(test_dat, response, lambda)
  test_results <- subset(test_results, select = -c(Intercept, lambda))

  return(train_results)
  return(test_results)
}

#' Get predictions from the beta values
#'
#' Returns
#'
#' No interaction terms are included.
#'
#'
#' @param train_dat A data frame to construct the model from
#' @param test_dat A data frame to test the model on
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return matrix y with predicted values
#'
#'

get_predictions <- function(train_dat, test_dat, train_results, test_results){

  x_train <- train_dat %>% select(-{{response}})
  x_test <- test_dat %>% select(-{{response}})

  x_train <- as.matrix(x_train)
  x_test <- as.matrix(x_test)

  train_results <- as.matrix(t(train_results))
  test_results <- as.matrix(t(test_results))

  y_train <- x_train %*% train_results
  y_test <- x_test %*% test_results

  return(y_train)
  return(y_test)

}

#'
#'
#' Returns
#'
#' No interaction terms are included.
#'
#'
#' @param train_dat A data frame to construct the model from
#' @param test_dat A data frame to test the model on
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return matrix y with predicted values
#'
#'

get_test_error <- function(){


}




ridge_results <- ridge_regression(mtcars, mpg, 1)
ridge_results <- subset(ridge_results, select = -c(Intercept,lambda))
ridge_results <- as.matrix(t(ridge_results))

x_design <- mtcars %>% select(-mpg)
x_design <- as.matrix(x_design)

y <- x_design %*% ridge_results



