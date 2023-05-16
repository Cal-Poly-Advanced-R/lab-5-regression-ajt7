#' Computes predicted values given coefficients
#'
#' This function takes a data frame of coefficients in the form outputted by
#' functions like \code{multiple_linear_regression} or \code{ridge_regression}.
#'
#' It calculates the predicted values of a response variable from these coefficients.
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param coefs A data frame of coefficient estimates
#'
#' @return A data frame of true and predicted values
#'
#' @import dplyr
#'
#' @export
predict_from_coefs <- function(dat, response, coefs){
stopifnot(is.data.frame(dat))
stopifnot(is.data.frame(coefs))
intercept <- coefs[1,1]
coefs_without_intercept <- coefs[,-1] |> t()
coefs_without_intercept <- as.data.frame(coefs_without_intercept)

dat <- as.data.frame(dat |> select(-{{response}}) |> t())

combined_set <- cbind(dat, coefs_without_intercept)

variable_multiply <- combined_set |>
  mutate(across(1:(length(combined_set)-1), function(x) x*combined_set[length(combined_set)])) |>
  select(-V1)
t_variable_multiply <- as.data.frame(variable_multiply |> t())

returned_coefficient_prediction <- t_variable_multiply |> mutate(Predicted_Value = rowSums(across(everything()))) |>
  select(Predicted_Value)
returned_coefficient_prediction
}

