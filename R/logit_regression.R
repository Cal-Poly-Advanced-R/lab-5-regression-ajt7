#' Implements logit regression
#'
#' @param dat A data frame
#' @param binary The name of a binary response variable in the data frame (unquoted)
#' @param min optional, a parameter of a threshold value to round logistic value to (unquoted)
#'
#' @return A data frame of coefficients and respective binary result
#'
#' @import dplyr
#'
#' @export
function(data, binary, min = 0.5){


  checktest <- data |> pull({{binary}})
  stopifnot(checktest %in% c(0,1))


  reg <- multiple_linear_regression(data, {{binary}})
  prediction <- predict_from_coefs(data, {{binary}}, reg)

  logistic_value <- 1/(1+exp(-(prediction |> pull(1) ) ) )

  binary_predict <- ifelse(logistic_value >= min, 1, 0 )

  logistic_return <- data.frame(logistic_value, binary_predict)

  logistic_return
}
