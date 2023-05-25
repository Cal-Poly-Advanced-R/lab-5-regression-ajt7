#' Creates a data frame with residuals
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param prediction A data frame with the predictions
#' @return A data frame with the residuals
#'
#' @import dplyr
#' @export

residual_calculate <- function(data, response, prediction){
  stopifnot(nrow(data) == nrow(prediction))
  response_data <- data |> select({{response}})
  difference <- response_data[1] - prediction[1]
  difference
}
#' Creates a histogram displaying the residuals
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param prediction A data frame with the predictions
#' @return A histogram with the residuals
#'
#' @import ggplot2
#' @export
residual_viz <- function(data, response, prediction){
  residuals <- residual_calculate(data, {{response}}, prediction)
  ggplot2::ggplot(residuals, ggplot2::aes(x = Intercept)) + ggplot2::geom_histogram() + ggplot2::labs(x = "Residuals", y = "Count")
}
