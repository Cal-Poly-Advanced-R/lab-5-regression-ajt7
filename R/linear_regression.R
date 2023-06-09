#' Implements simple linear regression by hand
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#' @param method The method used to compute the coefficients (NULL, "qr", "gradientdescent")
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom tibble rownames_to_column
#'
#' @export
simple_linear_regression <- function(dat, response, explanatory, method = NULL){

  x <- dat %>% pull({{explanatory}})
  y <- dat %>% pull({{response}})

  explan_name <- dat %>%
    select({{explanatory}}) %>%
    names()

  x_bar <- mean(x)
  y_bar <- mean(y)

  ### Edit code after here


  sd_x <- sd(x)
  sd_y <- sd(y)

  beta_1 <- sum((x - x_bar) * (y - y_bar)) / sum((x - x_bar)^2)
  beta_0 <- y_bar - beta_1 * x_bar


  # sd_x <- sd(x)
  # sd_y <- sd(y)
  #
  # n <- length(x)
  # x_bar <- mean(x)
  # y_bar <- mean(y)
  #
  # X <- cbind(1, x - x_bar)
  # Y <- y - y_bar
  #
  # beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  # beta_0 <- beta[1]
  # beta_1 <- beta[2]

  ### Stop editing

  results <- tibble::tibble(
    Intercept = beta_0,
    Slope = beta_1
  )

  names(results)[2] <- explan_name

  return(results)

}


#' Implements linear regression with many predictors by hand
#'
#' This function computes coefficients for multiple regression by hand.
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param method The method used to compute the coefficients (NULL, "qr", "gradientdescent")
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export
multiple_linear_regression <- function(dat, response, method = NULL) {

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
  xtx <- x_t %*% x

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
    rename("Intercept" = "intercept")

  return(results)

}
