test_that("lasso regression by gradient descent is (approximately) correct", {

  my_result <- mtcars %>%
    lasso_gd(., mpg, learning_rate = 0.05, lambda = 0.5, iterations = 50000)

  scaled_mtcars <- mtcars %>%
    mutate_all(scale)

  x <- as.matrix(scaled_mtcars[ , -1])
  y <- scaled_mtcars[ , 1]

  best_model <- glmnet::glmnet(x, y, alpha = 1, lambda = 0.5)
  mass_result <- coef(best_model)
  mass_result <- as.matrix(mass_result)
  mass_result <- as.data.frame(mass_result)
  mass_result <- mass_result %>%
    rownames_to_column("Var") %>%
    pivot_wider(names_from = Var, values_from = s0)

  expect_equal(mass_result$cyl, my_result$cyl,
               tolerance = 0.05, scale = abs(my_result$hp))

})
