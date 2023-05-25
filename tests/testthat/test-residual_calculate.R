test_that("residual-calculate is correct", {
  my_result <- simple_linear_regression(mtcars, mpg, hp)
  my_predictions <- predict_from_coefs(mtcars, mpg, my_result)
  my_residuals <- residual_calculate(mtcars, mpg, my_predictions)
  expect_equal(class(my_residuals), "data.frame")
})
