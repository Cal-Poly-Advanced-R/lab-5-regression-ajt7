test_that("residual-calculate is correct", {
  my_result <- simple_linear_regression(mtcars, mpg, hp)
  my_predictions <- predict_from_coefs(mtcars, mpg, my_result)
  check_viz <- residual_viz(mtcars, mpg, my_predictions)
  expect_equal(length(class(residual_viz(mtcars, mpg, my_predictions))), 2)
})
