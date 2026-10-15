test_that("explain_gbpp public generic works correctly", {
  skip_on_cran()
  skip_if_not(reticulate::py_available(initialize = TRUE), "Python not available")
  
  configure_gbpp_python()
  
  data <- new_gbpp_data(
    X = data.frame(
      x = c(0.5, 1.5, 2.5, 3.5),
      y = c(0.5, 1.5, 2.5, 3.5),
      v1 = c(1, 3, 5, 7),
      v2 = c(2, 4, 6, 8)
    ),
    y = c(1, 1, 0, 0),
    locations = data.frame(
      x = c(0.5, 1.5, 2.5, 3.5),
      y = c(0.5, 1.5, 2.5, 3.5)
    ),
    quadrature_weight = rep(1, 4)
  )

  model <- fit_gbpp_xgboost(data, params = list(eta = 0.1, max_depth = 2L), nrounds = 2L)
  
  # Test default (quadrature)
  exp_quad <- explain_gbpp(model, data)
  expect_s3_class(exp_quad, "gbpp_explanation")
  expect_equal(nrow(exp_quad$shap), 2)
  expect_equal(nrow(exp_quad$data), 2)
  expect_equal(exp_quad$data$v1, c(5, 7))
  
  # Test events
  exp_events <- explain_gbpp(model, data, background = "events")
  expect_equal(nrow(exp_events$shap), 2)
  expect_equal(nrow(exp_events$data), 2)
  expect_equal(exp_events$data$v1, c(1, 3))
  
  # Test all
  exp_all <- explain_gbpp(model, data, background = "all")
  expect_equal(nrow(exp_all$shap), 4)
  expect_equal(nrow(exp_all$data), 4)
  expect_equal(exp_all$data$v1, c(1, 3, 5, 7))
})
