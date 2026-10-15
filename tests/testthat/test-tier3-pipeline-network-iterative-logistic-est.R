test_that("network iterative Poisson works with estimated interaction", {
  skip_on_cran()
  skip_if_not_installed("spatstat.linnet")
  skip_if_not(reticulate::py_available(initialize = TRUE), "Python is not available.")
  skip_if_not(reticulate::py_module_available("xgboost"), "XGBoost is not available.")
  set.seed(123)

  fixture <- make_network_integration_fixture(n = 30L)

  data <- prepare_gbpp_network(
    point_pattern = fixture$point_pattern,
    covariates = fixture$covariates,
    nd = 20L
  )

  data <- attach_network_interaction_integral(
    data = data,
    point_pattern = fixture$point_pattern,
    rmax = 0.005,
    nr = 32L
  )

  expect_s3_class(data, "gbpp_data")
  expect_identical(data$type, "network")

  expect_equal(
    length(data$interaction_integral),
    nrow(data$X)
  )

  expect_true(
    all(is.finite(data$interaction_integral))
  )

  model <- fit_gbpp_xgboost_iterative_logistic(
    data = data,
    params = list(
      eta = 0.1,
      max_depth = 2L
    ),
    num_boost_round = 5L
  )

  expect_s3_class(model, "gbpp_xgboost")

  predictions <- predict(
    model,
    newdata = data$X
  )

  expect_type(predictions, "double")
  expect_length(predictions, nrow(data$X))
  expect_true(all(is.finite(predictions)))
})
