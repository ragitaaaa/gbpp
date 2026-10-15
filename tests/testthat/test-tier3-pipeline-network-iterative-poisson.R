test_that("network iterative Poisson integrates with XGBoost", {
  skip_on_cran()
  skip_if_not_installed("spatstat.linnet")
  skip_if_not(
    reticulate::py_available(),
    message = "Python is not available."
  )

  fixture <- make_network_integration_fixture()

  data <- prepare_gbpp_network(
    point_pattern = fixture$point_pattern,
    covariates = fixture$covariates,
    nd = 20L
  )

  # Supplied interaction integral for this integration test.
  # Network estimation is intentionally handled separately.
  data$interaction_integral <- rep(
    0.01,
    length(data$y)
  )

  expect_s3_class(data, "gbpp_data")
  expect_identical(data$type, "network")

  expect_equal(
    length(data$interaction_integral),
    length(data$y)
  )

  expect_true(
    all(is.finite(data$interaction_integral))
  )

  model <- fit_gbpp_xgboost_iterative_poisson(
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
