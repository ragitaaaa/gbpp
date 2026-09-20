test_that("network Poisson data integrates with XGBoost", {
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

  expect_s3_class(data, "gbpp_data")
  expect_identical(data$type, "network")

  expect_equal(
    nrow(data$X),
    length(data$y)
  )

  expect_equal(
    ncol(data$X),
    2L
  )

  expect_equal(
    length(data$quadrature_weight),
    length(data$y)
  )

  expect_equal(
    sum(data$y),
    8L
  )

  expect_true(all(is.finite(data$quadrature_weight)))
  expect_true(all(data$quadrature_weight > 0))

  model <- fit_gbpp_xgboost(
    data = data,
    params = list(
      eta = 0.1,
      max_depth = 2L
    ),
    nrounds = 5L
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
