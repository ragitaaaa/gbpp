test_that("network Logistic data integrates with XGBoost", {
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
    length(data$dummy_intensity),
    length(data$y)
  )

  expect_true(all(is.finite(data$dummy_intensity)))
  expect_true(all(data$dummy_intensity > 0))

  model <- fit_gbpp_xgboost_logistic(
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
