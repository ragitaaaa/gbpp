make_network_integration_fixture <- function() {
  data(simplenet, package = "spatstat.data")

  point_pattern <- spatstat.linnet::runiflpp(
    n = 8,
    L = simplenet
  )

  network <- spatstat.linnet::as.linnet(point_pattern)

  covariate_1 <- spatstat.linnet::as.linim(
    function(x, y) x + y,
    L = network,
    delta = 0.1
  )

  covariate_2 <- spatstat.linnet::as.linim(
    function(x, y) x^2 + y^2,
    L = network,
    delta = 0.1
  )

  list(
    point_pattern = point_pattern,
    covariates = list(
      x_sum = covariate_1,
      x2_y2 = covariate_2
    )
  )
}


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
