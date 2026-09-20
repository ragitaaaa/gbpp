test_that("attached interaction integral supports iterative Poisson XGBoost", {
  skip_if_not_installed("spatstat.explore")

  data(lansing, package = "spatstat.data")

  n <- 20L

  X <- matrix(
    rnorm(n * 2),
    ncol = 2
  )

  y <- c(
    rep(1, 5),
    rep(0, 15)
  )

  data_object <- new_gbpp_data(
    X = X,
    y = y,
    locations = matrix(
      runif(n * 2),
      ncol = 2
    ),
    quadrature_weight = rep(1 / n, n),
    dummy_intensity = rep(1, n),
    statistical_weight = rep(1, n),
    interaction_integral = NULL
  )

  data_object <- attach_interaction_integral(
    data = data_object,
    point_pattern = lansing,
    rmax = 0.2,
    correction = "isotropic"
  )

  expect_length(
    data_object$interaction_integral,
    length(data_object$y)
  )

  expect_true(
    all(is.finite(data_object$interaction_integral))
  )

  fit <- fit_gbpp_xgboost_iterative_poisson(
    data = data_object,
    params = list(
      eta = 0.05,
      max_depth = 2L,
      verbosity = 0L
    ),
    num_boost_round = 5L
  )

  expect_s3_class(
    fit,
    "xgboost.core.Booster"
  )

  expect_true(
    reticulate::py_has_attr(
      fit,
      "predict"
    )
  )
})


test_that("attached interaction integral supports iterative Logistic XGBoost", {
  skip_if_not_installed("spatstat.explore")

  data(lansing, package = "spatstat.data")

  n <- 20L

  X <- matrix(
    rnorm(n * 2),
    ncol = 2
  )

  y <- c(
    rep(1, 5),
    rep(0, 15)
  )

  data_object <- new_gbpp_data(
    X = X,
    y = y,
    locations = matrix(
      runif(n * 2),
      ncol = 2
    ),
    quadrature_weight = rep(1 / n, n),
    dummy_intensity = rep(1, n),
    statistical_weight = rep(1, n),
    interaction_integral = NULL
  )

  data_object <- attach_interaction_integral(
    data = data_object,
    point_pattern = lansing,
    rmax = 0.2,
    correction = "isotropic"
  )

  expect_length(
    data_object$interaction_integral,
    length(data_object$y)
  )

  expect_true(
    all(is.finite(data_object$interaction_integral))
  )

  fit <- fit_gbpp_xgboost_iterative_logistic(
    data = data_object,
    params = list(
      eta = 0.05,
      max_depth = 2L,
      verbosity = 0L
    ),
    num_boost_round = 5L
  )

  expect_s3_class(
    fit,
    "xgboost.core.Booster"
  )

  expect_true(
    reticulate::py_has_attr(
      fit,
      "predict"
    )
  )
})
