test_that("LightGBM prediction works for all GBPP objectives", {
  skip_on_cran()
  skip_if_not(reticulate::py_available(initialize = TRUE), "Python is not available.")
  skip_if_not(reticulate::py_module_available("lightgbm"), "LightGBM Python package is not available.")

  X <- matrix(
    c(
      0, 1,
      1, 0,
      2, 1,
      3, 0,
      4, 1,
      5, 0,
      6, 1,
      7, 0
    ),
    ncol = 2,
    byrow = TRUE
  )

  colnames(X) <- c("x1", "x2")

  y <- c(1, 0, 1, 0, 1, 0, 0, 1)

  quadrature_weight <- rep(1 / nrow(X), nrow(X))

  statistical_weight <- c(
    1.0, 0.9, 1.1, 1.0,
    0.8, 1.2, 1.0, 0.95
  )

  dummy_intensity <- rep(2, nrow(X))

  interaction_integral <- rep(0.01, nrow(X))

  newdata <- matrix(
    c(
      1.5, 0,
      2.5, 1,
      6.5, 0
    ),
    ncol = 2,
    byrow = TRUE
  )

  colnames(newdata) <- c("x1", "x2")

  data_poisson <- new_gbpp_data(
    X = X,
    y = y,
    locations = X,
    quadrature_weight = quadrature_weight,
    dummy_intensity = NULL,
    statistical_weight = NULL,
    interaction_integral = NULL,
    window = NULL,
    type = "planar"
  )

  data_weighted_poisson <- new_gbpp_data(
    X = X,
    y = y,
    locations = X,
    quadrature_weight = quadrature_weight,
    dummy_intensity = NULL,
    statistical_weight = statistical_weight,
    interaction_integral = NULL,
    window = NULL,
    type = "planar"
  )

  data_logistic <- new_gbpp_data(
    X = X,
    y = y,
    locations = X,
    quadrature_weight = quadrature_weight,
    dummy_intensity = dummy_intensity,
    statistical_weight = NULL,
    interaction_integral = NULL,
    window = NULL,
    type = "planar"
  )

  data_weighted_logistic <- new_gbpp_data(
    X = X,
    y = y,
    locations = X,
    quadrature_weight = quadrature_weight,
    dummy_intensity = dummy_intensity,
    statistical_weight = statistical_weight,
    interaction_integral = NULL,
    window = NULL,
    type = "planar"
  )

  data_iterative_poisson <- new_gbpp_data(
    X = X,
    y = y,
    locations = X,
    quadrature_weight = quadrature_weight,
    dummy_intensity = NULL,
    statistical_weight = NULL,
    interaction_integral = interaction_integral,
    window = NULL,
    type = "planar"
  )

  data_iterative_logistic <- new_gbpp_data(
    X = X,
    y = y,
    locations = X,
    quadrature_weight = quadrature_weight,
    dummy_intensity = dummy_intensity,
    statistical_weight = NULL,
    interaction_integral = interaction_integral,
    window = NULL,
    type = "planar"
  )

  fit_params <- list(
    min_data_in_leaf = 1L,
    feature_pre_filter = FALSE
  )

  model_poisson <- fit_gbpp_lightgbm(
    data = data_poisson,
    params = fit_params,
    num_boost_round = 5L
  )

  model_weighted_poisson <- fit_gbpp_lightgbm_weighted_poisson(
    data = data_weighted_poisson,
    params = fit_params,
    num_boost_round = 5L
  )

  model_logistic <- fit_gbpp_lightgbm_logistic(
    data = data_logistic,
    params = fit_params,
    num_boost_round = 5L
  )

  model_weighted_logistic <- fit_gbpp_lightgbm_weighted_logistic(
    data = data_weighted_logistic,
    params = fit_params,
    num_boost_round = 5L
  )

  model_iterative_poisson <- fit_gbpp_lightgbm_iterative_poisson(
    data = data_iterative_poisson,
    params = fit_params,
    num_boost_round = 5L
  )

  model_iterative_logistic <- fit_gbpp_lightgbm_iterative_logistic(
    data = data_iterative_logistic,
    params = fit_params,
    num_boost_round = 5L
  )

  models <- list(
    poisson = model_poisson,
    weighted_poisson = model_weighted_poisson,
    logistic = model_logistic,
    weighted_logistic = model_weighted_logistic,
    iterative_poisson = model_iterative_poisson,
    iterative_logistic = model_iterative_logistic
  )

  for (name in names(models)) {

    prediction <- predict(
      models[[name]],
      newdata = newdata
    )

    expect_type(prediction, "double")
    expect_length(
      prediction,
      nrow(newdata)
    )

    expect_true(
      all(is.finite(prediction))
    )
  }
})

test_that("LightGBM model can predict from gbpp_data features", {

  W <- spatstat.geom::owin(
    xrange = c(0, 1),
    yrange = c(0, 1)
  )

  pp <- spatstat.random::runifpoint(
    n = 10,
    win = W
  )

  covariate <- spatstat.geom::as.im(
    function(x, y) x + y,
    W = W
  )

  data <- prepare_gbpp(
    point_pattern = pp,
    covariates = list(
      elevation = covariate
    ),
    nd = 10L
  )

  model <- fit_gbpp_lightgbm(
    data = data,
    params = list(
      learning_rate = 0.1,
      max_depth = 2L,
      num_leaves = 4L,
      min_data_in_leaf = 1L,
      feature_pre_filter = FALSE
    ),
    num_boost_round = 3L
  )

  prediction <- predict(
    model,
    data$X
  )

  expect_type(prediction, "double")
  expect_length(prediction, nrow(data$X))
  expect_true(all(is.finite(prediction)))
})
