test_that("CatBoost backend fits and predicts all GBPP objectives", {

  data <- new_gbpp_data(
    X = matrix(
      c(
        0.0, 0.0,
        0.0, 1.0,
        1.0, 0.0,
        1.0, 1.0,
        0.5, 0.5,
        0.2, 0.8,
        0.8, 0.2,
        0.3, 0.7
      ),
      ncol = 2,
      byrow = TRUE
    ),
    y = c(1, 0, 1, 0, 1, 0, 0, 1),
    locations = matrix(
      c(
        0.0, 0.0,
        0.0, 1.0,
        1.0, 0.0,
        1.0, 1.0,
        0.5, 0.5,
        0.2, 0.8,
        0.8, 0.2,
        0.3, 0.7
      ),
      ncol = 2,
      byrow = TRUE
    ),
    quadrature_weight = rep(1 / 8, 8),
    dummy_intensity = rep(2, 8),
    statistical_weight = rep(1, 8),
    interaction_integral = rep(0.01, 8),
    window = NULL,
    type = "planar"
  )

  params <- list(
    depth = 2L,
    learning_rate = 0.05,
    verbose = FALSE,
    random_seed = 123L,
    thread_count = 1L,
    eval_metric = "RMSE"
  )

  models <- list(
    poisson = fit_gbpp_catboost(
      data,
      params = params,
      nrounds = 20L
    ),

    weighted_poisson = fit_gbpp_catboost_weighted_poisson(
      data,
      params = params,
      nrounds = 20L
    ),

    logistic = fit_gbpp_catboost_logistic(
      data,
      params = params,
      nrounds = 20L
    ),

    weighted_logistic = fit_gbpp_catboost_weighted_logistic(
      data,
      params = params,
      nrounds = 20L
    ),

    iterative_poisson = fit_gbpp_catboost_iterative_poisson(
      data,
      params = params,
      nrounds = 20L
    ),

    iterative_logistic = fit_gbpp_catboost_iterative_logistic(
      data,
      params = params,
      nrounds = 20L
    )
  )

  for (name in names(models)) {

    model <- models[[name]]

    expect_true(
      inherits(model, "gbpp_catboost"),
      info = paste("Model:", name)
    )

    prediction <- predict(
      model,
      newdata = data$X
    )

    expect_type(
      prediction,
      "double"
    )

    expect_length(
      prediction,
      nrow(data$X)
    )

    expect_true(
      all(is.finite(prediction))
    )
  }
})
