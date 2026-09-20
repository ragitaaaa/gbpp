test_that("XGBoost Poisson objective matches the GBPP reference objective", {

  skip_if_not(
    reticulate::py_available(),
    message = "Python is not available."
  )

  configure_gbpp_python()

  data <- new_gbpp_data(
    X = data.frame(
      x1 = c(0, 1, 2, 3),
      x2 = c(1, 1, 2, 2)
    ),
    y = c(1, 0, 1, 0),
    locations = data.frame(
      x = c(0, 1, 2, 3),
      y = c(0, 1, 0, 1)
    ),
    quadrature_weight = c(
      0.25, 0.50, 0.75, 1.00
    )
  )

  prediction <- c(
    -0.5, 0, 0.5, 1
  )

  expected <- gbpp_objective(
    data = data,
    prediction = prediction,
    objective = "poisson"
  )

  objective <- make_xgboost_poisson_objective(
    data$quadrature_weight
  )

  configure_gbpp_python()

  xgb <- reticulate::import("xgboost")

  dtrain <- xgb$DMatrix(
    data = as.matrix(data$X),
    label = data$y
  )

  result <- objective(
    reticulate::r_to_py(prediction),
    dtrain
  )

  gradient <- as.numeric(
    reticulate::py_to_r(result[[1]])
  )

  hessian <- as.numeric(
    reticulate::py_to_r(result[[2]])
  )

  expect_equal(
    gradient,
    expected$gradient,
    tolerance = 1e-12
  )

  expect_equal(
    hessian,
    expected$hessian,
    tolerance = 1e-12
  )
})


test_that("XGBoost can fit a GBPP Poisson model", {

  skip_if_not(
    reticulate::py_available(),
    message = "Python is not available."
  )

  configure_gbpp_python()

  data <- new_gbpp_data(
    X = data.frame(
      x1 = c(0, 1, 2, 3, 4, 5),
      x2 = c(1, 1, 2, 2, 3, 3)
    ),
    y = c(1, 0, 1, 0, 1, 0),
    locations = data.frame(
      x = c(0, 1, 2, 3, 4, 5),
      y = c(0, 1, 0, 1, 0, 1)
    ),
    quadrature_weight = rep(0.5, 6)
  )

  model <- fit_gbpp_xgboost(
    data = data,
    params = list(
      eta = 0.1,
      max_depth = 2L
    ),
    nrounds = 5L
  )

  expect_true(
    reticulate::py_has_attr(model, "predict")
  )

  dtest <- reticulate::import("xgboost")$DMatrix(
    data = as.matrix(data$X)
  )

  dtest$feature_names <- colnames(data$X)
  predictions <- model$predict(dtest)
  predictions <- reticulate::py_to_r(predictions)

  expect_length(
    predictions,
    nrow(data$X)
  )

  expect_true(
    all(is.finite(predictions))
  )
})

test_that(
  "XGBoost weighted Poisson objective matches the GBPP reference objective",
  {

    skip_if_not(
      reticulate::py_available(),
      message = "Python is not available."
    )

    configure_gbpp_python()

    data <- new_gbpp_data(
      X = data.frame(
        x1 = c(0, 1, 2, 3),
        x2 = c(1, 1, 2, 2)
      ),
      y = c(1, 0, 1, 0),
      locations = data.frame(
        x = c(0, 1, 2, 3),
        y = c(0, 1, 0, 1)
      ),
      quadrature_weight = c(
        0.25, 0.50, 0.75, 1.00
      ),
      statistical_weight = c(
        1.0, 0.5, 2.0, 1.5
      )
    )

    prediction <- c(
      -0.5, 0, 0.5, 1
    )

    expected <- gbpp_objective(
      data = data,
      prediction = prediction,
      objective = "weighted_poisson"
    )

    objective <- make_xgboost_weighted_poisson_objective(
      quadrature_weight = data$quadrature_weight,
      statistical_weight = data$statistical_weight
    )

    xgb <- reticulate::import("xgboost")

    dtrain <- xgb$DMatrix(
      data = as.matrix(data$X),
      label = data$y
    )

    result <- objective(
      reticulate::r_to_py(prediction),
      dtrain
    )

    gradient <- as.numeric(
      reticulate::py_to_r(result[[1]])
    )

    hessian <- as.numeric(
      reticulate::py_to_r(result[[2]])
    )

    expect_equal(
      gradient,
      expected$gradient,
      tolerance = 1e-12
    )

    expect_equal(
      hessian,
      expected$hessian,
      tolerance = 1e-12
    )
  }
)

test_that("XGBoost can fit a GBPP weighted Poisson model", {

  skip_if_not(
    reticulate::py_available(),
    message = "Python is not available."
  )

  configure_gbpp_python()

  data <- new_gbpp_data(
    X = data.frame(
      x1 = c(0, 1, 2, 3, 4, 5),
      x2 = c(1, 1, 2, 2, 3, 3)
    ),
    y = c(1, 0, 1, 0, 1, 0),
    locations = data.frame(
      x = c(0, 1, 2, 3, 4, 5),
      y = c(0, 1, 0, 1, 0, 1)
    ),
    quadrature_weight = rep(0.5, 6),
    statistical_weight = c(
      1.0, 0.5, 1.5, 1.0, 2.0, 0.75
    )
  )

  model <- fit_gbpp_xgboost_weighted_poisson(
    data = data,
    params = list(
      eta = 0.1,
      max_depth = 2L
    ),
    nrounds = 5L
  )

  expect_true(
    reticulate::py_has_attr(model, "predict")
  )

  dtest <- reticulate::import("xgboost")$DMatrix(
    data = as.matrix(data$X)
  )

  dtest$feature_names <- colnames(data$X)

  predictions <- model$predict(dtest)

  predictions <- reticulate::py_to_r(predictions)

  expect_length(
    predictions,
    nrow(data$X)
  )

  expect_true(
    all(is.finite(predictions))
  )
})

test_that(
  "XGBoost logistic objective matches the GBPP reference objective",
  {

    skip_if_not(
      reticulate::py_available(),
      message = "Python is not available."
    )

    configure_gbpp_python()

    data <- new_gbpp_data(
      X = data.frame(
        x1 = c(0, 1, 2, 3),
        x2 = c(1, 1, 2, 2)
      ),
      y = c(1, 0, 1, 0),
      locations = data.frame(
        x = c(0, 1, 2, 3),
        y = c(0, 1, 0, 1)
      ),
      dummy_intensity = c(
        100, 100, 100, 100
      )
    )

    prediction <- c(
      -0.5, 0, 0.5, 1
    )

    expected <- gbpp_objective(
      data = data,
      prediction = prediction,
      objective = "logistic"
    )

    objective <- make_xgboost_logistic_objective(
      dummy_intensity = data$dummy_intensity
    )

    xgb <- reticulate::import("xgboost")

    dtrain <- xgb$DMatrix(
      data = as.matrix(data$X),
      label = data$y
    )

    result <- objective(
      reticulate::r_to_py(prediction),
      dtrain
    )

    gradient <- as.numeric(
      reticulate::py_to_r(result[[1]])
    )

    hessian <- as.numeric(
      reticulate::py_to_r(result[[2]])
    )

    expect_equal(
      gradient,
      expected$gradient,
      tolerance = 1e-12
    )

    expect_equal(
      hessian,
      expected$hessian,
      tolerance = 1e-12
    )
  }
)

test_that("XGBoost can fit a GBPP logistic model", {

  skip_if_not(
    reticulate::py_available(),
    message = "Python is not available."
  )

  configure_gbpp_python()

  data <- new_gbpp_data(
    X = data.frame(
      x1 = c(0, 1, 2, 3, 4, 5),
      x2 = c(1, 1, 2, 2, 3, 3)
    ),
    y = c(1, 0, 1, 0, 1, 0),
    locations = data.frame(
      x = c(0, 1, 2, 3, 4, 5),
      y = c(0, 1, 0, 1, 0, 1)
    ),
    dummy_intensity = rep(100, 6)
  )

  model <- fit_gbpp_xgboost_logistic(
    data = data,
    params = list(
      eta = 0.1,
      max_depth = 2L
    ),
    nrounds = 5L
  )

  expect_true(
    reticulate::py_has_attr(model, "predict")
  )

  dtest <- reticulate::import("xgboost")$DMatrix(
    data = as.matrix(data$X)
  )

  dtest$feature_names <- colnames(data$X)

  predictions <- model$predict(dtest)

  predictions <- reticulate::py_to_r(predictions)

  expect_length(
    predictions,
    nrow(data$X)
  )

  expect_true(
    all(is.finite(predictions))
  )
})
