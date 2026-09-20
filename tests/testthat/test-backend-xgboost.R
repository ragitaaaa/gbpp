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

test_that("XGBoost weighted logistic objective matches GBPP derivatives", {

  data <- new_gbpp_data(
    X = data.frame(
      x1 = c(1, 2, 3, 4),
      x2 = c(4, 3, 2, 1)
    ),
    y = c(1, 0, 1, 0),
    locations = data.frame(
      x = c(0, 1, 2, 3),
      y = c(0, 1, 2, 3)
    ),
    dummy_intensity = rep(100, 4),
    statistical_weight = c(
      1.0,
      0.5,
      2.0,
      1.5
    )
  )

  prediction <- c(
    -0.5,
    0,
    0.5,
    1
  )

  expected <- gbpp_objective(
    data,
    prediction,
    objective = "weighted_logistic"
  )

  objective <- make_xgboost_weighted_logistic_objective(
    dummy_intensity = data$dummy_intensity,
    statistical_weight = data$statistical_weight
  )

  xgb <- reticulate::import("xgboost")

  dtrain <- xgb$DMatrix(
    data = as.matrix(data$X),
    label = as.numeric(data$y)
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

test_that("XGBoost weighted logistic model can be trained", {

  data <- new_gbpp_data(
    X = data.frame(
      x1 = c(1, 2, 3, 4, 5, 6),
      x2 = c(6, 5, 4, 3, 2, 1)
    ),
    y = c(1, 0, 1, 0, 1, 0),
    locations = data.frame(
      x = c(0, 1, 2, 3, 4, 5),
      y = c(0, 1, 2, 3, 4, 5)
    ),
    dummy_intensity = rep(100, 6),
    statistical_weight = c(
      1.0,
      0.8,
      1.2,
      0.7,
      1.5,
      0.9
    )
  )

  model <- fit_gbpp_xgboost_weighted_logistic(
    data = data,
    params = list(
      eta = 0.1,
      max_depth = 2L
    ),
    num_boost_round = 5L
  )

  expect_true(
    reticulate::py_is_null_xptr(model) == FALSE
  )

  xgb <- reticulate::import("xgboost")

  dtest <- xgb$DMatrix(
    data = as.matrix(data$X)
  )

  dtest$feature_names <- colnames(data$X)

  predictions <- model$predict(dtest)

  predictions <- as.numeric(
    reticulate::py_to_r(predictions)
  )

  expect_length(
    predictions,
    nrow(data$X)
  )

  expect_true(
    all(is.finite(predictions))
  )
})

test_that(
  "XGBoost iterative Poisson objective matches GBPP derivatives",
  {

    data <- new_gbpp_data(
      X = data.frame(
        x1 = c(1, 2, 3, 4),
        x2 = c(4, 3, 2, 1)
      ),
      y = c(1, 0, 1, 0),
      locations = data.frame(
        x = c(0, 1, 2, 3),
        y = c(0, 1, 2, 3)
      ),
      quadrature_weight = c(
        0.2,
        0.3,
        0.4,
        0.5
      ),
      interaction_integral = c(
        0,
        0.5,
        1,
        2
      )
    )

    prediction <- c(
      0,
      0,
      0,
      0
    )

    expected <- gbpp_objective(
      data = data,
      prediction = prediction,
      objective = "iterative_poisson"
    )

    objective <- make_xgboost_iterative_poisson_objective(
      quadrature_weight = data$quadrature_weight,
      interaction_integral = data$interaction_integral
    )

    xgb <- reticulate::import("xgboost")

    dtrain <- xgb$DMatrix(
      data = as.matrix(data$X),
      label = as.numeric(data$y)
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

test_that(
  "XGBoost iteratively weighted Poisson model can be trained",
  {

    data <- new_gbpp_data(
      X = data.frame(
        x1 = c(1, 2, 3, 4, 5, 6),
        x2 = c(6, 5, 4, 3, 2, 1)
      ),
      y = c(1, 0, 1, 0, 1, 0),
      locations = data.frame(
        x = c(0, 1, 2, 3, 4, 5),
        y = c(0, 1, 2, 3, 4, 5)
      ),
      quadrature_weight = c(
        0.10,
        0.15,
        0.20,
        0.15,
        0.20,
        0.20
      ),
      interaction_integral = c(
        0.00,
        0.25,
        0.50,
        0.75,
        0.25,
        0.50
      )
    )

    model <- fit_gbpp_xgboost_iterative_poisson(
      data = data,
      params = list(
        eta = 0.1,
        max_depth = 2L
      ),
      num_boost_round = 5L
    )

    expect_false(
      reticulate::py_is_null_xptr(model)
    )

    xgb <- reticulate::import("xgboost")

    dtest <- xgb$DMatrix(
      data = as.matrix(data$X)
    )

    dtest$feature_names <- colnames(data$X)

    predictions <- model$predict(dtest)

    predictions <- as.numeric(
      reticulate::py_to_r(predictions)
    )

    expect_length(
      predictions,
      nrow(data$X)
    )

    expect_true(
      all(is.finite(predictions))
    )
  }
)

test_that(
  "iterative Poisson reduces to Poisson when interaction is zero",
  {

    data <- new_gbpp_data(
      X = data.frame(
        x1 = c(1, 2, 3),
        x2 = c(3, 2, 1)
      ),
      y = c(1, 0, 1),
      locations = data.frame(
        x = c(0, 1, 2),
        y = c(0, 1, 2)
      ),
      quadrature_weight = c(
        0.2,
        0.3,
        0.4
      ),
      interaction_integral = c(
        0,
        0,
        0
      )
    )

    prediction <- c(
      -0.5,
      0.2,
      0.7
    )

    poisson <- gbpp_objective(
      data,
      prediction,
      objective = "poisson"
    )

    iterative <- gbpp_objective(
      data,
      prediction,
      objective = "iterative_poisson"
    )

    expect_equal(
      iterative$weight,
      rep(1, 3)
    )

    expect_equal(
      iterative$gradient,
      poisson$gradient
    )

    expect_equal(
      iterative$hessian,
      poisson$hessian
    )
  }
)
