test_that("Poisson derivatives are calculated correctly", {

  data <- new_gbpp_data(
    X = data.frame(x1 = 1:3),
    y = c(1, 0, 0),
    locations = data.frame(
      x = 1:3,
      y = 1:3
    ),
    quadrature_weight = c(0.2, 0.3, 0.5)
  )

  prediction <- c(
    log(2),
    log(3),
    log(4)
  )

  result <- gbpp_objective(
    data = data,
    prediction = prediction,
    objective = "poisson"
  )

  expected_gradient <- c(
    0.2 * 2 - 1,
    0.3 * 3 - 0,
    0.5 * 4 - 0
  )

  expected_hessian <- c(
    0.2 * 2,
    0.3 * 3,
    0.5 * 4
  )

  expect_equal(
    result$gradient,
    expected_gradient
  )

  expect_equal(
    result$hessian,
    expected_hessian
  )
})

test_that("Poisson derivatives are calculated correctly", {

  data <- new_gbpp_data(
    X = data.frame(x1 = 1:3),
    y = c(1, 0, 0),
    locations = data.frame(
      x = 1:3,
      y = 1:3
    ),
    quadrature_weight = c(0.2, 0.3, 0.5)
  )

  prediction <- c(
    log(2),
    log(3),
    log(4)
  )

  result <- gbpp_objective(
    data = data,
    prediction = prediction,
    objective = "poisson"
  )

  expected_gradient <- c(
    0.2 * 2 - 1,
    0.3 * 3 - 0,
    0.5 * 4 - 0
  )

  expected_hessian <- c(
    0.2 * 2,
    0.3 * 3,
    0.5 * 4
  )

  expect_equal(
    result$gradient,
    expected_gradient
  )

  expect_equal(
    result$hessian,
    expected_hessian
  )
})

test_that("Logistic derivatives are calculated correctly", {

  data <- new_gbpp_data(
    X = data.frame(x1 = 1:3),
    y = c(1, 0, 1),
    locations = data.frame(
      x = 1:3,
      y = 1:3
    ),
    dummy_intensity = c(100, 100, 100)
  )

  prediction <- rep(log(100), 3)

  result <- gbpp_objective(
    data = data,
    prediction = prediction,
    objective = "logistic"
  )

  expected_pi <- rep(0.5, 3)

  expected_gradient <- expected_pi - data$y

  expected_hessian <- rep(0.25, 3)

  expect_equal(
    result$gradient,
    expected_gradient
  )

  expect_equal(
    result$hessian,
    expected_hessian
  )
})

test_that("Weighted logistic derivatives are calculated correctly", {

  data <- new_gbpp_data(
    X = data.frame(x1 = 1:3),
    y = c(1, 0, 1),
    locations = data.frame(
      x = 1:3,
      y = 1:3
    ),
    dummy_intensity = c(100, 100, 100),
    statistical_weight = c(2, 3, 4)
  )

  prediction <- rep(log(100), 3)

  result <- gbpp_objective(
    data = data,
    prediction = prediction,
    objective = "weighted_logistic"
  )

  expected_pi <- rep(0.5, 3)

  expected_gradient <- c(
    2 * (0.5 - 1),
    3 * (0.5 - 0),
    4 * (0.5 - 1)
  )

  expected_hessian <- c(
    2 * 0.25,
    3 * 0.25,
    4 * 0.25
  )

  expect_equal(
    result$gradient,
    expected_gradient
  )

  expect_equal(
    result$hessian,
    expected_hessian
  )
})

test_that("Logistic derivatives remain finite for large predictions", {

  data <- new_gbpp_data(
    X = data.frame(x1 = 1:2),
    y = c(1, 0),
    locations = data.frame(
      x = 1:2,
      y = 1:2
    ),
    dummy_intensity = c(100, 100)
  )

  prediction <- c(1000, -1000)

  result <- gbpp_objective(
    data = data,
    prediction = prediction,
    objective = "logistic"
  )

  expect_true(all(is.finite(result$gradient)))
  expect_true(all(is.finite(result$hessian)))
})

test_that("iterative Poisson objective matches theoretical derivatives", {

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

  result <- gbpp_objective(
    data = data,
    prediction = prediction,
    objective = "iterative_poisson"
  )

  expected_weight <- c(
    1,
    2 / 3,
    1 / 2,
    1 / 3
  )

  expected_gradient <- c(
    -0.8,
    0.2,
    -0.3,
    1 / 6
  )

  expected_hessian <- c(
    0.2,
    0.2,
    0.2,
    1 / 6
  )

  expect_equal(
    result$weight,
    expected_weight,
    tolerance = 1e-12
  )

  expect_equal(
    result$gradient,
    expected_gradient,
    tolerance = 1e-12
  )

  expect_equal(
    result$hessian,
    expected_hessian,
    tolerance = 1e-12
  )
})

test_that("iterative Poisson objective is available through gbpp_objective", {

  data <- new_gbpp_data(
    X = data.frame(
      x1 = c(1, 2),
      x2 = c(2, 1)
    ),
    y = c(1, 0),
    locations = data.frame(
      x = c(0, 1),
      y = c(0, 1)
    ),
    quadrature_weight = c(
      0.25,
      0.25
    ),
    interaction_integral = c(
      0,
      1
    )
  )

  expect_no_error(
    gbpp_objective(
      data = data,
      prediction = c(0, 0),
      objective = "iterative_poisson"
    )
  )
})
