test_that("new_gbpp_data creates a gbpp_data object", {

  X <- data.frame(
    x1 = c(1, 2, 3),
    x2 = c(4, 5, 6)
  )

  y <- c(1, 0, 0)

  locations <- data.frame(
    x = c(0.1, 0.2, 0.3),
    y = c(0.4, 0.5, 0.6)
  )

  data <- new_gbpp_data(
    X = X,
    y = y,
    locations = locations,
    quadrature_weight = c(0.1, 0.2, 0.3),
    type = "planar"
  )

  expect_s3_class(data, "gbpp_data")
  expect_equal(nrow(data$X), 3)
  expect_equal(data$y, c(1, 0, 0))
})


test_that("validate_gbpp_data accepts valid Poisson data", {

  X <- data.frame(
    x1 = c(1, 2, 3),
    x2 = c(4, 5, 6)
  )

  y <- c(1, 0, 0)

  locations <- data.frame(
    x = c(0.1, 0.2, 0.3),
    y = c(0.4, 0.5, 0.6)
  )

  data <- new_gbpp_data(
    X = X,
    y = y,
    locations = locations,
    quadrature_weight = c(0.1, 0.2, 0.3),
    type = "planar"
  )

  expect_true(
    validate_gbpp_data(data, objective = "poisson")
  )
})


test_that("validate_gbpp_data rejects invalid labels", {

  X <- data.frame(
    x1 = c(1, 2, 3)
  )

  y <- c(1, -1, 0)

  locations <- data.frame(
    x = c(0.1, 0.2, 0.3),
    y = c(0.4, 0.5, 0.6)
  )

  data <- new_gbpp_data(
    X = X,
    y = y,
    locations = locations,
    quadrature_weight = c(0.1, 0.2, 0.3)
  )

  expect_error(
    validate_gbpp_data(data),
    "only 0"
  )
})


test_that("weighted objectives require statistical weights", {

  X <- data.frame(
    x1 = c(1, 2, 3)
  )

  y <- c(1, 0, 0)

  locations <- data.frame(
    x = c(0.1, 0.2, 0.3),
    y = c(0.4, 0.5, 0.6)
  )

  data <- new_gbpp_data(
    X = X,
    y = y,
    locations = locations,
    quadrature_weight = c(0.1, 0.2, 0.3)
  )

  expect_error(
    validate_gbpp_data(
      data,
      objective = "weighted_poisson"
    ),
    "statistical_weight"
  )
})
