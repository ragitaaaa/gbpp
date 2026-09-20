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

test_that("iterative Poisson requires interaction_integral", {

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
    )
  )

  expect_error(
    validate_gbpp_data(
      data,
      objective = "iterative_poisson"
    ),
    "`interaction_integral` is required"
  )
})

test_that("interaction_integral must match number of observations", {

  expect_error(
    new_gbpp_data(
      X = data.frame(
        x1 = c(1, 2),
        x2 = c(2, 1)
      ),
      y = c(1, 0),
      locations = data.frame(
        x = c(0, 1),
        y = c(0, 1)
      ),
      interaction_integral = 1:3
    ) |>
      validate_gbpp_data(),
    "`interaction_integral` must have one value per observation"
  )
})
