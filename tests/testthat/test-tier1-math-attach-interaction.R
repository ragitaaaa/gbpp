test_that("interaction integral can be attached to gbpp data", {
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

  point_pattern <- lansing[seq_len(5)]

  result <- attach_interaction_integral(
    data = data_object,
    point_pattern = point_pattern,
    rmax = 0.2
  )

  expect_s3_class(result, "gbpp_data")
  expect_length(result$interaction_integral, n)
  expect_true(all(is.finite(result$interaction_integral)))
  expect_true(
    all(
      result$interaction_integral ==
        result$interaction_integral[1]
    )
  )
})


test_that("interaction integral attachment validates its inputs", {
  data(lansing, package = "spatstat.data")

  expect_error(
    attach_interaction_integral(
      data = list(),
      point_pattern = lansing,
      rmax = 0.2
    ),
    "`data` must be a `gbpp_data` object"
  )

  n <- 20L

  data_object <- new_gbpp_data(
    X = matrix(
      rnorm(n * 2),
      ncol = 2
    ),
    y = c(
      rep(1, 5),
      rep(0, 15)
    ),
    locations = matrix(
      runif(n * 2),
      ncol = 2
    ),
    quadrature_weight = rep(1 / n, n),
    dummy_intensity = rep(1, n),
    statistical_weight = rep(1, n),
    interaction_integral = NULL
  )

  expect_error(
    attach_interaction_integral(
      data = data_object,
      point_pattern = lansing,
      rmax = 0.2
    ),
    "`data` contains 5 event\\(s\\), but `point_pattern` contains"
  )
})


test_that("interaction integral validates event count", {
  data(lansing, package = "spatstat.data")

  n <- 20L

  data_object <- new_gbpp_data(
    X = matrix(
      rnorm(n * 2),
      ncol = 2
    ),
    y = c(
      rep(1, 5),
      rep(0, 15)
    ),
    locations = matrix(
      runif(n * 2),
      ncol = 2
    ),
    quadrature_weight = rep(1 / n, n),
    dummy_intensity = rep(1, n),
    statistical_weight = rep(1, n),
    interaction_integral = NULL
  )

  point_pattern <- lansing[seq_len(5)]

  expect_no_error(
    attach_interaction_integral(
      data = data_object,
      point_pattern = point_pattern,
      rmax = 0.2
    )
  )
})


test_that("interaction integral validation rejects invalid values", {
  n <- 10L

  data_object <- new_gbpp_data(
    X = matrix(
      rnorm(n * 2),
      ncol = 2
    ),
    y = c(
      rep(1, 3),
      rep(0, 7)
    ),
    locations = matrix(
      runif(n * 2),
      ncol = 2
    ),
    quadrature_weight = rep(1 / n, n),
    dummy_intensity = rep(1, n),
    statistical_weight = rep(1, n),
    interaction_integral = NULL
  )

  data_object$interaction_integral <- rep(0, n - 1)

  expect_error(
    validate_gbpp_data(data_object),
    "`interaction_integral` must have one value per observation"
  )

  data_object$interaction_integral <- rep(0, n)
  data_object$interaction_integral[1] <- NA_real_

  expect_error(
    validate_gbpp_data(data_object),
    "`interaction_integral` must contain only finite values"
  )

  data_object$interaction_integral[1] <- Inf

  expect_error(
    validate_gbpp_data(data_object),
    "`interaction_integral` must contain only finite values"
  )

  data_object$interaction_integral <- as.character(
    rep(0, n)
  )

  expect_error(
    validate_gbpp_data(data_object),
    "`interaction_integral` must be numeric"
  )
})
