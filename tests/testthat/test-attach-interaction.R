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

  result <- attach_interaction_integral(
    data = data_object,
    point_pattern = lansing,
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
  expect_error(
    attach_interaction_integral(
      data = list(),
      point_pattern = lansing,
      rmax = 0.2
    ),
    "`data` must be a `gbpp_data` object"
  )
})
