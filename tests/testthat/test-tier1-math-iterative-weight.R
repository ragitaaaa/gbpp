test_that("iterative Poisson weight equals one for zero interaction", {

  prediction <- c(-1, 0, 1)
  interaction_integral <- c(0, 0, 0)

  result <- calculate_iterative_poisson_weight(
    prediction,
    interaction_integral
  )

  expect_equal(
    result,
    c(1, 1, 1)
  )
})


test_that("iterative Poisson weight follows theoretical formula", {

  prediction <- c(0, 0, 0)
  interaction_integral <- c(0, 0.5, 1)

  result <- calculate_iterative_poisson_weight(
    prediction,
    interaction_integral
  )

  expected <- c(
    1,
    1 / 1.5,
    1 / 2
  )
  expected <- expected / mean(expected)

  expect_equal(
    result,
    expected
  )
})


test_that("iterative Poisson weight uses current intensity", {

  prediction <- log(c(2, 1, 0.5))
  interaction_integral <- rep(0.5, 3)

  result <- calculate_iterative_poisson_weight(
    prediction,
    interaction_integral
  )

  expected <- 1 / (
    1 +
      c(2, 1, 0.5) * 0.5
  )
  expected <- expected / mean(expected)

  expect_equal(
    result,
    expected
  )
})


test_that("iterative Poisson weight is numerically stable", {

  prediction <- c(-100, 0, 100)
  interaction_integral <- c(1, 1, 1)

  result <- calculate_iterative_poisson_weight(
    prediction,
    interaction_integral
  )

  expect_true(
    all(is.finite(result))
  )
})


test_that("iterative Poisson weight rejects non-positive denominator", {

  expect_error(
    calculate_iterative_poisson_weight(
      prediction = log(2),
      interaction_integral = -1
    ),
    "weight is undefined"
  )
})

test_that("iterative Logistic weight follows theoretical formula", {

  prediction <- log(c(1, 2, 4))

  dummy_intensity <- c(
    1,
    2,
    4
  )

  interaction_integral <- c(
    0,
    0.5,
    1
  )

  result <- calculate_iterative_logistic_weight(
    prediction = prediction,
    dummy_intensity = dummy_intensity,
    interaction_integral = interaction_integral
  )

  lambda <- exp(prediction)

  expected <- (
    lambda + dummy_intensity
  ) / (
    dummy_intensity *
      (1 + lambda * interaction_integral)
  )
  expected <- expected / mean(expected)

  expect_equal(
    result,
    expected,
    tolerance = 1e-12
  )
})


test_that("iterative Logistic weight is not necessarily one when interaction is zero", {

  prediction <- log(c(1, 2))

  dummy_intensity <- c(
    1,
    2
  )

  interaction_integral <- c(
    0,
    0
  )

  result <- calculate_iterative_logistic_weight(
    prediction = prediction,
    dummy_intensity = dummy_intensity,
    interaction_integral = interaction_integral
  )

  expected <- c(
    2,
    2
  )
  expected <- expected / mean(expected)

  expect_equal(
    result,
    expected
  )
})


test_that("iterative Logistic weight remains finite for extreme predictions", {

  prediction <- c(
    -100,
    0,
    100
  )

  dummy_intensity <- c(
    100,
    100,
    100
  )

  interaction_integral <- c(
    0.5,
    0.5,
    0.5
  )

  result <- calculate_iterative_logistic_weight(
    prediction = prediction,
    dummy_intensity = dummy_intensity,
    interaction_integral = interaction_integral
  )

  expect_true(
    all(is.finite(result))
  )
})
