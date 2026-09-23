test_that("network dummy intensity is calculated from dummy density", {
  y <- c(1, 1, 0, 0, 0)
  quadrature_weight <- c(1, 2, 3, 4, 5)

  result <- calculate_network_dummy_intensity(
    y = y,
    quadrature_weight = quadrature_weight
  )

  expect_type(result, "double")
  expect_length(result, 1L)
  expect_equal(result, 3 / 15)
})


test_that("network dummy intensity rejects invalid inputs", {
  expect_error(
    calculate_network_dummy_intensity(
      y = c(1, 1),
      quadrature_weight = c(1)
    )
  )

  expect_error(
    calculate_network_dummy_intensity(
      y = c(1, 1),
      quadrature_weight = c(1, 0)
    )
  )

  expect_error(
    calculate_network_dummy_intensity(
      y = c(1, 1),
      quadrature_weight = c(1, 1)
    ),
    "At least one dummy point"
  )
})
