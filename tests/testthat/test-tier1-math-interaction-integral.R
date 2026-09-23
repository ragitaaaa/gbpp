test_that(
  "interaction integral follows the K-function approximation",
  {

    K_value <- 10
    rmax <- 1

    result <- calculate_interaction_integral(
      K_value = K_value,
      rmax = rmax
    )

    expect_equal(
      result,
      10 - pi
    )
  }
)


test_that(
  "interaction integral can be negative",
  {

    K_value <- 2
    rmax <- 1

    result <- calculate_interaction_integral(
      K_value = K_value,
      rmax = rmax
    )

    expect_equal(
      result,
      2 - pi
    )
  }
)


test_that(
  "interaction integral scales with rmax",
  {

    K_value <- 20
    rmax <- 2

    result <- calculate_interaction_integral(
      K_value = K_value,
      rmax = rmax
    )

    expected <- 20 - pi * 2^2

    expect_equal(
      result,
      expected
    )
  }
)


test_that(
  "interaction integral rejects invalid K_value",
  {

    expect_error(
      calculate_interaction_integral(
        K_value = c(10, 11),
        rmax = 1
      ),
      "`K_value` must be a single finite numeric value"
    )

    expect_error(
      calculate_interaction_integral(
        K_value = NA_real_,
        rmax = 1
      ),
      "`K_value` must be a single finite numeric value"
    )
  }
)


test_that(
  "interaction integral rejects invalid rmax",
  {

    expect_error(
      calculate_interaction_integral(
        K_value = 10,
        rmax = 0
      ),
      "`rmax` must be a single positive finite numeric value"
    )

    expect_error(
      calculate_interaction_integral(
        K_value = 10,
        rmax = -1
      ),
      "`rmax` must be a single positive finite numeric value"
    )

    expect_error(
      calculate_interaction_integral(
        K_value = 10,
        rmax = NA_real_
      ),
      "`rmax` must be a single positive finite numeric value"
    )
  }
)


test_that(
  "interaction integral is finite for valid input",
  {

    result <- calculate_interaction_integral(
      K_value = 15,
      rmax = 2
    )

    expect_true(
      is.finite(result)
    )
  }
)
