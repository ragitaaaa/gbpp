test_that("extract_dummy_intensity extracts rho from logistic quadrature", {

  W <- spatstat.geom::owin(
    xrange = c(0, 1),
    yrange = c(0, 1)
  )

  pp <- spatstat.random::runifpoint(
    n = 5,
    win = W
  )

  Q <- spatstat.geom::quadscheme.logi(
    pp,
    dummytype = "grid",
    nd = 10
  )

  delta <- extract_dummy_intensity(Q)

  n <- length(spatstat.geom::is.data(Q))

  expect_type(delta, "double")
  expect_length(delta, n)
  expect_true(all(is.finite(delta)))
  expect_true(all(delta > 0))
  expect_equal(
    delta,
    rep(Q$param$rho, n)
  )
})

test_that("dummy intensity is distinct from quadrature weight", {

  W <- spatstat.geom::owin(
    xrange = c(0, 1),
    yrange = c(0, 1)
  )

  pp <- spatstat.random::runifpoint(
    n = 5,
    win = W
  )

  Q <- spatstat.geom::quadscheme.logi(
    pp,
    dummytype = "grid",
    nd = 10
  )

  delta <- extract_dummy_intensity(Q)
  quadrature_weight <- spatstat.geom::w.quad(Q)

  expect_length(delta, length(quadrature_weight))

  expect_false(
    isTRUE(
      all.equal(
        delta,
        1 / quadrature_weight
      )
    )
  )
})

test_that("extract_dummy_intensity fails when rho is unavailable", {

  W <- spatstat.geom::owin(
    xrange = c(0, 1),
    yrange = c(0, 1)
  )

  pp <- spatstat.random::runifpoint(
    n = 5,
    win = W
  )

  Q <- spatstat.geom::quadscheme(
    pp,
    nd = 10
  )

  expect_error(
    extract_dummy_intensity(Q),
    "Dummy-process intensity could not be extracted"
  )
})

test_that("calculate_planar_dummy_intensity calculates m / |W| correctly", {
  W <- spatstat.geom::owin(c(0, 2), c(0, 3)) # area = 6
  y <- c(rep(1, 10), rep(0, 30)) # 30 dummy points
  
  # Passing window
  delta <- calculate_planar_dummy_intensity(y, W)
  expect_equal(delta, 30 / 6)
  
  # Passing quadrature weights summing to area
  w <- rep(6 / 40, 40)
  delta_w <- calculate_planar_dummy_intensity(y, w)
  expect_equal(delta_w, 30 / 6)
  
  # Input validation
  expect_error(calculate_planar_dummy_intensity(c(1, 2), W))
  expect_error(calculate_planar_dummy_intensity(rep(1, 10), W), "At least one dummy point is required")
})

