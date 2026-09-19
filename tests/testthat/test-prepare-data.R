test_that("prepare_pp_data converts a planar quadrature scheme", {

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

  X <- data.frame(
    x1 = seq_len(length(spatstat.geom::is.data(Q))),
    x2 = 1
  )

  data <- prepare_pp_data(
    Q = Q,
    X = X,
    objective = "poisson",
    type = "planar"
  )

  expect_s3_class(data, "gbpp_data")

  expect_equal(
    nrow(data$X),
    length(data$y)
  )

  expect_equal(
    length(data$quadrature_weight),
    length(data$y)
  )

  expect_equal(
    nrow(data$locations),
    length(data$y)
  )

  expect_equal(
    data$y,
    as.integer(spatstat.geom::is.data(Q))
  )

  expect_equal(
    data$type,
    "planar"
  )
})

test_that("prepare_pp_data converts a planar quadrature scheme", {

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

  X <- data.frame(
    x1 = seq_len(length(spatstat.geom::is.data(Q))),
    x2 = 1
  )

  data <- prepare_pp_data(
    Q = Q,
    X = X,
    objective = "poisson",
    type = "planar"
  )

  expect_s3_class(data, "gbpp_data")

  expect_equal(
    nrow(data$X),
    length(data$y)
  )

  expect_equal(
    length(data$quadrature_weight),
    length(data$y)
  )

  expect_equal(
    nrow(data$locations),
    length(data$y)
  )

  expect_equal(
    data$y,
    as.integer(spatstat.geom::is.data(Q))
  )

  expect_equal(
    data$type,
    "planar"
  )
})
