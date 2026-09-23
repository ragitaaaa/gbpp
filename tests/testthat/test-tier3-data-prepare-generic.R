test_that("prepare_gbpp prepares planar point-process data", {

  W <- spatstat.geom::owin(
    xrange = c(0, 1),
    yrange = c(0, 1)
  )

  pp <- spatstat.random::runifpoint(
    n = 5,
    win = W
  )

  covariate <- spatstat.geom::as.im(
    function(x, y) x + y,
    W = W
  )

  data <- prepare_gbpp(
    point_pattern = pp,
    covariates = list(
      elevation = covariate
    ),
    nd = 10L
  )

  expect_s3_class(data, "gbpp_data")

  expect_equal(
    data$type,
    "planar"
  )

  expect_equal(
    nrow(data$X),
    length(data$y)
  )

  expect_equal(
    nrow(data$locations),
    length(data$y)
  )

  expect_equal(
    length(data$quadrature_weight),
    length(data$y)
  )

  expect_type(
    data$dummy_intensity,
    "double"
  )

  expect_length(
    data$dummy_intensity,
    length(data$y)
  )

  expect_equal(
    names(data$X),
    "elevation"
  )

  expect_identical(
    data$window,
    W
  )
})
