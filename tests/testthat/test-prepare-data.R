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

test_that("prepare_pp_data extracts dummy intensity for logistic objective", {

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

  X <- data.frame(
    x1 = seq_len(length(spatstat.geom::is.data(Q)))
  )

  data <- prepare_pp_data(
    Q = Q,
    X = X,
    objective = "logistic"
  )

  expect_type(data$dummy_intensity, "double")
  expect_length(
    data$dummy_intensity,
    length(data$y)
  )

  expect_equal(
    data$dummy_intensity,
    rep(Q$param$rho, length(data$y))
  )
})

test_that("prepare_pp_data does not create dummy intensity for Poisson objective", {

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
    x1 = seq_len(length(spatstat.geom::is.data(Q)))
  )

  data <- prepare_pp_data(
    Q = Q,
    X = X,
    objective = "poisson"
  )

  expect_null(data$dummy_intensity)
})

test_that(
  "prepare_gbpp accepts covariate images that cover the point-pattern window",
  {
    data(bei, package = "spatstat.data")

    covariate <- bei.extra$elev

    expect_false(
      identical(
        spatstat.geom::Window(bei),
        spatstat.geom::Window(covariate)
      )
    )

    expect_true(
      spatstat.geom::is.subset.owin(
        spatstat.geom::Window(bei),
        spatstat.geom::Window(covariate)
      )
    )

    data <- prepare_gbpp(
      point_pattern = bei,
      covariates = list(elevation = covariate),
      nd = 20L
    )

    expect_s3_class(data, "gbpp_data")

    expect_equal(
      nrow(data$X),
      length(data$y)
    )

    expect_identical(
      colnames(data$X),
      "elevation"
    )

    expect_true(
      all(is.finite(data$X$elevation))
    )

    expect_equal(
      data$type,
      "planar"
    )
  }
)
