make_test_network_data <- function() {
  data(simplenet, package = "spatstat.data")

  point_pattern <- spatstat.linnet::runiflpp(
    n = 8,
    L = simplenet
  )

  network <- spatstat.linnet::as.linnet(point_pattern)

  numeric_covariate <- spatstat.linnet::as.linim(
    function(x, y) x + y,
    L = network,
    delta = 0.1
  )

  list(
    point_pattern = point_pattern,
    covariates = list(
      numeric = numeric_covariate
    )
  )
}


test_that("network preparation constructs general Berman-Turner data", {
  fixture <- make_test_network_data()

  result <- prepare_gbpp_network(
    point_pattern = fixture$point_pattern,
    covariates = fixture$covariates,
    nd = 20L
  )

  expect_s3_class(result, "gbpp_data")
  expect_identical(result$type, "network")

  expect_equal(nrow(result$X), length(result$y))
  expect_equal(
    length(result$y),
    length(result$quadrature_weight)
  )

  expect_equal(sum(result$y), 8L)
  expect_true(all(result$y %in% c(0L, 1L)))

  expect_true(all(is.finite(result$quadrature_weight)))
  expect_true(all(result$quadrature_weight > 0))

  expect_identical(names(result$X), "numeric")
  expect_true(is.numeric(result$X$numeric))
  expect_false(anyNA(result$X))

  expect_equal(
    nrow(result$X),
    length(result$locations$x)
  )
})


test_that("network preparation accepts no covariates", {
  fixture <- make_test_network_data()

  result <- prepare_gbpp_network(
    point_pattern = fixture$point_pattern,
    covariates = list(),
    nd = 20L
  )

  expect_s3_class(result, "gbpp_data")
  expect_equal(nrow(result$X), length(result$y))
  expect_equal(ncol(result$X), 0L)
})


test_that("network preparation rejects unnamed covariates", {
  fixture <- make_test_network_data()

  expect_error(
    prepare_gbpp_network(
      point_pattern = fixture$point_pattern,
      covariates = unname(fixture$covariates),
      nd = 20L
    ),
    "All network covariates must be named"
  )
})


test_that("network preparation rejects duplicate covariate names", {
  fixture <- make_test_network_data()

  covariates <- list(
    x = fixture$covariates$numeric,
    x = fixture$covariates$numeric
  )

  expect_error(
    prepare_gbpp_network(
      point_pattern = fixture$point_pattern,
      covariates = covariates,
      nd = 20L
    ),
    "must be unique"
  )
})


test_that("network preparation rejects non-linim covariates", {
  fixture <- make_test_network_data()

  expect_error(
    prepare_gbpp_network(
      point_pattern = fixture$point_pattern,
      covariates = list(x = 1),
      nd = 20L
    ),
    "All network covariates must be `linim` objects"
  )
})


test_that("network preparation validates point pattern", {
  expect_error(
    prepare_gbpp_network(
      point_pattern = spatstat.geom::ppp(
        x = 0.5,
        y = 0.5,
        window = spatstat.geom::square(1)
      )
    ),
    "must be an `lpp` object"
  )
})


test_that("network preparation validates nd", {
  fixture <- make_test_network_data()

  expect_error(
    prepare_gbpp_network(
      point_pattern = fixture$point_pattern,
      nd = 1.5
    ),
    "positive integer"
  )

  expect_error(
    prepare_gbpp_network(
      point_pattern = fixture$point_pattern,
      nd = 0
    ),
    "positive integer"
  )
})
