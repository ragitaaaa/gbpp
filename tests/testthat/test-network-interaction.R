test_that("network interaction integral handles a constant pair correlation", {
  data(simplenet, package = "spatstat.data")

  locations <- spatstat.linnet::runiflpp(
    n = 4,
    L = simplenet
  )

  pair_correlation <- function(distance) {
    rep(1.5, length(distance))
  }

  result <- calculate_network_interaction_integral(
    locations = locations,
    network = simplenet,
    pair_correlation = pair_correlation,
    exact = TRUE
  )

  unit_function <- spatstat.linnet::linfun(
    function(x, y, seg, tp) {
      rep(1, length(x))
    },
    L = simplenet
  )

  network_length <- spatstat.linnet::integral.linfun(
    unit_function,
    exact = TRUE
  )

  expect_type(result, "double")
  expect_length(result, 4L)
  expect_equal(
    result,
    rep(0.5 * network_length, 4L),
    tolerance = 1e-8
  )
})


test_that("network interaction integral handles distance-dependent pair correlation", {
  data(simplenet, package = "spatstat.data")

  locations <- spatstat.linnet::runiflpp(
    n = 3,
    L = simplenet
  )

  pair_correlation <- function(distance) {
    1 + 0.5 * exp(-distance)
  }

  result <- calculate_network_interaction_integral(
    locations = locations,
    network = simplenet,
    pair_correlation = pair_correlation,
    exact = TRUE
  )

  expect_type(result, "double")
  expect_length(result, 3L)
  expect_true(all(is.finite(result)))
})


test_that("network interaction integral rejects incompatible networks", {
  data(simplenet, package = "spatstat.data")

  locations <- spatstat.linnet::runiflpp(
    n = 3,
    L = simplenet
  )

  other_network <- spatstat.linnet::as.linnet(
    spatstat.linnet::insertVertices(
      simplenet,
      seg = 1,
      tp = 0.5
    )
  )

  expect_error(
    calculate_network_interaction_integral(
      locations = locations,
      network = other_network,
      pair_correlation = function(distance) {
        rep(1, length(distance))
      }
    ),
    "same linear network"
  )
})


test_that("network interaction integral validates its arguments", {
  data(simplenet, package = "spatstat.data")

  locations <- spatstat.linnet::runiflpp(
    n = 2,
    L = simplenet
  )

  expect_error(
    calculate_network_interaction_integral(
      # locations = spatstat.geom::runifpoint(2),
      locations = spatstat.geom::ppp(
        x = c(0.25, 0.75),
        y = c(0.25, 0.75),
        window = spatstat.geom::square(1)
      ),
      network = simplenet,
      pair_correlation = function(distance) {
        distance
      }
    ),
    "must be an `lpp` object"
  )

  expect_error(
    calculate_network_interaction_integral(
      locations = locations,
      network = simplenet,
      pair_correlation = 1
    ),
    "must be a function"
  )

  expect_error(
    calculate_network_interaction_integral(
      locations = locations,
      network = simplenet,
      pair_correlation = function(distance) {
        distance
      },
      nd = 0
    ),
    "positive integer"
  )
})
