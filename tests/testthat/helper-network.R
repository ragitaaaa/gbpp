make_network_integration_fixture <- function(n = 8L) {
  simplenet <- spatstat.data::simplenet

  point_pattern <- spatstat.linnet::runiflpp(
    n = n,
    L = simplenet
  )

  network <- spatstat.linnet::as.linnet(point_pattern)

  covariate_1 <- spatstat.linnet::as.linim(
    function(x, y) x + y,
    L = network,
    delta = 0.1
  )

  covariate_2 <- spatstat.linnet::as.linim(
    function(x, y) x^2 + y^2,
    L = network,
    delta = 0.1
  )

  list(
    point_pattern = point_pattern,
    covariates = list(
      x_sum = covariate_1,
      x2_y2 = covariate_2
    )
  )
}
