# Helper fixtures for gbpp testthat suite

make_small_gbpp_data <- function(n = 8L) {
  set.seed(42)
  X <- matrix(
    c(
      0.0, 0.0,
      0.0, 1.0,
      1.0, 0.0,
      1.0, 1.0,
      0.5, 0.5,
      0.2, 0.8,
      0.8, 0.2,
      0.3, 0.7
    ),
    ncol = 2,
    byrow = TRUE
  )
  colnames(X) <- c("feat1", "feat2")

  new_gbpp_data(
    X = X,
    y = c(1, 0, 1, 0, 1, 0, 0, 1),
    locations = X,
    quadrature_weight = rep(1.0 / n, n),
    dummy_intensity = rep(2.0, n),
    statistical_weight = rep(1.0, n),
    interaction_integral = rep(0.01, n),
    window = NULL,
    type = "planar"
  )
}

make_small_planar_fixture <- function(n = 20L) {
  set.seed(42)
  win <- spatstat.geom::owin(c(0, 1), c(0, 1))
  X <- spatstat.random::runifpoint(n, win = win)
  cov1 <- spatstat.geom::as.im(function(x, y) x + y, W = win, dimyx = c(20, 20))
  cov2 <- spatstat.geom::as.im(function(x, y) x^2 + y^2, W = win, dimyx = c(20, 20))
  list(
    point_pattern = X,
    covariates = list(elev = cov1, grad = cov2)
  )
}
