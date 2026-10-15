#' Calculate a network interaction integral
#'
#' Calculates the location-specific interaction integral
#'
#' \deqn{
#' A(u) = \int_L [g(d_L(u,s)) - 1] \, d\ell(s),
#' }
#'
#' where `d_L(u, s)` is the shortest-path distance on the linear network
#' and `g` is a supplied pair-correlation function.
#'
#' @param locations An `lpp` object containing the locations `u` at which the
#'   interaction integral is evaluated.
#' @param network A `linnet` object defining the integration domain.
#' @param pair_correlation A function accepting a numeric vector of distances
#'   and returning the corresponding pair-correlation values.
#' @param exact Logical. If `TRUE`, use the exact segment-wise integration
#'   method provided by `spatstat.linnet::integral.linfun()`. If `FALSE`, use network
#'   quadrature with `nd` sample locations.
#' @param nd Approximate number of network integration points when `exact = FALSE`.
#'
#' @return A numeric vector containing one interaction integral for each location in `locations`.
#' @export
calculate_network_interaction_integral <- function(
    locations,
    network,
    pair_correlation,
    exact = FALSE,
    nd = 1000L
) {
  if (!inherits(locations, "lpp")) {
    stop("`locations` must be an `lpp` object.", call. = FALSE)
  }

  if (!inherits(network, "linnet")) {
    stop("`network` must be a `linnet` object.", call. = FALSE)
  }

  if (!is.function(pair_correlation)) {
    stop("`pair_correlation` must be a function.", call. = FALSE)
  }

  if (length(exact) != 1L || is.na(exact) || !is.logical(exact)) {
    stop("`exact` must be one logical value.", call. = FALSE)
  }

  if (length(nd) != 1L || !is.numeric(nd) || !is.finite(nd) || nd <= 0 || nd != as.integer(nd)) {
    stop("`nd` must be one positive integer.", call. = FALSE)
  }

  nd <- as.integer(nd)
  location_network <- spatstat.linnet::as.linnet(locations)

  if (!identical(location_network, network)) {
    stop("`locations` and `network` must be defined on the same linear network.", call. = FALSE)
  }

  n_locations <- spatstat.geom::npoints(locations)
  if (n_locations == 0L) {
    return(numeric(0))
  }

  result <- numeric(n_locations)

  for (i in seq_len(n_locations)) {
    location <- locations[i]
    distance_function <- spatstat.geom::distfun(location)

    integrand <- spatstat.linnet::linfun(
      function(x, y, seg, tp) {
        distance <- distance_function(x, y, seg = seg, tp = tp)
        value <- pair_correlation(distance)

        if (length(value) != length(distance)) {
          stop("`pair_correlation` must return one value for each distance.", call. = FALSE)
        }

        if (!is.numeric(value) || anyNA(value) || any(!is.finite(value))) {
          stop("`pair_correlation` must return finite numeric values.", call. = FALSE)
        }

        value - 1
      },
      L = network
    )

    result[i] <- spatstat.linnet::integral.linfun(
      integrand,
      exact = exact,
      nd = nd
    )
  }

  if (anyNA(result) || any(!is.finite(result))) {
    stop("Network interaction integral produced non-finite values.", call. = FALSE)
  }

  result
}

#' Estimate the network interaction integral
#'
#' Estimates the location-specific interaction integral
#'
#' \deqn{
#' A(u) = \int_L [g(d_L(u,s)) - 1] \, d\ell(s),
#' }
#'
#' using the inhomogeneous linear pair-correlation function estimated by
#' [spatstat.linnet::linearpcfinhom()].
#'
#' @param point_pattern An `lpp` object used to estimate the network pair-correlation function.
#' @param rmax Maximum shortest-path distance included in the interaction integral.
#' @param locations An `lpp` object containing the locations at which the
#'   interaction integral is evaluated. Defaults to `point_pattern`.
#' @param lambda Optional intensity specification passed to [spatstat.linnet::linearpcfinhom()].
#' @param correction Network geometry correction used by [spatstat.linnet::linearpcfinhom()]. Must be `"Ang"` or `"none"`.
#' @param nr Number of distances used to estimate the pair-correlation function between `0` and `rmax`.
#' @param exact Logical indicating whether the network integral should use exact segment-wise integration.
#' @param nd Number of network quadrature points when `exact = FALSE`.
#'
#' @return A numeric vector containing one estimated interaction integral for each location in `locations`.
#' @export
estimate_network_interaction_integral <- function(
    point_pattern,
    rmax,
    locations = point_pattern,
    lambda = NULL,
    correction = "Ang",
    nr = 128L,
    exact = TRUE,
    nd = 1000L
) {
  if (!inherits(point_pattern, "lpp")) {
    stop("`point_pattern` must be an `lpp` object.", call. = FALSE)
  }

  if (!inherits(locations, "lpp")) {
    stop("`locations` must be an `lpp` object.", call. = FALSE)
  }

  if (length(rmax) != 1L || !is.numeric(rmax) || !is.finite(rmax) || rmax <= 0) {
    stop("`rmax` must be one positive finite numeric value.", call. = FALSE)
  }

  if (!is.character(correction) || length(correction) != 1L || !correction %in% c("Ang", "none")) {
    stop("`correction` must be either \"Ang\" or \"none\".", call. = FALSE)
  }

  if (length(nr) != 1L || !is.numeric(nr) || !is.finite(nr) || nr < 2 || nr != as.integer(nr)) {
    stop("`nr` must be one integer greater than or equal to 2.", call. = FALSE)
  }

  if (length(exact) != 1L || !is.logical(exact) || is.na(exact)) {
    stop("`exact` must be one logical value.", call. = FALSE)
  }

  if (length(nd) != 1L || !is.numeric(nd) || !is.finite(nd) || nd <= 0 || nd != as.integer(nd)) {
    stop("`nd` must be one positive integer.", call. = FALSE)
  }

  nr <- as.integer(nr)
  nd <- as.integer(nd)
  network <- spatstat.linnet::as.linnet(point_pattern)

  if (!identical(spatstat.linnet::as.linnet(locations), network)) {
    stop("`point_pattern` and `locations` must be defined on the same linear network.", call. = FALSE)
  }

  if (spatstat.geom::npoints(locations) == 0L) {
    return(numeric(0))
  }

  r_grid <- seq(from = 0, to = rmax, length.out = nr)

  if (is.null(lambda)) {
    g_hat <- spatstat.linnet::linearpcf(
      X = point_pattern,
      r = r_grid,
      correction = correction
    )
  } else {
    g_hat <- spatstat.linnet::linearpcfinhom(
      X = point_pattern,
      lambda = lambda,
      r = r_grid,
      correction = correction
    )
  }

  g_values <- g_hat$est

  if (!is.numeric(g_values) || anyNA(g_values) || any(!is.finite(g_values))) {
    stop("Estimated network pair correlation contains non-finite values.", call. = FALSE)
  }

  g_function <- as.function(g_hat, value = "est", extrapolate = FALSE)

  pair_correlation <- function(distance) {
    value <- rep(1, length(distance))
    inside <- distance <= rmax

    if (any(inside)) {
      estimate <- g_function(distance[inside])
      if (anyNA(estimate) || any(!is.finite(estimate))) {
        stop("Network pair-correlation interpolation produced non-finite values.", call. = FALSE)
      }
      value[inside] <- estimate
    }
    value
  }

  calculate_network_interaction_integral(
    locations = locations,
    network = network,
    pair_correlation = pair_correlation,
    exact = exact,
    nd = nd
  )
}

#' Attach an estimated network interaction integral
#'
#' Estimates a location-specific network interaction integral and stores it
#' in an existing `gbpp_data` object.
#'
#' @param data A `gbpp_data` object produced by [prepare_gbpp_network()].
#' @param point_pattern An `lpp` object used to estimate the network pair-correlation function.
#' @param rmax Maximum shortest-path distance included in the interaction integral.
#' @param lambda Optional intensity specification passed to [spatstat.linnet::linearpcfinhom()].
#' @param correction Network geometry correction used by [spatstat.linnet::linearpcfinhom()].
#' @param nr Number of distances used to estimate the pair-correlation function.
#' @param exact Logical indicating whether network integration should use exact segment-wise integration.
#' @param nd Number of network quadrature points when `exact = FALSE`.
#'
#' @return The input `gbpp_data` object with `interaction_integral` attached.
#' @export
attach_network_interaction_integral <- function(
    data,
    point_pattern,
    rmax,
    lambda = NULL,
    correction = "Ang",
    nr = 128L,
    exact = TRUE,
    nd = 1000L
) {
  if (!inherits(data, "gbpp_data")) {
    stop("`data` must be a `gbpp_data` object.", call. = FALSE)
  }

  if (!identical(data$type, "network")) {
    stop("`data` must have `type = \"network\"`.", call. = FALSE)
  }

  if (!inherits(point_pattern, "lpp")) {
    stop("`point_pattern` must be an `lpp` object.", call. = FALSE)
  }

  if (is.null(data$metadata) || is.null(data$metadata$quadrature)) {
    stop("`data` does not contain the network quadrature metadata.", call. = FALSE)
  }

  quadrature <- data$metadata$quadrature
  quadrature_locations <- attr(quadrature, "situ")

  if (!inherits(quadrature_locations, "lpp")) {
    stop("Network quadrature metadata does not contain an `lpp` object.", call. = FALSE)
  }

  if (!identical(
    spatstat.linnet::as.linnet(point_pattern),
    spatstat.linnet::as.linnet(quadrature_locations)
  )) {
    stop("`point_pattern` and the network quadrature must be defined on the same linear network.", call. = FALSE)
  }

  if (spatstat.geom::npoints(quadrature_locations) != nrow(data$X)) {
    stop("Network quadrature locations must match the rows of `data$X`.", call. = FALSE)
  }

  interaction_integral <- estimate_network_interaction_integral(
    point_pattern = point_pattern,
    locations = quadrature_locations,
    rmax = rmax,
    lambda = lambda,
    correction = correction,
    nr = nr,
    exact = exact,
    nd = nd
  )

  if (length(interaction_integral) != nrow(data$X)) {
    stop("Estimated interaction integral has an invalid length.", call. = FALSE)
  }

  if (anyNA(interaction_integral) || any(!is.finite(interaction_integral))) {
    stop("Estimated interaction integral contains non-finite values.", call. = FALSE)
  }

  data$interaction_integral <- pmax(0, interaction_integral)
  validate_gbpp_data(data)
  data
}
