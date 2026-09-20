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
#' @param point_pattern An `lpp` object used to estimate the network
#'   pair-correlation function.
#' @param rmax Maximum shortest-path distance included in the interaction
#'   integral.
#' @param locations An `lpp` object containing the locations at which the
#'   interaction integral is evaluated. Defaults to `point_pattern`.
#' @param lambda Optional intensity specification passed to
#'   [spatstat.linnet::linearpcfinhom()].
#' @param correction Network geometry correction used by
#'   [spatstat.linnet::linearpcfinhom()]. Must be `"Ang"` or `"none"`.
#' @param nr Number of distances used to estimate the pair-correlation
#'   function between `0` and `rmax`.
#' @param exact Logical indicating whether the network integral should use
#'   exact segment-wise integration.
#' @param nd Number of network quadrature points when `exact = FALSE`.
#'
#' @return A numeric vector containing one estimated interaction integral
#'   for each location in `locations`.
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
    stop(
      "`point_pattern` must be an `lpp` object.",
      call. = FALSE
    )
  }

  if (!inherits(locations, "lpp")) {
    stop(
      "`locations` must be an `lpp` object.",
      call. = FALSE
    )
  }

  if (length(rmax) != 1L ||
      !is.numeric(rmax) ||
      !is.finite(rmax) ||
      rmax <= 0) {
    stop(
      "`rmax` must be one positive finite numeric value.",
      call. = FALSE
    )
  }

  if (!is.character(correction) ||
      length(correction) != 1L ||
      !correction %in% c("Ang", "none")) {
    stop(
      "`correction` must be either \"Ang\" or \"none\".",
      call. = FALSE
    )
  }

  if (length(nr) != 1L ||
      !is.numeric(nr) ||
      !is.finite(nr) ||
      nr < 2 ||
      nr != as.integer(nr)) {
    stop(
      "`nr` must be one integer greater than or equal to 2.",
      call. = FALSE
    )
  }

  if (length(exact) != 1L ||
      !is.logical(exact) ||
      is.na(exact)) {
    stop(
      "`exact` must be one logical value.",
      call. = FALSE
    )
  }

  if (length(nd) != 1L ||
      !is.numeric(nd) ||
      !is.finite(nd) ||
      nd <= 0 ||
      nd != as.integer(nd)) {
    stop(
      "`nd` must be one positive integer.",
      call. = FALSE
    )
  }

  nr <- as.integer(nr)
  nd <- as.integer(nd)

  network <- spatstat.linnet::as.linnet(point_pattern)

  if (!identical(
    spatstat.linnet::as.linnet(locations),
    network
  )) {
    stop(
      paste0(
        "`point_pattern` and `locations` must be defined on the same ",
        "linear network."
      ),
      call. = FALSE
    )
  }

  if (spatstat.geom::npoints(locations) == 0L) {
    return(numeric(0))
  }

  r_grid <- seq(
    from = 0,
    to = rmax,
    length.out = nr
  )

  g_hat <- spatstat.linnet::linearpcfinhom(
    X = point_pattern,
    lambda = lambda,
    r = r_grid,
    correction = correction
  )

  g_values <- g_hat$est

  if (!is.numeric(g_values) ||
      anyNA(g_values) ||
      any(!is.finite(g_values))) {
    stop(
      "Estimated network pair correlation contains non-finite values.",
      call. = FALSE
    )
  }

  g_function <- as.function(
    g_hat,
    value = "est",
    extrapolate = FALSE
  )

  pair_correlation <- function(distance) {
    value <- rep(1, length(distance))

    inside <- distance <= rmax

    if (any(inside)) {
      estimate <- g_function(distance[inside])

      if (anyNA(estimate) ||
          any(!is.finite(estimate))) {
        stop(
          "Network pair-correlation interpolation produced non-finite values.",
          call. = FALSE
        )
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
