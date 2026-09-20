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
#'   method provided by [spatstat.linnet::integral()]. If `FALSE`, use network
#'   quadrature with `nd` sample locations.
#' @param nd Approximate number of network integration points when
#'   `exact = FALSE`.
#'
#' @return A numeric vector containing one interaction integral for each
#'   location in `locations`.
#'
#' @export
calculate_network_interaction_integral <- function(
    locations,
    network,
    pair_correlation,
    exact = FALSE,
    nd = 1000L
) {
  if (!inherits(locations, "lpp")) {
    stop(
      "`locations` must be an `lpp` object.",
      call. = FALSE
    )
  }

  if (!inherits(network, "linnet")) {
    stop(
      "`network` must be a `linnet` object.",
      call. = FALSE
    )
  }

  if (!is.function(pair_correlation)) {
    stop(
      "`pair_correlation` must be a function.",
      call. = FALSE
    )
  }

  if (length(exact) != 1L ||
      is.na(exact) ||
      !is.logical(exact)) {
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

  nd <- as.integer(nd)

  location_network <- spatstat.linnet::as.linnet(locations)

  if (!identical(location_network, network)) {
    stop(
      paste0(
        "`locations` and `network` must be defined on the same ",
        "linear network."
      ),
      call. = FALSE
    )
  }

  n_locations <- spatstat.geom::npoints(locations)

  if (n_locations == 0L) {
    return(numeric(0))
  }

  result <- numeric(n_locations)

  for (i in seq_len(n_locations)) {
    location <- locations[i]

    distance_function <- spatstat.geom::distfun(
      location
    )

    integrand <- spatstat.linnet::linfun(
      function(x, y, seg, tp) {
        distance <- distance_function(
          x,
          y,
          seg = seg,
          tp = tp
        )

        value <- pair_correlation(distance)

        if (length(value) != length(distance)) {
          stop(
            "`pair_correlation` must return one value for each distance.",
            call. = FALSE
          )
        }

        if (!is.numeric(value) ||
            anyNA(value) ||
            any(!is.finite(value))) {
          stop(
            "`pair_correlation` must return finite numeric values.",
            call. = FALSE
          )
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
    stop(
      "Network interaction integral produced non-finite values.",
      call. = FALSE
    )
  }

  result
}
