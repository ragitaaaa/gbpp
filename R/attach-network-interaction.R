#' Attach an estimated network interaction integral
#'
#' Estimates a location-specific network interaction integral and stores it
#' in an existing `gbpp_data` object.
#'
#' @param data A `gbpp_data` object produced by [prepare_gbpp_network()].
#' @param point_pattern An `lpp` object used to estimate the network
#'   pair-correlation function.
#' @param rmax Maximum shortest-path distance included in the interaction
#'   integral.
#' @param lambda Optional intensity specification passed to
#'   [spatstat.linnet::linearpcfinhom()].
#' @param correction Network geometry correction used by
#'   [spatstat.linnet::linearpcfinhom()].
#' @param nr Number of distances used to estimate the pair-correlation
#'   function.
#' @param exact Logical indicating whether network integration should use
#'   exact segment-wise integration.
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
    stop(
      "`data` must be a `gbpp_data` object.",
      call. = FALSE
    )
  }

  if (!identical(data$type, "network")) {
    stop(
      "`data` must have `type = \"network\"`.",
      call. = FALSE
    )
  }

  if (!inherits(point_pattern, "lpp")) {
    stop(
      "`point_pattern` must be an `lpp` object.",
      call. = FALSE
    )
  }

  if (is.null(data$metadata) ||
      is.null(data$metadata$quadrature)) {
    stop(
      "`data` does not contain the network quadrature metadata.",
      call. = FALSE
    )
  }

  quadrature <- data$metadata$quadrature
  quadrature_locations <- attr(quadrature, "situ")

  if (!inherits(quadrature_locations, "lpp")) {
    stop(
      "Network quadrature metadata does not contain an `lpp` object.",
      call. = FALSE
    )
  }

  if (!identical(
    spatstat.linnet::as.linnet(point_pattern),
    spatstat.linnet::as.linnet(quadrature_locations)
  )) {
    stop(
      paste0(
        "`point_pattern` and the network quadrature must be defined ",
        "on the same linear network."
      ),
      call. = FALSE
    )
  }

  if (spatstat.geom::npoints(quadrature_locations) !=
      nrow(data$X)) {
    stop(
      "Network quadrature locations must match the rows of `data$X`.",
      call. = FALSE
    )
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
    stop(
      "Estimated interaction integral has an invalid length.",
      call. = FALSE
    )
  }

  if (anyNA(interaction_integral) ||
      any(!is.finite(interaction_integral))) {
    stop(
      "Estimated interaction integral contains non-finite values.",
      call. = FALSE
    )
  }

  data$interaction_integral <- interaction_integral

  validate_gbpp_data(data)

  data
}
