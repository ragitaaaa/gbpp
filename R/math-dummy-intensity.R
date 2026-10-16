#' Extract dummy-process intensity from a logistic quadrature scheme
#'
#' @param Q A logistic quadrature scheme from `spatstat`.
#'
#' @return A numeric vector with one dummy-process intensity value for each quadrature point.
#' @export
extract_dummy_intensity <- function(Q) {

  if (!inherits(Q, "quad")) {
    stop(
      "`Q` must be a spatstat quadrature scheme.",
      call. = FALSE
    )
  }

  n <- length(spatstat.geom::is.data(Q))
  rho <- Q$param$rho

  if (is.null(rho)) {
    stop(
      paste0(
        "Dummy-process intensity could not be extracted from `Q`. ",
        "Provide it explicitly for quadrature schemes that do not ",
        "store `rho` in `Q$param$rho`."
      ),
      call. = FALSE
    )
  }

  if (!is.numeric(rho)) {
    stop(
      "`Q$param$rho` must be numeric.",
      call. = FALSE
    )
  }

  if (any(!is.finite(rho)) || any(rho <= 0)) {
    stop(
      "Dummy-process intensity must contain positive finite values.",
      call. = FALSE
    )
  }

  if (length(rho) == 1L) {
    rho <- rep(rho, n)
  }

  if (length(rho) != n) {
    stop(
      paste0(
        "Dummy-process intensity must either be a single value ",
        "or have one value for each quadrature point."
      ),
      call. = FALSE
    )
  }

  rho
}

#' Calculate the effective dummy intensity for a planar quadrature scheme
#'
#' Calculates the homogeneous dummy intensity implied by a planar
#' Berman-Turner quadrature scheme:
#'
#' \deqn{
#' \delta = \frac{m}{|W|}
#' }
#'
#' where `m` is the number of dummy points and `|W|` is the total window
#' area represented by the observation window (or quadrature weights).
#'
#' @param y Binary response vector with `1` for data points and `0` for
#'   dummy points.
#' @param quadrature_weight Positive Berman-Turner quadrature weights, or
#'   an observation window (`owin`).
#'
#' @return A positive numeric scalar giving the effective homogeneous dummy
#'   intensity per unit area.
#' @export
calculate_planar_dummy_intensity <- function(
    y,
    quadrature_weight
) {
  if (!is.numeric(y) ||
      length(y) == 0L ||
      anyNA(y) ||
      any(!is.finite(y)) ||
      any(!y %in% c(0, 1))) {
    stop(
      "`y` must be a non-empty finite binary numeric vector.",
      call. = FALSE
    )
  }

  if (inherits(quadrature_weight, "owin")) {
    window_area <- spatstat.geom::area(quadrature_weight)
  } else {
    if (!is.numeric(quadrature_weight) ||
        length(quadrature_weight) != length(y) ||
        anyNA(quadrature_weight) ||
        any(!is.finite(quadrature_weight)) ||
        any(quadrature_weight <= 0)) {
      stop(
        "`quadrature_weight` must be a positive finite numeric vector with the same length as `y`, or an `owin` window.",
        call. = FALSE
      )
    }
    window_area <- sum(quadrature_weight)
  }

  n_dummy <- sum(y == 0)

  if (n_dummy <= 0) {
    stop(
      "At least one dummy point is required.",
      call. = FALSE
    )
  }

  if (!is.finite(window_area) || window_area <= 0) {
    stop(
      "The total window area must be positive and finite.",
      call. = FALSE
    )
  }

  dummy_intensity <- n_dummy / window_area

  if (!is.finite(dummy_intensity) || dummy_intensity <= 0) {
    stop(
      "Failed to calculate a positive finite dummy intensity.",
      call. = FALSE
    )
  }

  dummy_intensity
}

#' Calculate the effective dummy intensity for a network quadrature scheme
#'
#' Calculates the homogeneous dummy intensity implied by a linear-network
#' quadrature scheme:
#'
#' \deqn{
#' \delta = \frac{m}{|L|}
#' }
#'
#' where `m` is the number of dummy points and `|L|` is the total network
#' length represented by the quadrature weights.
#'
#' @param y Binary response vector with `1` for data points and `0` for
#'   dummy points.
#' @param quadrature_weight Positive Berman-Turner quadrature weights.
#'
#' @return A positive numeric scalar giving the effective homogeneous dummy
#'   intensity per unit network length.
#' @export
calculate_network_dummy_intensity <- function(
    y,
    quadrature_weight
) {
  if (!is.numeric(y) ||
      length(y) == 0L ||
      anyNA(y) ||
      any(!is.finite(y)) ||
      any(!y %in% c(0, 1))) {
    stop(
      "`y` must be a non-empty finite binary numeric vector.",
      call. = FALSE
    )
  }

  if (!is.numeric(quadrature_weight) ||
      length(quadrature_weight) != length(y) ||
      anyNA(quadrature_weight) ||
      any(!is.finite(quadrature_weight)) ||
      any(quadrature_weight <= 0)) {
    stop(
      "`quadrature_weight` must be a positive finite numeric vector with the same length as `y`.",
      call. = FALSE
    )
  }

  n_dummy <- sum(y == 0)
  network_length <- sum(quadrature_weight)

  if (n_dummy <= 0) {
    stop(
      "At least one dummy point is required.",
      call. = FALSE
    )
  }

  if (!is.finite(network_length) || network_length <= 0) {
    stop(
      "The total quadrature length must be positive and finite.",
      call. = FALSE
    )
  }

  dummy_intensity <- n_dummy / network_length

  if (!is.finite(dummy_intensity) || dummy_intensity <= 0) {
    stop(
      "Failed to calculate a positive finite dummy intensity.",
      call. = FALSE
    )
  }

  dummy_intensity
}
