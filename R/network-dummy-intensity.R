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
