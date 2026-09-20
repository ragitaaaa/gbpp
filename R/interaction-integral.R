#' Calculate the planar spatial interaction integral
#'
#' Calculates the planar approximation to the spatial interaction
#' integral used by the iteratively weighted GBPP objectives:
#'
#' \deqn{
#' A = K(r_{\max}) - \pi r_{\max}^2.
#' }
#'
#' Here, \eqn{K(r_{\max})} is the estimated Ripley's K-function
#' at distance \eqn{r_{\max}}.
#'
#' This function performs only the mathematical transformation.
#' Estimation of the K-function itself is handled separately.
#'
#' @param K_value Numeric scalar containing the estimated K-function
#'   value at `rmax`.
#' @param rmax Numeric scalar giving the maximum distance used for
#'   the K-function approximation.
#'
#' @return A numeric scalar containing the estimated interaction
#'   integral.
#'
#' @keywords internal
calculate_interaction_integral <- function(
    K_value,
    rmax
) {

  if (length(K_value) != 1L ||
      !is.numeric(K_value) ||
      !is.finite(K_value)) {
    stop(
      "`K_value` must be a single finite numeric value.",
      call. = FALSE
    )
  }

  if (length(rmax) != 1L ||
      !is.numeric(rmax) ||
      !is.finite(rmax) ||
      rmax <= 0) {
    stop(
      "`rmax` must be a single positive finite numeric value.",
      call. = FALSE
    )
  }

  interaction_integral <- (
    K_value -
      pi * rmax^2
  )

  if (!is.finite(interaction_integral)) {
    stop(
      "The interaction integral is not finite.",
      call. = FALSE
    )
  }

  interaction_integral
}
