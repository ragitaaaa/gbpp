#' Calculate iteratively updated Poisson weights
#'
#' Calculates the spatial dependence-adjusted weights used by the
#' iteratively weighted Poisson formulation. For a current log-intensity
#' prediction \eqn{\phi_i}, the corresponding intensity is
#' \eqn{\lambda_i = \exp(\phi_i)} and the weight is
#'
#' \deqn{
#' w_i =
#' \frac{1}{1 + \lambda_i A_i},
#' }
#'
#' where \eqn{A_i} denotes the spatial interaction integral
#' \eqn{\int_B [g(u_i-s)-1]\,ds}.
#'
#' @param prediction Numeric vector of current predicted log-intensities.
#' @param interaction_integral Numeric vector containing the spatial
#'   interaction integral for each quadrature point.
#'
#' @return A numeric vector of iteratively updated weights.
#'
#' @keywords internal
calculate_iterative_poisson_weight <- function(
    prediction,
    interaction_integral
) {

  if (!is.numeric(prediction)) {
    stop(
      "`prediction` must be numeric.",
      call. = FALSE
    )
  }

  if (!is.numeric(interaction_integral)) {
    stop(
      "`interaction_integral` must be numeric.",
      call. = FALSE
    )
  }

  if (length(prediction) != length(interaction_integral)) {
    stop(
      "`prediction` and `interaction_integral` must have the same length.",
      call. = FALSE
    )
  }

  if (any(!is.finite(prediction))) {
    stop(
      "`prediction` must contain only finite values.",
      call. = FALSE
    )
  }

  if (any(!is.finite(interaction_integral))) {
    stop(
      "`interaction_integral` must contain only finite values.",
      call. = FALSE
    )
  }

  phi <- pmin(
    pmax(prediction, -50),
    50
  )

  lambda <- exp(phi)

  denominator <- (
    1 +
      lambda * interaction_integral
  )

  if (any(denominator <= 0)) {
    stop(
      paste0(
        "The iterative Poisson weight is undefined for at least ",
        "one observation because `1 + lambda * interaction_integral` ",
        "is not positive."
      ),
      call. = FALSE
    )
  }

  1 / denominator
}

#' Calculate iteratively updated logistic weights
#'
#' Calculates the spatial dependence-adjusted weights used by the
#' iteratively weighted logistic formulation.
#'
#' @param prediction Numeric vector of current predicted
#'   log-intensities.
#' @param dummy_intensity Numeric vector of dummy-process intensities.
#' @param interaction_integral Numeric vector containing the spatial
#'   interaction integral for each quadrature point.
#'
#' @return A numeric vector of iteratively updated logistic weights.
#'
#' @keywords internal
calculate_iterative_logistic_weight <- function(
    prediction,
    dummy_intensity,
    interaction_integral
) {

  if (!is.numeric(prediction)) {
    stop(
      "`prediction` must be numeric.",
      call. = FALSE
    )
  }

  if (!is.numeric(dummy_intensity)) {
    stop(
      "`dummy_intensity` must be numeric.",
      call. = FALSE
    )
  }

  if (!is.numeric(interaction_integral)) {
    stop(
      "`interaction_integral` must be numeric.",
      call. = FALSE
    )
  }

  if (length(prediction) != length(dummy_intensity) ||
      length(prediction) != length(interaction_integral)) {
    stop(
      "`prediction`, `dummy_intensity`, and `interaction_integral` ",
      "must have the same length.",
      call. = FALSE
    )
  }

  if (any(!is.finite(prediction))) {
    stop(
      "`prediction` must contain only finite values.",
      call. = FALSE
    )
  }

  if (any(!is.finite(dummy_intensity)) ||
      any(dummy_intensity <= 0)) {
    stop(
      "`dummy_intensity` must contain positive finite values.",
      call. = FALSE
    )
  }

  if (any(!is.finite(interaction_integral))) {
    stop(
      "`interaction_integral` must contain only finite values.",
      call. = FALSE
    )
  }

  phi <- pmin(
    pmax(prediction, -50),
    50
  )

  lambda <- exp(phi)

  denominator <- (
    dummy_intensity *
      (
        1 +
          lambda * interaction_integral
      )
  )

  if (any(denominator <= 0)) {
    stop(
      paste0(
        "The iterative Logistic weight is undefined for at least ",
        "one observation because the denominator is not positive."
      ),
      call. = FALSE
    )
  }

  (
    lambda + dummy_intensity
  ) / denominator
}
