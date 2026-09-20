#' Estimate the planar interaction integral from an inhomogeneous K-function
#'
#' Estimates the spatial interaction integral used by the iteratively
#' weighted GBPP objectives. The inhomogeneous K-function is evaluated
#' at `rmax` and transformed using
#'
#' \deqn{
#' A = K(r_{\max}) - \pi r_{\max}^2.
#' }
#'
#' The resulting scalar represents a common planar interaction
#' integral for the point pattern.
#'
#' @param point_pattern A planar `spatstat` point pattern (`ppp`).
#' @param rmax Maximum distance at which the inhomogeneous K-function
#'   is evaluated.
#' @param lambda Optional numeric vector of estimated intensity values
#'   at the observed points. If omitted, `Kinhom()` estimates the
#'   intensity using its default leave-one-out kernel procedure.
#' @param correction Edge correction used by `Kinhom()`. One of
#'   `"isotropic"`, `"translate"`, or `"border"`.
#'
#' @return A numeric scalar containing the estimated interaction
#'   integral.
#'
#' @keywords internal
estimate_interaction_integral <- function(
    point_pattern,
    rmax,
    lambda = NULL,
    correction = c(
      "isotropic",
      "translate",
      "border"
    )
) {

  correction <- match.arg(correction)

  if (!inherits(point_pattern, "ppp")) {
    stop(
      "`point_pattern` must be a planar `spatstat` point pattern (`ppp`).",
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

  if (!is.null(lambda)) {

    if (!is.numeric(lambda)) {
      stop(
        "`lambda` must be numeric.",
        call. = FALSE
      )
    }

    if (length(lambda) != spatstat.geom::npoints(point_pattern)) {
      stop(
        "`lambda` must contain one value for each point in `point_pattern`.",
        call. = FALSE
      )
    }

    if (any(!is.finite(lambda)) ||
        any(lambda <= 0)) {
      stop(
        "`lambda` must contain positive finite values.",
        call. = FALSE
      )
    }
  }

  # Kinhom() requires at least two r values.
  r_values <- seq(
    from = 0,
    to = as.numeric(rmax),
    length.out = 2L
  )

  stopifnot(
    length(r_values) == 2L,
    is.numeric(r_values),
    identical(unname(r_values[1L]), 0),
    r_values[2L] > 0
  )

  K <- if (is.null(lambda)) {

    spatstat.explore::Kinhom(
      point_pattern,
      r = r_values,
      correction = correction
    )

  } else {

    spatstat.explore::Kinhom(
      point_pattern,
      lambda = lambda,
      r = r_values,
      correction = correction
    )
  }

  K_data <- as.data.frame(K)

  correction_column <- switch(
    correction,
    isotropic = "iso",
    translate = "trans",
    border = "border"
  )

  if (!correction_column %in% names(K_data)) {
    stop(
      paste0(
        "The requested K-function correction `",
        correction,
        "` was not returned by `Kinhom()`."
      ),
      call. = FALSE
    )
  }

  r_index <- which.min(
    abs(
      K_data$r - rmax
    )
  )

  K_value <- K_data[[correction_column]][r_index]

  if (length(K_value) != 1L ||
      !is.finite(K_value)) {
    stop(
      "The K-function estimate at `rmax` is not finite.",
      call. = FALSE
    )
  }

  calculate_interaction_integral(
    K_value = K_value,
    rmax = rmax
  )
}
