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
#' @param K_value Numeric scalar containing the estimated K-function value at `rmax`.
#' @param rmax Numeric scalar giving the maximum distance used for the K-function approximation.
#'
#' @return A numeric scalar containing the estimated interaction integral.
#' @export
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

  interaction_integral <- K_value - pi * rmax^2

  if (!is.finite(interaction_integral)) {
    stop(
      "The interaction integral is not finite.",
      call. = FALSE
    )
  }

  interaction_integral
}

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
#' @param point_pattern A planar `spatstat` point pattern (`ppp`).
#' @param rmax Maximum distance at which the inhomogeneous K-function is evaluated.
#' @param lambda Optional numeric vector of estimated intensity values
#'   at the observed points. If omitted, `Kinhom()` estimates the
#'   intensity using its default leave-one-out kernel procedure.
#' @param correction Edge correction used by `Kinhom()`. One of
#'   `"isotropic"`, `"translate"`, or `"border"`.
#'
#' @return A numeric scalar containing the estimated interaction integral.
#' @export
estimate_interaction_integral <- function(
    point_pattern,
    rmax,
    lambda = NULL,
    correction = c(
      "translate",
      "isotropic",
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

  if (correction == "isotropic" && spatstat.geom::is.mask(spatstat.geom::Window(point_pattern))) {
    correction <- "translate"
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

#' Attach an estimated interaction integral to gbpp data
#'
#' Estimates the planar interaction integral from an observed point pattern
#' and attaches the resulting value to every row of a [`gbpp_data`] object.
#'
#' @param data A [`gbpp_data`] object.
#' @param point_pattern A `ppp` object containing the observed event locations.
#' @param rmax Maximum distance used in the K-function estimate.
#' @param lambda Optional intensity values supplied to `Kinhom()`.
#' @param correction Edge correction used by [`spatstat.explore::Kinhom()`].
#'
#' @return The modified [`gbpp_data`] object with `interaction_integral` populated.
#' @export
attach_interaction_integral <- function(
    data,
    point_pattern,
    rmax,
    lambda = NULL,
    correction = c("translate", "isotropic", "border")
) {
  correction <- match.arg(correction)

  if (!inherits(data, "gbpp_data")) {
    stop("`data` must be a `gbpp_data` object.", call. = FALSE)
  }

  if (!inherits(point_pattern, "ppp")) {
    stop("`point_pattern` must be a `ppp` object.", call. = FALSE)
  }

  n_events <- sum(data$y == 1)
  n_points <- spatstat.geom::npoints(point_pattern)

  if (n_events != n_points) {
    stop(
      paste0(
        "`data` contains ", n_events,
        " event(s), but `point_pattern` contains ",
        n_points, " point(s)."
      ),
      call. = FALSE
    )
  }

  interaction_integral <- estimate_interaction_integral(
    point_pattern = point_pattern,
    rmax = rmax,
    lambda = lambda,
    correction = correction
  )

  data$interaction_integral <- rep(
    interaction_integral,
    length.out = length(data$y)
  )

  data
}
