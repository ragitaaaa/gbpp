#' Attach an estimated interaction integral to gbpp data
#'
#' Estimates the planar interaction integral from an observed point pattern
#' and attaches the resulting value to every row of a [`gbpp_data`] object.
#'
#' Automatic interaction-integral estimation is currently supported for
#' planar point patterns represented by a `ppp` object. For network point
#' processes, the interaction integral must currently be supplied directly
#' when constructing the [`gbpp_data`] object.
#'
#' @param data A [`gbpp_data`] object.
#' @param point_pattern A `ppp` object containing the observed event locations.
#' @param rmax Maximum distance used in the K-function estimate.
#' @param lambda Optional intensity values supplied to `Kinhom()`.
#' @param correction Edge correction used by [`spatstat.explore::Kinhom()`].
#'
#' @return The modified [`gbpp_data`] object with `interaction_integral`
#' populated.
#'
#' @export
attach_interaction_integral <- function(
    data,
    point_pattern,
    rmax,
    lambda = NULL,
    correction = c("isotropic", "translate", "border")
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
