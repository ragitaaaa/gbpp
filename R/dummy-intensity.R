#' Extract dummy-process intensity from a logistic quadrature scheme
#'
#' @param Q A logistic quadrature scheme.
#'
#' @return A numeric vector with one dummy-process intensity value for each quadrature point.
#' @keywords internal

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
