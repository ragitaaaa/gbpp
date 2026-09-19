#' Convert a spatstat quadrature scheme to a GBPP data object
#'
#' @param Q A quadrature scheme produced by `spatstat`.
#' @param X Covariate data corresponding to all quadrature points.
#' @param objective Objective used by the model.
#' @param type Spatial domain type: `"planar"` or `"network"`.
#' @param statistical_weight Optional statistical weights.
#'
#' @return An object of class `gbpp_data`.
#' @keywords internal
prepare_pp_data <- function(
    Q,
    X,
    objective = c(
      "poisson",
      "weighted_poisson",
      "logistic",
      "weighted_logistic"
    ),
    type = c("planar", "network"),
    statistical_weight = NULL
) {

  objective <- match.arg(objective)
  type <- match.arg(type)

  if (!is.data.frame(X) && !is.matrix(X)) {
    stop("`X` must be a data frame or matrix.", call. = FALSE)
  }

  y <- as.integer(spatstat.geom::is.data(Q))
  locations <- as.data.frame(spatstat.geom::coords(Q))
  quadrature_weight <- spatstat.geom::w.quad(Q)

  if (nrow(X) != length(y)) {
    stop(
      "`X` must have one row for each point in the quadrature scheme.",
      call. = FALSE
    )
  }

  if (length(quadrature_weight) != length(y)) {
    stop(
      "`quadrature_weight` must have one value for each point.",
      call. = FALSE
    )
  }

  if (objective %in% c("weighted_poisson", "weighted_logistic") &&
      is.null(statistical_weight)) {
    stop(
      "`statistical_weight` is required for weighted objectives.",
      call. = FALSE
    )
  }

  if (!is.null(statistical_weight) &&
      length(statistical_weight) != length(y)) {
    stop(
      "`statistical_weight` must have one value for each point.",
      call. = FALSE
    )
  }

  dummy_intensity <- NULL

  if (objective %in% c("logistic", "weighted_logistic")) {
    dummy_intensity <- extract_dummy_intensity(Q)
  }

  new_gbpp_data(
    X = X,
    y = y,
    locations = locations,
    quadrature_weight = quadrature_weight,
    dummy_intensity = dummy_intensity,
    statistical_weight = statistical_weight,
    type = type,
    metadata = list(
      objective = objective,
      source = "spatstat_quadrature"
    )
  )
}
