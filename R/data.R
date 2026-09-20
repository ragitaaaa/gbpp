#' Title
#'
#' @param X Covariate data.
#' @param y Event/dummy indicator, where 1 denotes observed event and 0 denotes a dummy point.
#' @param locations Spatial locations corresponding to the rows of `X`.
#' @param quadrature_weight Berman-Turner quadrature weights.
#' @param dummy_intensity Dummy-process intensity for logistic objectives.
#' @param statistical_weight Spatial/dependence weights for weighted objectives.
#' @param interaction_integral Integral part for dynamic weighted likelihood loss.
#' @param window Observation window or linear network.
#' @param type Spatial domain type: `"planar"` or `"network"`.
#' @param metadata Additional metadata.
#'
#' @returns An object of class `gbpp_data`.
#' @keywords internal
#' @aliases gbpp_data
new_gbpp_data <- function(
    X,
    y,
    locations,
    quadrature_weight = NULL,
    dummy_intensity = NULL,
    statistical_weight = NULL,
    interaction_integral = NULL,
    window = NULL,
    type = c("planar", "network"),
    metadata = list()
) {
  type <- match.arg(type)

  structure(
    list(
      X = X,
      y = y,
      locations = locations,
      quadrature_weight = quadrature_weight,
      dummy_intensity = dummy_intensity,
      statistical_weight = statistical_weight,
      interaction_integral = interaction_integral,
      window = window,
      type = type,
      metadata = metadata
    ),
    class = "gbpp_data"
  )
}
