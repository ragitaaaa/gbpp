#' Extract TreeSHAP values for a GBPP model
#'
#' @param object A `gbpp_model` object.
#' @param newdata A data frame of covariate values to explain.
#' @param ... Additional arguments.
#'
#' @return A list containing `shap` (a matrix of feature contributions) and `bias` (the expected value).
#' @export
.gbpp_shap <- function(object, newdata, ...) {
  UseMethod(".gbpp_shap")
}
