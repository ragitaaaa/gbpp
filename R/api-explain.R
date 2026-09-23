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

#' Extract Model Explanations (SHAP)
#'
#' Extracts Shapley Additive Explanations (TreeSHAP) from a fitted GBPP model.
#' Following the theory of spatial point processes, the background reference distribution
#' should typically represent the entire spatial domain (quadrature dummy points), not just
#' the observed event locations.
#'
#' @param object A fitted `gbpp_model` object.
#' @param data A `gbpp_data` object used for training or prediction.
#' @param background Character string specifying which subset of `data` to use as the background
#'   reference for SHAP calculations. Options are `"quadrature"` (default, uses dummy points representing
#'   the spatial domain), `"events"` (uses only observed event points), or `"all"` (uses all points).
#' @param ... Additional arguments passed to methods.
#'
#' @return A list containing:
#'   \item{shap}{A matrix of SHAP values for each feature on the additive link scale.}
#'   \item{bias}{A numeric vector of the expected baseline prediction.}
#'   \item{data}{The filtered covariate data frame corresponding to the extracted SHAP values.}
#'
#' @export
explain_gbpp <- function(
  object,
  data,
  background = c("quadrature", "events", "all"),
  ...
) {
  if (!inherits(object, "gbpp_model")) {
    stop("`object` must be a fitted gbpp_model.", call. = FALSE)
  }
  if (!inherits(data, "gbpp_data")) {
    stop("`data` must be a gbpp_data object.", call. = FALSE)
  }
  
  background <- match.arg(background)
  
  X <- data$X
  y <- data$y
  
  idx <- switch(
    background,
    quadrature = which(y == 0),
    events = which(y == 1),
    all = seq_len(nrow(X))
  )
  
  if (length(idx) == 0L) {
    stop("No points found for the requested background subset.", call. = FALSE)
  }
  
  X_bg <- X[idx, , drop = FALSE]
  
  res <- .gbpp_shap(object, X_bg, ...)
  res$data <- X_bg
  
  class(res) <- c("gbpp_explanation", class(res))
  res
}

