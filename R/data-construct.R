#' Construct a GBPP data object
#'
#' Creates an S3 object of class `gbpp_data` encapsulating observed points,
#' quadrature background dummy points, covariates, weights, and domain metadata.
#'
#' @param X Covariate data (matrix or data frame).
#' @param y Event/dummy indicator, where 1 denotes observed event and 0 denotes a dummy point.
#' @param locations Spatial locations corresponding to the rows of `X`.
#' @param quadrature_weight Berman-Turner quadrature weights.
#' @param dummy_intensity Dummy-process intensity for logistic objectives.
#' @param statistical_weight Spatial/dependence weights for weighted objectives.
#' @param interaction_integral Spatial interaction integral for iteratively weighted objectives.
#' @param window Observation window (`owin`) or linear network (`linnet`).
#' @param type Spatial domain type: `"planar"` or `"network"`.
#' @param metadata Additional arbitrary metadata.
#'
#' @return An object of class `gbpp_data`.
#' @export
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

#' Print a GBPP data object
#'
#' @param x A `gbpp_data` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#' @export
print.gbpp_data <- function(x, ...) {
  n_events <- sum(x$y == 1)
  n_dummy <- sum(x$y == 0)
  n_covariates <- if (is.data.frame(x$X) || is.matrix(x$X)) ncol(x$X) else 0L

  cat("GBPP Point Process Data Object\n")
  cat("------------------------------\n")
  cat("Domain type        :", x$type, "\n")
  cat("Total observations :", length(x$y), "\n")
  cat("  - Events (y = 1) :", n_events, "\n")
  cat("  - Dummy  (y = 0) :", n_dummy, "\n")
  cat("Covariates (p)     :", n_covariates, "\n")
  if (n_covariates > 0L) {
    cat("Covariate names    :", paste(colnames(x$X), collapse = ", "), "\n")
  }
  cat("Quadrature weights :", if (!is.null(x$quadrature_weight)) "Present" else "None", "\n")
  cat("Dummy intensity    :", if (!is.null(x$dummy_intensity)) "Present" else "None", "\n")
  cat("Statistical weights:", if (!is.null(x$statistical_weight)) "Present" else "None", "\n")
  cat("Interaction int.   :", if (!is.null(x$interaction_integral)) "Present" else "None", "\n")
  invisible(x)
}

#' Attach statistical weights to a GBPP data object
#'
#' Attaches custom statistical weights (e.g., from pilot kernel estimates,
#' local LISA summary statistics, or inverse probability weighting) to an
#' existing `gbpp_data` object for use with weighted objectives
#' (`"weighted_poisson"` or `"weighted_logistic"`).
#'
#' @param data A `gbpp_data` object.
#' @param weights A numeric vector of positive weights with length equal to
#'   the total number of observations (`length(data$y)`).
#'
#' @return The modified `gbpp_data` object with `statistical_weight` attached.
#' @export
attach_statistical_weight <- function(data, weights) {
  if (!inherits(data, "gbpp_data")) {
    stop("`data` must be a `gbpp_data` object.", call. = FALSE)
  }

  if (!is.numeric(weights) || length(weights) != length(data$y)) {
    stop(
      "`weights` must be a numeric vector with length equal to the number of quadrature points.",
      call. = FALSE
    )
  }

  if (any(!is.finite(weights)) || any(weights <= 0)) {
    stop(
      "`weights` must contain only strictly positive finite values.",
      call. = FALSE
    )
  }

  data$statistical_weight <- weights
  data
}
