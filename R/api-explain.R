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

#' Print TreeSHAP Explanation Summary
#'
#' @param x A `gbpp_explanation` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#' @export
print.gbpp_explanation <- function(x, ...) {
  cat("TreeSHAP Model Explanation\n")
  cat("--------------------------\n")
  cat("Evaluation samples :", nrow(x$shap), "\n")
  cat("Number of features :", ncol(x$shap), "\n")
  cat("Baseline bias      :", round(mean(x$bias), 4), "\n")
  cat("\nMean absolute SHAP values (Feature Importance):\n")
  mean_abs <- colMeans(abs(x$shap))
  mean_abs <- sort(mean_abs, decreasing = TRUE)
  for (i in seq_along(mean_abs)) {
    cat(sprintf("  %-16s: %.4f\n", names(mean_abs)[i], mean_abs[i]))
  }
  invisible(x)
}

#' Plot TreeSHAP Explanations
#'
#' Visualizes feature importance barplots or partial dependence scatter plots
#' derived from TreeSHAP values.
#'
#' @param x A `gbpp_explanation` object.
#' @param type Type of plot: `"importance"` for global mean absolute SHAP barplot, or
#'   `"dependence"` for feature dependence curve. Default is `"importance"`.
#' @param feature Name of feature to plot when `type = "dependence"`. Defaults to the most important feature.
#' @param ... Additional arguments passed to base plotting functions.
#'
#' @return Invisibly returns `x`.
#' @export
plot.gbpp_explanation <- function(x, type = c("importance", "dependence"), feature = NULL, ...) {
  type <- match.arg(type)

  if (type == "importance") {
    imp <- colMeans(abs(x$shap))
    imp <- sort(imp, decreasing = FALSE)
    graphics::barplot(
      imp,
      horiz = TRUE,
      las = 1,
      col = "steelblue",
      border = NA,
      xlab = "Mean |SHAP value|",
      main = "Feature Importance (TreeSHAP)",
      ...
    )
  } else if (type == "dependence") {
    if (is.null(feature)) {
      imp <- colMeans(abs(x$shap))
      feature <- names(sort(imp, decreasing = TRUE))[1]
    }
    if (!feature %in% colnames(x$shap)) {
      stop(sprintf("Feature '%s' not found in explanation data.", feature), call. = FALSE)
    }

    feat_x <- x$data[[feature]]
    shap_y <- x$shap[, feature]

    if (is.factor(feat_x) || is.character(feat_x)) {
      graphics::boxplot(
        shap_y ~ feat_x,
        xlab = feature,
        ylab = paste("SHAP value for", feature),
        col = "lightblue",
        main = paste("TreeSHAP Dependence:", feature),
        ...
      )
    } else {
      graphics::plot(
        feat_x,
        shap_y,
        xlab = feature,
        ylab = paste("SHAP value for", feature),
        pch = 16,
        col = grDevices::rgb(0.2, 0.4, 0.8, 0.4),
        main = paste("TreeSHAP Dependence:", feature),
        ...
      )
      if (length(unique(feat_x)) > 5) {
        fit_smooth <- stats::lowess(feat_x, shap_y)
        graphics::lines(fit_smooth, col = "red", lwd = 2)
      }
    }
  }

  invisible(x)
}

