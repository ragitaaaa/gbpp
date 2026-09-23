#' Plot GBPP Cross-Validation Diagnostics
#'
#' Generates a two-panel diagnostic plot for stochastic thinning cross-validation results.
#' The left panel displays the distribution of out-of-sample deviance across folds,
#' indicating model stability. The right panel displays the expected versus observed number of events
#' for each fold, indicating the calibration of the spatial integral compensator.
#'
#' @param x An object of class `gbpp_cv`.
#' @param ... Additional arguments passed to the underlying base R plot functions.
#'
#' @return Invisibly returns `x`.
#' @export
plot.gbpp_cv <- function(x, ...) {
  if (!inherits(x, "gbpp_cv")) {
    stop("`x` must be a `gbpp_cv` object.", call. = FALSE)
  }
  
  folds <- length(x$folds)
  dev <- vapply(x$folds, function(f) f$deviance, numeric(1))
  exp_ev <- vapply(x$folds, function(f) f$expected_events, numeric(1))
  obs_ev <- vapply(x$folds, function(f) f$observed_events, numeric(1))
  
  old_par <- graphics::par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
  on.exit(graphics::par(old_par))
  
  # Panel 1: Stability (Boxplot of Deviance)
  graphics::boxplot(
    dev,
    main = "Model Stability",
    ylab = "Out-of-Sample Poisson Deviance",
    col = "lightblue",
    border = "darkblue",
    pch = 16
  )
  
  # Panel 2: Calibration (Expected vs Observed Events)
  # Calculate symmetrical axis limits to accommodate the 1:1 line perfectly
  axis_lims <- range(c(obs_ev, exp_ev))
  # Add small buffer to avoid points on the very edge
  buffer <- diff(axis_lims) * 0.1
  if (buffer == 0) buffer <- 1
  axis_lims <- c(axis_lims[1] - buffer, axis_lims[2] + buffer)
  
  graphics::plot(
    obs_ev, exp_ev,
    main = "Spatial Calibration",
    xlab = "Observed Test Events",
    ylab = "Expected Test Events",
    pch = 16,
    col = "darkorange",
    xlim = axis_lims,
    ylim = axis_lims
  )
  graphics::abline(0, 1, lty = 2, col = "black", lwd = 2)
  
  graphics::title("Stochastic Thinning CV Diagnostics", outer = TRUE, cex.main = 1.5)
  
  invisible(x)
}
