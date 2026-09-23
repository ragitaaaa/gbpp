#' Plot continuous spatial predictions from a fitted GBPP model
#'
#' Generates a spatial intensity or link scale map using the `spatstat` plotting engine.
#' 
#' @param x A `gbpp_model` object.
#' @param newdata A list of spatial covariates (`im` or `linim`) or a spatial window/network.
#' @param type Type of prediction to plot: `"intensity"` (default) or `"link"`.
#' @param overlay_points Logical; if `TRUE`, overlays the provided point pattern.
#' @param pts A point pattern (`ppp` or `lpp`) to overlay if `overlay_points = TRUE`.
#' @param eps Spatial resolution.
#' @param dimyx Spatial resolution for planar grids.
#' @param ... Additional graphical arguments passed to `plot.im` or `plot.linim`.
#'
#' @return Invisibly returns the plotted `im` or `linim` object.
#' @export
plot.gbpp_model <- function(
    x,
    newdata,
    type = c("intensity", "link"),
    overlay_points = FALSE,
    pts = NULL,
    eps = NULL,
    dimyx = NULL,
    ...
) {
  type <- match.arg(type)
  
  if (missing(newdata)) {
    stop("`newdata` must be supplied to generate a continuous spatial plot.", call. = FALSE)
  }

  is_network <- inherits(newdata, "linnet") || 
                (is.list(newdata) && length(newdata) > 0 && all(vapply(newdata, inherits, logical(1), "linim")))

  pred_type <- if (is_network) "linim" else "im"
  
  # Predict the spatial surface
  # Our prediction pipelines currently default to mapping the "intensity".
  spatial_obj <- predict(x, newdata = newdata, type = pred_type, eps = eps, dimyx = dimyx)
  
  # Map to link scale if requested
  if (type == "link") {
    # spatstat supports Math.im and Math.linim generics, so log() works natively
    spatial_obj <- log(spatial_obj)
  }

  graphics::plot(spatial_obj, ...)
  
  if (overlay_points) {
    if (is.null(pts)) {
      warning("`pts` must be provided to overlay points.", call. = FALSE)
    } else {
      graphics::plot(pts, add = TRUE, col = "black", pch = 16, cex = 0.5)
    }
  }
  
  invisible(spatial_obj)
}
