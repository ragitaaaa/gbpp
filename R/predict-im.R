#' Internal function to map GBPP predictions to a spatstat im object
#'
#' @param object A `gbpp_model` object.
#' @param newdata A list of `spatstat.geom::im` objects or an `owin` object.
#' @param eps Optional spatial resolution (grid spacing).
#' @param dimyx Optional spatial resolution (dimensions).
#' @param ... Additional arguments passed to predict.
#'
#' @return A `spatstat.geom::im` object.
#' @keywords internal
.gbpp_predict_im <- function(object, newdata, eps = NULL, dimyx = NULL, ...) {
  if (missing(newdata) || is.null(newdata)) {
    stop("`newdata` must be a list of `spatstat.geom::im` objects or an `owin` window.", call. = FALSE)
  }

  if (inherits(newdata, "owin")) {
    window <- newdata
    covariates <- list()
  } else if (is.list(newdata) && length(newdata) > 0 && all(vapply(newdata, inherits, logical(1), "im"))) {
    window <- spatstat.geom::Window(newdata[[1]])
    covariates <- newdata
  } else {
    stop("`newdata` for type = 'im' must be a list of `im` objects or an `owin` window.", call. = FALSE)
  }

  feature_names <- attr(object, "feature_names")
  if (is.null(feature_names)) feature_names <- object$feature_names

  if (length(feature_names) > 0) {
    required_covs <- setdiff(feature_names, c("x", "y"))
    if (length(required_covs) > 0) {
      if (!all(required_covs %in% names(covariates))) {
        missing_covs <- setdiff(required_covs, names(covariates))
        stop(sprintf("Missing required covariates in `newdata`: %s", 
                     paste(missing_covs, collapse = ", ")), 
             call. = FALSE)
      }
    }
  }

  mask <- spatstat.geom::as.mask(window, eps = eps, dimyx = dimyx)
  coords <- spatstat.geom::raster.xy(mask, drop = FALSE)

  df <- data.frame(
    x = as.numeric(coords$x),
    y = as.numeric(coords$y)
  )

  if (length(feature_names) > 0) {
    for (f in feature_names) {
      if (f %in% c("x", "y")) next
      v_extracted <- covariates[[f]][list(x = df$x, y = df$y)]
      if (inherits(object, "gbpp_catboost") && (is.factor(v_extracted) || is.character(v_extracted))) {
        df[[f]] <- as.character(v_extracted)
      } else if (is.factor(v_extracted) || is.character(v_extracted)) {
        df[[f]] <- as.numeric(as.factor(v_extracted))
      } else {
        df[[f]] <- as.numeric(v_extracted)
      }
    }
  }

  inside <- as.vector(mask$m)
  valid_covariates <- complete.cases(df)
  valid <- inside & valid_covariates

  out_mat <- matrix(NA_real_, nrow = mask$dim[1], ncol = mask$dim[2])

  if (any(valid)) {
    df_valid <- df[valid, feature_names, drop = FALSE]
    preds <- predict.gbpp_model(object, newdata = df_valid, type = "intensity", ...)
    out_mat[valid] <- preds
  }

  spatstat.geom::im(out_mat, xcol = mask$xcol, yrow = mask$yrow)
}
