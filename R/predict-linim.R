#' Internal function to map GBPP predictions to a spatstat linim object
#'
#' @param object A `gbpp_model` object.
#' @param newdata A list of `spatstat.linnet::linim` objects or a `linnet` object.
#' @param eps Optional spatial resolution (spacing between sample points).
#' @param ... Additional arguments passed to predict.
#'
#' @return A `spatstat.linnet::linim` object.
#' @keywords internal
.gbpp_predict_linim <- function(object, newdata, eps = NULL, ...) {
  if (missing(newdata) || is.null(newdata)) {
    stop("`newdata` must be a list of `spatstat.linnet::linim` objects or a `linnet` object.", call. = FALSE)
  }

  if (inherits(newdata, "linnet")) {
    network <- newdata
    covariates <- list()
  } else if (is.list(newdata) && length(newdata) > 0 && all(vapply(newdata, inherits, logical(1), "linim"))) {
    # Extract the network from the first linim
    network <- spatstat.linnet::as.linnet(newdata[[1]])
    covariates <- newdata
  } else {
    stop("`newdata` for type = 'linim' must be a list of `linim` objects or a `linnet` object.", call. = FALSE)
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

  f_predict <- function(x, y, seg, tp) {
    df <- data.frame(
      x = as.numeric(x),
      y = as.numeric(y)
    )
    
    if (length(feature_names) > 0 && length(covariates) > 0) {
      # Construct lpp efficiently using pre-computed seg and tp
      p_lpp <- spatstat.linnet::lpp(
        data.frame(x = df$x, y = df$y, seg = seg, tp = tp), 
        network
      )
      for (f in feature_names) {
        if (f %in% c("x", "y")) next
        df[[f]] <- as.numeric(covariates[[f]][p_lpp])
      }
    }
    
    valid <- complete.cases(df)
    out <- rep(NA_real_, length(x))
    
    if (any(valid)) {
      df_valid <- df[valid, feature_names, drop = FALSE]
      out[valid] <- predict.gbpp_model(object, newdata = df_valid, type = "intensity")
    }
    out
  }

  lf <- spatstat.linnet::linfun(f_predict, network)
  spatstat.linnet::as.linim(lf, eps = eps)
}
