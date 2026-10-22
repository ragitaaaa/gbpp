# ============================================================
# GBPP - Unified Prediction & Print Methods
# ============================================================

#' Predict from a Fitted GBPP Model
#'
#' Evaluates the fitted gradient boosted point process model on new covariate data,
#' returning the estimated log-intensity (link), spatial intensity, raw boosting tree margin,
#' or continuous spatial pixel images on planar or linear network domains.
#'
#' @param object A fitted model of class `gbpp_model`.
#' @param newdata Predictor covariates. Can be:
#'   - A `data.frame` or numeric `matrix` of covariate values evaluated at specific locations (for `type` in `"link"`, `"intensity"`, `"raw"`).
#'   - A named list of `spatstat.geom::im` objects (or an `owin` window) for 2D continuous planar evaluation (for `type = "im"`).
#'   - A named list of `spatstat.linnet::linim` or `linfun` objects (or a `linnet` network) for linear network evaluation (for `type = "linim"`).
#' @param type Type of prediction:
#'   - `"link"` (default): The complete linear predictor \eqn{\hat{\eta}(u) = \eta_0 + \sum f_m(z(u))}.
#'   - `"intensity"`: The spatial intensity \eqn{\hat{\lambda}(u) = \exp(\hat{\eta}(u))}.
#'   - `"raw"`: The raw ensemble contribution \eqn{\sum f_m(z(u))} without baseline offset.
#'   - `"im"`: A continuous 2D pixel image (`spatstat.geom::im`) of spatial intensity across a planar window.
#'   - `"linim"`: A continuous linear network pixel image (`spatstat.linnet::linim`) of spatial intensity across a network.
#' @param ... Additional arguments passed to the underlying engine prediction method
#'   (e.g., `eps` or `dimyx` when `type = "im"` or `type = "linim"`).
#'
#' @return A numeric vector of predictions (for `type` in `"link"`, `"intensity"`, `"raw"`),
#'   an object of class `spatstat.geom::im` (for `type = "im"`), or an object of class
#'   `spatstat.linnet::linim` (for `type = "linim"`).
#' @export
predict.gbpp_model <- function(
    object,
    newdata,
    type = c("link", "intensity", "raw", "im", "linim"),
    ...
) {
  type <- match.arg(type)

  if (type == "im") {
    return(.gbpp_predict_im(object, newdata, ...))
  }
  if (type == "linim") {
    return(.gbpp_predict_linim(object, newdata, ...))
  }

  if (missing(newdata) || is.null(newdata)) {
    stop("`newdata` must be supplied.", call. = FALSE)
  }

  if (!is.data.frame(newdata) && !is.matrix(newdata)) {
    stop("`newdata` must be a numeric matrix or data frame.", call. = FALSE)
  }

  if (inherits(object, "python.builtin.object")) {
    raw_model <- object
    baseline <- attr(object, "baseline")
    baseline <- if (!is.null(baseline)) as.numeric(baseline) else 0.0
    engine <- attr(object, "engine")
    if (is.null(engine)) {
      engine <- if (inherits(object, "gbpp_xgboost")) {
        "xgboost"
      } else if (inherits(object, "gbpp_lightgbm")) {
        "lightgbm"
      } else if (inherits(object, "gbpp_catboost")) {
        "catboost"
      } else {
        stop("Unknown GBPP model engine.", call. = FALSE)
      }
    }
  } else if (is.list(object) && !is.null(object$raw_model)) {
    raw_model <- object$raw_model
    baseline <- if (!is.null(object$baseline)) as.numeric(object$baseline) else 0.0
    engine <- object$engine
  } else {
    stop("Invalid `gbpp_model` object.", call. = FALSE)
  }

  # Engine-specific raw prediction
  raw_pred <- switch(
    engine,

    xgboost = {
      if (!reticulate::py_available(initialize = TRUE)) stop("Python not available")
      if (is.data.frame(newdata)) {
        for (col in names(newdata)) {
          v <- newdata[[col]]
          if (is.data.frame(v)) v <- v[[1]]
          if (is.factor(v) || is.character(v)) {
            newdata[[col]] <- as.numeric(as.factor(v))
          }
        }
        feature_names <- colnames(newdata)
        mat <- as.matrix(newdata)
      } else {
        feature_names <- colnames(newdata)
        mat <- newdata
      }

      if (!is.numeric(mat) || any(!is.finite(mat))) {
        stop("`newdata` must contain finite numeric values.", call. = FALSE)
      }

      xgb <- reticulate::import("xgboost", delay_load = FALSE)
      dtest <- if (!is.null(feature_names)) {
        xgb$DMatrix(data = mat, feature_names = reticulate::r_to_py(as.list(feature_names)))
      } else {
        xgb$DMatrix(data = mat)
      }

      pred <- raw_model$predict(dtest, ...)
      as.numeric(reticulate::py_to_r(pred))
    },

    lightgbm = {
      if (!reticulate::py_available(initialize = TRUE)) stop("Python not available")
      if (is.data.frame(newdata)) {
        for (col in names(newdata)) {
          v <- newdata[[col]]
          if (is.data.frame(v)) v <- v[[1]]
          if (is.factor(v) || is.character(v)) {
            newdata[[col]] <- as.numeric(as.factor(v))
          }
        }
        mat <- as.matrix(newdata)
      } else {
        mat <- newdata
      }

      if (!is.numeric(mat) || any(!is.finite(mat))) {
        stop("`newdata` must contain finite numeric values.", call. = FALSE)
      }

      pred <- raw_model$predict(data = mat, raw_score = TRUE, ...)
      as.numeric(reticulate::py_to_r(pred))
    },

    catboost = {
      if (!reticulate::py_available(initialize = TRUE)) stop("Python not available")
      if (is.matrix(newdata)) {
        if (!is.numeric(newdata) || any(!is.finite(newdata))) {
          stop("`newdata` matrix must contain finite numeric values.", call. = FALSE)
        }
      } else if (is.data.frame(newdata)) {
        categorical_cols <- which(
          vapply(newdata, function(z) is.factor(z) || is.character(z), logical(1))
        )
        numeric_cols <- which(vapply(newdata, is.numeric, logical(1)))

        if (length(numeric_cols) > 0L) {
          num_vals <- newdata[, numeric_cols, drop = FALSE]
          if (any(!is.finite(as.matrix(num_vals)))) {
            stop("Numeric columns in `newdata` must contain only finite values.", call. = FALSE)
          }
        }

        if (length(categorical_cols) > 0L) {
          for (j in categorical_cols) {
            if (anyNA(newdata[[j]])) {
              stop("Categorical columns in `newdata` must not contain missing values.", call. = FALSE)
            }
            if (is.factor(newdata[[j]])) {
              newdata[[j]] <- as.character(newdata[[j]])
            }
          }
        }
      }

      pred <- raw_model$predict(data = newdata, prediction_type = "RawFormulaVal", ...)
      as.numeric(reticulate::py_to_r(pred))
    }
  )

  # Apply baseline and link transformation
  switch(
    type,
    raw = raw_pred,
    link = baseline + raw_pred,
    intensity = exp(pmin(pmax(baseline + raw_pred, -50), 50))
  )
}

#' @export
predict.gbpp_xgboost <- function(object, newdata, ...) {
  predict.gbpp_model(object = object, newdata = newdata, ...)
}

#' @export
predict.gbpp_lightgbm <- function(object, newdata, ...) {
  predict.gbpp_model(object = object, newdata = newdata, ...)
}

#' @export
predict.gbpp_catboost <- function(object, newdata, ...) {
  predict.gbpp_model(object = object, newdata = newdata, ...)
}

#' Print a GBPP fitted model
#'
#' @param x A `gbpp_model` object.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#' @export
print.gbpp_model <- function(x, ...) {
  cat("Gradient Boosted Point Process Model\n")
  cat("------------------------------------\n")
  cat("Engine          :", attr(x, "engine"), "\n")
  cat("Objective       :", attr(x, "objective"), "\n")
  cat("Number of trees :", attr(x, "n_trees"), "\n")
  cat("Baseline (eta0) :", round(attr(x, "baseline"), 4), sprintf("(lambda0 = %.4f)\n", exp(attr(x, "baseline"))))
  cat("Features (p)    :", length(attr(x, "feature_names")), "\n")
  if (length(attr(x, "feature_names")) > 0L) {
    cat("Feature names   :", paste(attr(x, "feature_names"), collapse = ", "), "\n")
  }
  invisible(x)
}
