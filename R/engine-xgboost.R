# ============================================================
# GBPP - XGBoost Backend Engine
# ============================================================

#' Import the XGBoost Python module for GBPP
#'
#' @keywords internal
#' @noRd
.gbpp_xgboost_module <- function() {
  configure_gbpp_python()
  python_path <- gbpp_python_path()

  reticulate::import_from_path(
    module = "gbpp_xgboost",
    path = python_path
  )
}

#' Construct an XGBoost DMatrix for GBPP
#'
#' @keywords internal
#' @noRd
.gbpp_xgboost_dmatrix <- function(X, y = NULL, base_margin = NULL) {
  xgb <- reticulate::import("xgboost", delay_load = FALSE)

  if (is.data.frame(X)) {
    for (col in names(X)) {
      v <- X[[col]]
      if (is.data.frame(v)) v <- v[[1]]
      if (is.factor(v) || is.character(v)) {
        X[[col]] <- as.numeric(as.factor(v))
      }
    }
  }

  X_matrix <- as.matrix(X)
  feature_names <- colnames(X_matrix)

  labels <- if (!is.null(y)) as.numeric(y) else NULL

  if (!is.null(base_margin)) {
    base_margin <- as.numeric(base_margin)
    if (length(base_margin) != nrow(X_matrix)) {
      stop("`base_margin` must have the same length as observations.", call. = FALSE)
    }
  }

  if (is.null(feature_names)) {
    xgb$DMatrix(
      data = X_matrix,
      label = labels,
      base_margin = base_margin
    )
  } else {
    xgb$DMatrix(
      data = X_matrix,
      label = labels,
      base_margin = base_margin,
      feature_names = as.list(feature_names)
    )
  }
}

# ============================================================
# Objective factories
# ============================================================

#' @keywords internal
make_xgboost_poisson_objective <- function(quadrature_weight) {
  module <- .gbpp_xgboost_module()
  module$make_poisson_objective(quadrature_weight = quadrature_weight)
}

#' @keywords internal
make_xgboost_weighted_poisson_objective <- function(quadrature_weight, statistical_weight) {
  module <- .gbpp_xgboost_module()
  module$make_weighted_poisson_objective(
    quadrature_weight = quadrature_weight,
    statistical_weight = statistical_weight
  )
}

#' @keywords internal
make_xgboost_logistic_objective <- function(dummy_intensity) {
  module <- .gbpp_xgboost_module()
  module$make_logistic_objective(dummy_intensity = dummy_intensity)
}

#' @keywords internal
make_xgboost_weighted_logistic_objective <- function(dummy_intensity, statistical_weight) {
  module <- .gbpp_xgboost_module()
  module$make_weighted_logistic_objective(
    dummy_intensity = dummy_intensity,
    statistical_weight = statistical_weight
  )
}

#' @keywords internal
make_xgboost_iterative_poisson_objective <- function(quadrature_weight, interaction_integral) {
  module <- .gbpp_xgboost_module()
  module$make_iterative_poisson_objective(
    quadrature_weight = quadrature_weight,
    interaction_integral = interaction_integral
  )
}

#' @keywords internal
make_xgboost_iterative_logistic_objective <- function(dummy_intensity, interaction_integral) {
  module <- .gbpp_xgboost_module()
  module$make_iterative_logistic_objective(
    dummy_intensity = dummy_intensity,
    interaction_integral = interaction_integral
  )
}

# ============================================================
# Internal Engine Fitting Function
# ============================================================

#' Internal XGBoost engine fitting function
#'
#' @keywords internal
#' @noRd
.gbpp_fit_xgboost <- function(
    data,
    objective,
    params,
    n_trees,
    baseline = NULL,
    ...
) {
  validate_gbpp_data(data, objective = objective)
  configure_gbpp_python()

  xgb <- reticulate::import("xgboost", delay_load = FALSE)

  # Calculate initial baseline offset if not provided
  if (is.null(baseline)) {
    if (objective %in% c("poisson", "weighted_poisson", "iterative_poisson")) {
      lambda0 <- sum(data$y) / sum(data$quadrature_weight)
      if (lambda0 <= 0 || !is.finite(lambda0)) {
        stop("Initial Poisson intensity must be positive and finite.", call. = FALSE)
      }
      baseline <- log(lambda0)
    } else {
      baseline <- 0.0
    }
  }

  base_margin_vec <- if (!is.null(baseline) && baseline != 0.0) {
    rep(as.numeric(baseline), nrow(data$X))
  } else {
    NULL
  }

  dtrain <- .gbpp_xgboost_dmatrix(
    X = data$X,
    y = data$y,
    base_margin = base_margin_vec
  )

  py_objective <- switch(
    objective,
    poisson = make_xgboost_poisson_objective(data$quadrature_weight),
    weighted_poisson = make_xgboost_weighted_poisson_objective(data$quadrature_weight, data$statistical_weight),
    logistic = make_xgboost_logistic_objective(data$dummy_intensity),
    weighted_logistic = make_xgboost_weighted_logistic_objective(data$dummy_intensity, data$statistical_weight),
    iterative_poisson = make_xgboost_iterative_poisson_objective(data$quadrature_weight, data$interaction_integral),
    iterative_logistic = make_xgboost_iterative_logistic_objective(data$dummy_intensity, data$interaction_integral)
  )

  extra_args <- list(...)
  extra_args$num_boost_round <- NULL
  extra_args$nrounds <- NULL

  train_args <- c(
    list(
      params = params,
      dtrain = dtrain,
      num_boost_round = as.integer(n_trees),
      obj = py_objective
    ),
    extra_args
  )

  raw_model <- do.call(xgb$train, train_args)

  attr(raw_model, "engine") <- "xgboost"
  attr(raw_model, "objective") <- objective
  attr(raw_model, "baseline") <- baseline
  attr(raw_model, "feature_names") <- colnames(data$X)
  attr(raw_model, "params") <- params
  attr(raw_model, "n_trees") <- n_trees
  attr(raw_model, "data_info") <- list(
    n_obs = nrow(data$X),
    n_events = sum(data$y == 1),
    type = data$type
  )
  class(raw_model) <- c("gbpp_xgboost", "gbpp_model", class(raw_model))
  raw_model
}

# ============================================================
# SHAP Extraction
# ============================================================

#' @export
.gbpp_shap.gbpp_xgboost <- function(object, newdata, ...) {
  configure_gbpp_python()
  
  # Ensure newdata only contains the features used during training
  if (!is.null(attr(object, "feature_names"))) {
    newdata <- newdata[, attr(object, "feature_names"), drop = FALSE]
  }

  dmat <- .gbpp_xgboost_dmatrix(X = newdata)
  shap_vals <- object$predict(dmat, pred_contribs = TRUE)
  
  colnames(shap_vals) <- c(attr(object, "feature_names"), "bias")
  
  list(
    shap = shap_vals[, -ncol(shap_vals), drop = FALSE],
    bias = shap_vals[, ncol(shap_vals)]
  )
}

