# ============================================================
# GBPP - LightGBM Backend Engine
# ============================================================

#' Import the LightGBM Python module for GBPP
#'
#' @keywords internal
#' @noRd
.gbpp_lightgbm_module <- function() {
  configure_gbpp_python()
  python_path <- gbpp_python_path()

  reticulate::import_from_path(
    module = "gbpp_lightgbm",
    path = python_path
  )
}

# ============================================================
# Objective factories
# ============================================================

#' @keywords internal
make_lightgbm_poisson_objective <- function(quadrature_weight) {
  module <- .gbpp_lightgbm_module()
  module$make_poisson_objective(quadrature_weight = quadrature_weight)
}

#' @keywords internal
make_lightgbm_weighted_poisson_objective <- function(quadrature_weight, statistical_weight) {
  module <- .gbpp_lightgbm_module()
  module$make_weighted_poisson_objective(
    quadrature_weight = quadrature_weight,
    statistical_weight = statistical_weight
  )
}

#' @keywords internal
make_lightgbm_logistic_objective <- function(dummy_intensity) {
  module <- .gbpp_lightgbm_module()
  module$make_logistic_objective(dummy_intensity = dummy_intensity)
}

#' @keywords internal
make_lightgbm_weighted_logistic_objective <- function(dummy_intensity, statistical_weight) {
  module <- .gbpp_lightgbm_module()
  module$make_weighted_logistic_objective(
    dummy_intensity = dummy_intensity,
    statistical_weight = statistical_weight
  )
}

#' @keywords internal
make_lightgbm_iterative_poisson_objective <- function(quadrature_weight, interaction_integral) {
  module <- .gbpp_lightgbm_module()
  module$make_iterative_poisson_objective(
    quadrature_weight = quadrature_weight,
    interaction_integral = interaction_integral
  )
}

#' @keywords internal
make_lightgbm_iterative_logistic_objective <- function(dummy_intensity, interaction_integral) {
  module <- .gbpp_lightgbm_module()
  module$make_iterative_logistic_objective(
    dummy_intensity = dummy_intensity,
    interaction_integral = interaction_integral
  )
}

# ============================================================
# Internal Engine Fitting Function
# ============================================================

#' Internal LightGBM engine fitting function
#'
#' @keywords internal
#' @noRd
.gbpp_fit_lightgbm <- function(
    data,
    objective,
    params,
    n_trees,
    baseline = NULL,
    ...
) {
  validate_gbpp_data(data, objective = objective)
  configure_gbpp_python()

  lgb <- reticulate::import("lightgbm", delay_load = FALSE)

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

  feature_names <- colnames(data$X)
  init_score <- if (!is.null(baseline) && baseline != 0.0) {
    rep(as.numeric(baseline), nrow(data$X))
  } else {
    NULL
  }

  if (nrow(data$X) < 20L) {
    if (is.null(params$min_data_in_bin) || params$min_data_in_bin > 1L) {
      params$min_data_in_bin <- 1L
    }
    if (is.null(params$min_data_in_leaf) || params$min_data_in_leaf > 1L) {
      params$min_data_in_leaf <- 1L
    }
    if (is.null(params$min_sum_hessian_in_leaf) || params$min_sum_hessian_in_leaf > 1e-3) {
      params$min_sum_hessian_in_leaf <- 1e-3
    }
  }

  ds_params <- list()
  if (!is.null(params$min_data_in_bin)) {
    ds_params$min_data_in_bin <- as.integer(params$min_data_in_bin)
  }
  if (!is.null(params$min_data_in_leaf)) {
    ds_params$min_data_in_leaf <- as.integer(params$min_data_in_leaf)
  }

  X_data <- data$X
  if (is.data.frame(X_data)) {
    for (col in names(X_data)) {
      v <- X_data[[col]]
      if (is.data.frame(v)) v <- v[[1]]
      if (is.factor(v) || is.character(v)) {
        X_data[[col]] <- as.numeric(as.factor(v))
      }
    }
  }

  dtrain <- lgb$Dataset(
    data = as.matrix(X_data),
    label = data$y,
    feature_name = if (!is.null(feature_names)) reticulate::r_to_py(as.list(feature_names)) else "auto",
    init_score = init_score,
    params = if (length(ds_params) > 0) ds_params else NULL
  )

  py_objective <- switch(
    objective,
    poisson = make_lightgbm_poisson_objective(data$quadrature_weight),
    weighted_poisson = make_lightgbm_weighted_poisson_objective(data$quadrature_weight, data$statistical_weight),
    logistic = make_lightgbm_logistic_objective(data$dummy_intensity),
    weighted_logistic = make_lightgbm_weighted_logistic_objective(data$dummy_intensity, data$statistical_weight),
    iterative_poisson = make_lightgbm_iterative_poisson_objective(data$quadrature_weight, data$interaction_integral),
    iterative_logistic = make_lightgbm_iterative_logistic_objective(data$dummy_intensity, data$interaction_integral)
  )

  params$objective <- py_objective

  extra_args <- list(...)
  extra_args$num_boost_round <- NULL
  extra_args$nrounds <- NULL

  train_args <- c(
    list(
      params = params,
      train_set = dtrain,
      num_boost_round = as.integer(n_trees)
    ),
    extra_args
  )

  raw_model <- do.call(lgb$train, train_args)

  attr(raw_model, "engine") <- "lightgbm"
  attr(raw_model, "objective") <- objective
  attr(raw_model, "baseline") <- baseline
  attr(raw_model, "feature_names") <- feature_names
  attr(raw_model, "params") <- params
  attr(raw_model, "n_trees") <- n_trees
  attr(raw_model, "data_info") <- list(
    n_obs = nrow(data$X),
    n_events = sum(data$y == 1),
    type = data$type
  )
  class(raw_model) <- c("gbpp_lightgbm", "gbpp_model", class(raw_model))
  raw_model
}

# ============================================================
# SHAP Extraction
# ============================================================

#' @export
.gbpp_shap.gbpp_lightgbm <- function(object, newdata, ...) {
  configure_gbpp_python()
  
  if (!is.null(attr(object, "feature_names"))) {
    newdata <- newdata[, attr(object, "feature_names"), drop = FALSE]
  }

  X_data <- newdata
  if (is.data.frame(X_data)) {
    for (col in names(X_data)) {
      v <- X_data[[col]]
      if (is.data.frame(v)) v <- v[[1]]
      if (is.factor(v) || is.character(v)) {
        X_data[[col]] <- as.numeric(as.factor(v))
      }
    }
  }

  shap_vals <- object$predict(as.matrix(X_data), pred_contrib = TRUE)
  
  colnames(shap_vals) <- c(attr(object, "feature_names"), "bias")
  
  list(
    shap = shap_vals[, -ncol(shap_vals), drop = FALSE],
    bias = shap_vals[, ncol(shap_vals)]
  )
}

