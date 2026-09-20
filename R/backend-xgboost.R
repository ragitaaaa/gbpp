# XGBoost backend for gbpp
#
# This file contains:
# - XGBoost objective factories
# - XGBoost fitting functions
# - XGBoost prediction method

# ============================================================
# Internal helper: import GBPP XGBoost Python module
# ============================================================

.gbpp_xgboost_module <- function() {
  configure_gbpp_python()

  python_path <- gbpp_python_path()

  reticulate::import_from_path(
    module = "gbpp_xgboost",
    path = python_path
  )
}

.gbpp_xgboost_dmatrix <- function(X, y) {
  xgb <- reticulate::import("xgboost", delay_load = FALSE)

  X_matrix <- as.matrix(X)
  labels <- as.numeric(y)
  feature_names <- colnames(X_matrix)

  if (is.null(feature_names)) {
    return(
      xgb$DMatrix(
        data = X_matrix,
        label = labels
      )
    )
  }

  xgb$DMatrix(
    data = X_matrix,
    label = labels,
    feature_names = as.list(feature_names)
  )
}

# ============================================================
# Internal helper: mark fitted model
# ============================================================

mark_gbpp_xgboost <- function(model) {
  class(model) <- c("gbpp_xgboost", class(model))
  model
}


# ============================================================
# Prediction method
# ============================================================

#' Predict from a fitted GBPP XGBoost model
#'
#' @param object A fitted `gbpp_xgboost` model.
#' @param newdata A numeric matrix or data frame containing predictor
#'   variables.
#' @param ... Additional arguments passed to the XGBoost prediction
#'   method.
#'
#' @return A numeric vector of predictions.
#'
#' @export
predict.gbpp_xgboost <- function(object, newdata, ...) {

  if (missing(newdata)) {
    stop("`newdata` must be supplied.", call. = FALSE)
  }

  if (!is.matrix(newdata) && !is.data.frame(newdata)) {
    stop(
      "`newdata` must be a numeric matrix or data frame.",
      call. = FALSE
    )
  }

  if (is.data.frame(newdata)) {
    if (!all(vapply(newdata, is.numeric, logical(1)))) {
      stop(
        "`newdata` must contain only numeric columns.",
        call. = FALSE
      )
    }

    newdata <- as.matrix(newdata)
  }

  if (!is.numeric(newdata)) {
    stop(
      "`newdata` must be numeric.",
      call. = FALSE
    )
  }

  if (any(!is.finite(newdata))) {
    stop(
      "`newdata` must contain only finite values.",
      call. = FALSE
    )
  }

  xgb <- reticulate::import("xgboost")

  dtest <- xgb$DMatrix(
    data = newdata
  )

  if (!is.null(colnames(newdata))) {
    dtest$feature_names <- colnames(newdata)
  }

  predictions <- object$predict(
    dtest,
    ...
  )

  reticulate::py_to_r(predictions)
}


# ============================================================
# Objective factories
# ============================================================

#' Create XGBoost Poisson objective
#'
#' @param quadrature_weight Numeric vector of quadrature weights.
#'
#' @return A Python callable implementing the GBPP Poisson objective.
#'
#' @keywords internal
make_xgboost_poisson_objective <- function(quadrature_weight) {

  if (!is.numeric(quadrature_weight)) {
    stop("`quadrature_weight` must be numeric.", call. = FALSE)
  }

  if (any(!is.finite(quadrature_weight))) {
    stop(
      "`quadrature_weight` must contain only finite values.",
      call. = FALSE
    )
  }

  if (any(quadrature_weight <= 0)) {
    stop(
      "`quadrature_weight` must contain only positive values.",
      call. = FALSE
    )
  }

  module <- .gbpp_xgboost_module()

  module$make_poisson_objective(
    quadrature_weight = quadrature_weight
  )
}


#' Create XGBoost weighted Poisson objective
#'
#' @param quadrature_weight Numeric vector of quadrature weights.
#' @param statistical_weight Numeric vector of statistical weights.
#'
#' @return A Python callable implementing the weighted GBPP Poisson objective.
#'
#' @keywords internal
make_xgboost_weighted_poisson_objective <- function(
    quadrature_weight,
    statistical_weight) {

  if (!is.numeric(quadrature_weight)) {
    stop("`quadrature_weight` must be numeric.", call. = FALSE)
  }

  if (!is.numeric(statistical_weight)) {
    stop("`statistical_weight` must be numeric.", call. = FALSE)
  }

  if (length(quadrature_weight) != length(statistical_weight)) {
    stop(
      "`quadrature_weight` and `statistical_weight` must have the same length.",
      call. = FALSE
    )
  }

  if (any(!is.finite(quadrature_weight))) {
    stop(
      "`quadrature_weight` must contain only finite values.",
      call. = FALSE
    )
  }

  if (any(!is.finite(statistical_weight))) {
    stop(
      "`statistical_weight` must contain only finite values.",
      call. = FALSE
    )
  }

  if (any(quadrature_weight <= 0)) {
    stop(
      "`quadrature_weight` must contain only positive values.",
      call. = FALSE
    )
  }

  if (any(statistical_weight <= 0)) {
    stop(
      "`statistical_weight` must contain only positive values.",
      call. = FALSE
    )
  }

  module <- .gbpp_xgboost_module()

  module$make_weighted_poisson_objective(
    quadrature_weight = quadrature_weight,
    statistical_weight = statistical_weight
  )
}


#' Create XGBoost Logistic objective
#'
#' @param dummy_intensity Numeric vector of dummy intensities.
#'
#' @return A Python callable implementing the GBPP Logistic objective.
#'
#' @keywords internal
make_xgboost_logistic_objective <- function(dummy_intensity) {

  if (!is.numeric(dummy_intensity)) {
    stop("`dummy_intensity` must be numeric.", call. = FALSE)
  }

  if (any(!is.finite(dummy_intensity))) {
    stop(
      "`dummy_intensity` must contain only finite values.",
      call. = FALSE
    )
  }

  if (any(dummy_intensity <= 0)) {
    stop(
      "`dummy_intensity` must contain only positive values.",
      call. = FALSE
    )
  }

  module <- .gbpp_xgboost_module()

  module$make_logistic_objective(
    dummy_intensity = dummy_intensity
  )
}


#' Create XGBoost weighted Logistic objective
#'
#' @param dummy_intensity Numeric vector of dummy intensities.
#' @param statistical_weight Numeric vector of statistical weights.
#'
#' @return A Python callable implementing the weighted GBPP Logistic objective.
#'
#' @keywords internal
make_xgboost_weighted_logistic_objective <- function(
    dummy_intensity,
    statistical_weight) {

  if (!is.numeric(dummy_intensity)) {
    stop("`dummy_intensity` must be numeric.", call. = FALSE)
  }

  if (!is.numeric(statistical_weight)) {
    stop("`statistical_weight` must be numeric.", call. = FALSE)
  }

  if (length(dummy_intensity) != length(statistical_weight)) {
    stop(
      "`dummy_intensity` and `statistical_weight` must have the same length.",
      call. = FALSE
    )
  }

  if (any(!is.finite(dummy_intensity))) {
    stop(
      "`dummy_intensity` must contain only finite values.",
      call. = FALSE
    )
  }

  if (any(!is.finite(statistical_weight))) {
    stop(
      "`statistical_weight` must contain only finite values.",
      call. = FALSE
    )
  }

  if (any(dummy_intensity <= 0)) {
    stop(
      "`dummy_intensity` must contain only positive values.",
      call. = FALSE
    )
  }

  if (any(statistical_weight <= 0)) {
    stop(
      "`statistical_weight` must contain only positive values.",
      call. = FALSE
    )
  }

  module <- .gbpp_xgboost_module()

  module$make_weighted_logistic_objective(
    dummy_intensity = dummy_intensity,
    statistical_weight = statistical_weight
  )
}


#' Create XGBoost iterative Poisson objective
#'
#' @param quadrature_weight Numeric vector of quadrature weights.
#' @param interaction_integral Numeric vector of interaction integrals.
#'
#' @return A Python callable implementing the iterative GBPP Poisson objective.
#'
#' @keywords internal
make_xgboost_iterative_poisson_objective <- function(
    quadrature_weight,
    interaction_integral) {

  if (!is.numeric(quadrature_weight)) {
    stop("`quadrature_weight` must be numeric.", call. = FALSE)
  }

  if (!is.numeric(interaction_integral)) {
    stop("`interaction_integral` must be numeric.", call. = FALSE)
  }

  if (length(quadrature_weight) != length(interaction_integral)) {
    stop(
      "`quadrature_weight` and `interaction_integral` must have the same length.",
      call. = FALSE
    )
  }

  if (any(!is.finite(quadrature_weight))) {
    stop(
      "`quadrature_weight` must contain only finite values.",
      call. = FALSE
    )
  }

  if (any(!is.finite(interaction_integral))) {
    stop(
      "`interaction_integral` must contain only finite values.",
      call. = FALSE
    )
  }

  if (any(quadrature_weight <= 0)) {
    stop(
      "`quadrature_weight` must contain only positive values.",
      call. = FALSE
    )
  }

  module <- .gbpp_xgboost_module()

  module$make_iterative_poisson_objective(
    quadrature_weight = quadrature_weight,
    interaction_integral = interaction_integral
  )
}


#' Create XGBoost iterative Logistic objective
#'
#' @param dummy_intensity Numeric vector of dummy intensities.
#' @param interaction_integral Numeric vector of interaction integrals.
#'
#' @return A Python callable implementing the iterative GBPP Logistic objective.
#'
#' @keywords internal
make_xgboost_iterative_logistic_objective <- function(
    dummy_intensity,
    interaction_integral) {

  if (!is.numeric(dummy_intensity)) {
    stop("`dummy_intensity` must be numeric.", call. = FALSE)
  }

  if (!is.numeric(interaction_integral)) {
    stop("`interaction_integral` must be numeric.", call. = FALSE)
  }

  if (length(dummy_intensity) != length(interaction_integral)) {
    stop(
      "`dummy_intensity` and `interaction_integral` must have the same length.",
      call. = FALSE
    )
  }

  if (any(!is.finite(dummy_intensity))) {
    stop(
      "`dummy_intensity` must contain only finite values.",
      call. = FALSE
    )
  }

  if (any(!is.finite(interaction_integral))) {
    stop(
      "`interaction_integral` must contain only finite values.",
      call. = FALSE
    )
  }

  if (any(dummy_intensity <= 0)) {
    stop(
      "`dummy_intensity` must contain only positive values.",
      call. = FALSE
    )
  }

  module <- .gbpp_xgboost_module()

  module$make_iterative_logistic_objective(
    dummy_intensity = dummy_intensity,
    interaction_integral = interaction_integral
  )
}


# ============================================================
# Fit: Poisson
# ============================================================

#' Fit a GBPP XGBoost Poisson model
#'
#' @param data A `gbpp_data` object.
#' @param params A list of XGBoost parameters.
#' @param nrounds Number of boosting rounds.
#'
#' @return A fitted XGBoost Booster.
#'
#' @export
fit_gbpp_xgboost <- function(
    data,
    params = list(),
    nrounds = 100) {

  validate_gbpp_data(data)

  if (!is.numeric(nrounds) || length(nrounds) != 1L ||
      !is.finite(nrounds) || nrounds < 1) {
    stop("`nrounds` must be a positive scalar.", call. = FALSE)
  }

  configure_gbpp_python()

  xgb <- reticulate::import("xgboost")

  dtrain <- .gbpp_xgboost_dmatrix(
    X = data$X,
    y = data$y
  )

  objective <- make_xgboost_poisson_objective(
    data$quadrature_weight
  )

  default_params <- list(
    booster = "gbtree",
    eta = 0.05,
    max_depth = 6L,
    verbosity = 0L
  )

  train_params <- utils::modifyList(
    default_params,
    params
  )

  model <- xgb$train(
    params = train_params,
    dtrain = dtrain,
    num_boost_round = as.integer(nrounds),
    obj = objective
  )

  mark_gbpp_xgboost(model)
}


# ============================================================
# Fit: Weighted Poisson
# ============================================================

#' Fit a GBPP XGBoost weighted Poisson model
#'
#' @param data A `gbpp_data` object.
#' @param params A list of XGBoost parameters.
#' @param nrounds Number of boosting rounds.
#'
#' @return A fitted XGBoost Booster.
#'
#' @export
fit_gbpp_xgboost_weighted_poisson <- function(
    data,
    params = list(),
    nrounds = 100) {

  validate_gbpp_data(data)

  if (is.null(data$statistical_weight)) {
    stop(
      "`statistical_weight` is required for weighted Poisson.",
      call. = FALSE
    )
  }

  if (!is.numeric(nrounds) || length(nrounds) != 1L ||
      !is.finite(nrounds) || nrounds < 1) {
    stop("`nrounds` must be a positive scalar.", call. = FALSE)
  }

  configure_gbpp_python()

  xgb <- reticulate::import("xgboost")

  dtrain <- .gbpp_xgboost_dmatrix(
    X = data$X,
    y = data$y
  )

  objective <- make_xgboost_weighted_poisson_objective(
    data$quadrature_weight,
    data$statistical_weight
  )

  default_params <- list(
    booster = "gbtree",
    eta = 0.05,
    max_depth = 6L,
    verbosity = 0L
  )

  train_params <- utils::modifyList(
    default_params,
    params
  )

  model <- xgb$train(
    params = train_params,
    dtrain = dtrain,
    num_boost_round = as.integer(nrounds),
    obj = objective
  )

  mark_gbpp_xgboost(model)
}


# ============================================================
# Fit: Logistic
# ============================================================

#' Fit a GBPP XGBoost Logistic model
#'
#' @param data A `gbpp_data` object.
#' @param params A list of XGBoost parameters.
#' @param nrounds Number of boosting rounds.
#'
#' @return A fitted XGBoost Booster.
#'
#' @export
fit_gbpp_xgboost_logistic <- function(
    data,
    params = list(),
    nrounds = 100) {

  validate_gbpp_data(data)

  if (is.null(data$dummy_intensity)) {
    stop(
      "`dummy_intensity` is required for Logistic.",
      call. = FALSE
    )
  }

  if (!is.numeric(nrounds) || length(nrounds) != 1L ||
      !is.finite(nrounds) || nrounds < 1) {
    stop("`nrounds` must be a positive scalar.", call. = FALSE)
  }

  configure_gbpp_python()

  xgb <- reticulate::import("xgboost")

  dtrain <- .gbpp_xgboost_dmatrix(
    X = data$X,
    y = data$y
  )

  objective <- make_xgboost_logistic_objective(
    data$dummy_intensity
  )

  default_params <- list(
    booster = "gbtree",
    eta = 0.05,
    max_depth = 6L,
    verbosity = 0L
  )

  train_params <- utils::modifyList(
    default_params,
    params
  )

  model <- xgb$train(
    params = train_params,
    dtrain = dtrain,
    num_boost_round = as.integer(nrounds),
    obj = objective
  )

  mark_gbpp_xgboost(model)
}


# ============================================================
# Fit: Weighted Logistic
# ============================================================

#' Fit a GBPP XGBoost weighted Logistic model
#'
#' @param data A `gbpp_data` object.
#' @param params A list of XGBoost parameters.
#' @param num_boost_round Number of boosting rounds.
#'
#' @return A fitted XGBoost Booster.
#'
#' @export
fit_gbpp_xgboost_weighted_logistic <- function(
    data,
    params = list(),
    num_boost_round = 100L) {

  validate_gbpp_data(data)

  if (is.null(data$dummy_intensity)) {
    stop(
      "`dummy_intensity` is required for weighted Logistic.",
      call. = FALSE
    )
  }

  if (is.null(data$statistical_weight)) {
    stop(
      "`statistical_weight` is required for weighted Logistic.",
      call. = FALSE
    )
  }

  if (!is.numeric(num_boost_round) ||
      length(num_boost_round) != 1L ||
      !is.finite(num_boost_round) ||
      num_boost_round < 1) {
    stop(
      "`num_boost_round` must be a positive scalar.",
      call. = FALSE
    )
  }

  configure_gbpp_python()

  xgb <- reticulate::import("xgboost")

  dtrain <- .gbpp_xgboost_dmatrix(
    X = data$X,
    y = data$y
  )

  objective <- make_xgboost_weighted_logistic_objective(
    data$dummy_intensity,
    data$statistical_weight
  )

  default_params <- list(
    booster = "gbtree",
    eta = 0.05,
    max_depth = 6L,
    verbosity = 0L
  )

  train_params <- utils::modifyList(
    default_params,
    params
  )

  model <- xgb$train(
    params = train_params,
    dtrain = dtrain,
    num_boost_round = as.integer(num_boost_round),
    obj = objective
  )

  mark_gbpp_xgboost(model)
}


# ============================================================
# Fit: Iterative Poisson
# ============================================================

#' Fit a GBPP XGBoost iterative Poisson model
#'
#' @param data A `gbpp_data` object.
#' @param params A list of XGBoost parameters.
#' @param num_boost_round Number of boosting rounds.
#'
#' @return A fitted XGBoost Booster.
#'
#' @export
fit_gbpp_xgboost_iterative_poisson <- function(
    data,
    params = list(),
    num_boost_round = 100L) {

  validate_gbpp_data(data)

  if (is.null(data$interaction_integral)) {
    stop(
      "`interaction_integral` is required for iterative Poisson.",
      call. = FALSE
    )
  }

  if (!is.numeric(num_boost_round) ||
      length(num_boost_round) != 1L ||
      !is.finite(num_boost_round) ||
      num_boost_round < 1) {
    stop(
      "`num_boost_round` must be a positive scalar.",
      call. = FALSE
    )
  }

  configure_gbpp_python()

  xgb <- reticulate::import("xgboost")

  dtrain <- .gbpp_xgboost_dmatrix(
    X = data$X,
    y = data$y
  )

  objective <- make_xgboost_iterative_poisson_objective(
    data$quadrature_weight,
    data$interaction_integral
  )

  default_params <- list(
    booster = "gbtree",
    eta = 0.05,
    max_depth = 6L,
    verbosity = 0L
  )

  train_params <- utils::modifyList(
    default_params,
    params
  )

  model <- xgb$train(
    params = train_params,
    dtrain = dtrain,
    num_boost_round = as.integer(num_boost_round),
    obj = objective
  )

  mark_gbpp_xgboost(model)
}


# ============================================================
# Fit: Iterative Logistic
# ============================================================

#' Fit a GBPP XGBoost iterative Logistic model
#'
#' @param data A `gbpp_data` object.
#' @param params A list of XGBoost parameters.
#' @param num_boost_round Number of boosting rounds.
#'
#' @return A fitted XGBoost Booster.
#'
#' @export
fit_gbpp_xgboost_iterative_logistic <- function(
    data,
    params = list(),
    num_boost_round = 100L) {

  validate_gbpp_data(data)

  if (is.null(data$dummy_intensity)) {
    stop(
      "`dummy_intensity` is required for iterative Logistic.",
      call. = FALSE
    )
  }

  if (is.null(data$interaction_integral)) {
    stop(
      "`interaction_integral` is required for iterative Logistic.",
      call. = FALSE
    )
  }

  if (!is.numeric(num_boost_round) ||
      length(num_boost_round) != 1L ||
      !is.finite(num_boost_round) ||
      num_boost_round < 1) {
    stop(
      "`num_boost_round` must be a positive scalar.",
      call. = FALSE
    )
  }

  configure_gbpp_python()

  xgb <- reticulate::import("xgboost")

  dtrain <- .gbpp_xgboost_dmatrix(
    X = data$X,
    y = data$y
  )

  objective <- make_xgboost_iterative_logistic_objective(
    data$dummy_intensity,
    data$interaction_integral
  )

  default_params <- list(
    booster = "gbtree",
    eta = 0.05,
    max_depth = 6L,
    verbosity = 0L
  )

  train_params <- utils::modifyList(
    default_params,
    params
  )

  model <- xgb$train(
    params = train_params,
    dtrain = dtrain,
    num_boost_round = as.integer(num_boost_round),
    obj = objective
  )

  mark_gbpp_xgboost(model)
}
