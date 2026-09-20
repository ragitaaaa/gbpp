# LightGBM backend for gbpp
#
# This file contains:
# - LightGBM objective factories
# - LightGBM fitting functions
#
# Prediction will be added separately after fit() is tested.


# ============================================================
# Internal helper: import GBPP LightGBM Python module
# ============================================================

.gbpp_lightgbm_module <- function() {

  configure_gbpp_python()

  python_path <- gbpp_python_path()

  reticulate::import_from_path(
    module = "gbpp_lightgbm",
    path = python_path
  )
}


# ============================================================
# Internal helper: mark fitted model
# ============================================================

mark_gbpp_lightgbm <- function(model) {

  class(model) <- c(
    "gbpp_lightgbm",
    class(model)
  )

  model
}


# ============================================================
# Objective factories
# ============================================================


#' Create LightGBM Poisson objective
#'
#' @param quadrature_weight Numeric vector of quadrature weights.
#'
#' @return A Python callable implementing the GBPP Poisson objective.
#'
#' @keywords internal
make_lightgbm_poisson_objective <- function(
    quadrature_weight) {

  if (!is.numeric(quadrature_weight)) {
    stop(
      "`quadrature_weight` must be numeric.",
      call. = FALSE
    )
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

  module <- .gbpp_lightgbm_module()

  module$make_poisson_objective(
    quadrature_weight = quadrature_weight
  )
}


#' Create LightGBM weighted Poisson objective
#'
#' @param quadrature_weight Numeric vector of quadrature weights.
#' @param statistical_weight Numeric vector of statistical weights.
#'
#' @return A Python callable implementing the weighted GBPP
#'   Poisson objective.
#'
#' @keywords internal
make_lightgbm_weighted_poisson_objective <- function(
    quadrature_weight,
    statistical_weight) {

  if (!is.numeric(quadrature_weight)) {
    stop(
      "`quadrature_weight` must be numeric.",
      call. = FALSE
    )
  }

  if (!is.numeric(statistical_weight)) {
    stop(
      "`statistical_weight` must be numeric.",
      call. = FALSE
    )
  }

  if (length(quadrature_weight) != length(statistical_weight)) {
    stop(
      paste(
        "`quadrature_weight` and `statistical_weight`",
        "must have the same length."
      ),
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

  module <- .gbpp_lightgbm_module()

  module$make_weighted_poisson_objective(
    quadrature_weight = quadrature_weight,
    statistical_weight = statistical_weight
  )
}


#' Create LightGBM Logistic objective
#'
#' @param dummy_intensity Numeric vector of dummy intensities.
#'
#' @return A Python callable implementing the GBPP Logistic objective.
#'
#' @keywords internal
make_lightgbm_logistic_objective <- function(
    dummy_intensity) {

  if (!is.numeric(dummy_intensity)) {
    stop(
      "`dummy_intensity` must be numeric.",
      call. = FALSE
    )
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

  module <- .gbpp_lightgbm_module()

  module$make_logistic_objective(
    dummy_intensity = dummy_intensity
  )
}


#' Create LightGBM weighted Logistic objective
#'
#' @param dummy_intensity Numeric vector of dummy intensities.
#' @param statistical_weight Numeric vector of statistical weights.
#'
#' @return A Python callable implementing the weighted GBPP
#'   Logistic objective.
#'
#' @keywords internal
make_lightgbm_weighted_logistic_objective <- function(
    dummy_intensity,
    statistical_weight) {

  if (!is.numeric(dummy_intensity)) {
    stop(
      "`dummy_intensity` must be numeric.",
      call. = FALSE
    )
  }

  if (!is.numeric(statistical_weight)) {
    stop(
      "`statistical_weight` must be numeric.",
      call. = FALSE
    )
  }

  if (length(dummy_intensity) != length(statistical_weight)) {
    stop(
      paste(
        "`dummy_intensity` and `statistical_weight`",
        "must have the same length."
      ),
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

  module <- .gbpp_lightgbm_module()

  module$make_weighted_logistic_objective(
    dummy_intensity = dummy_intensity,
    statistical_weight = statistical_weight
  )
}


#' Create LightGBM iterative Poisson objective
#'
#' @param quadrature_weight Numeric vector of quadrature weights.
#' @param interaction_integral Numeric vector of interaction integrals.
#'
#' @return A Python callable implementing the iterative GBPP
#'   Poisson objective.
#'
#' @keywords internal
make_lightgbm_iterative_poisson_objective <- function(
    quadrature_weight,
    interaction_integral) {

  if (!is.numeric(quadrature_weight)) {
    stop(
      "`quadrature_weight` must be numeric.",
      call. = FALSE
    )
  }

  if (!is.numeric(interaction_integral)) {
    stop(
      "`interaction_integral` must be numeric.",
      call. = FALSE
    )
  }

  if (length(quadrature_weight) != length(interaction_integral)) {
    stop(
      paste(
        "`quadrature_weight` and `interaction_integral`",
        "must have the same length."
      ),
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

  module <- .gbpp_lightgbm_module()

  module$make_iterative_poisson_objective(
    quadrature_weight = quadrature_weight,
    interaction_integral = interaction_integral
  )
}


#' Create LightGBM iterative Logistic objective
#'
#' @param dummy_intensity Numeric vector of dummy intensities.
#' @param interaction_integral Numeric vector of interaction integrals.
#'
#' @return A Python callable implementing the iterative GBPP
#'   Logistic objective.
#'
#' @keywords internal
make_lightgbm_iterative_logistic_objective <- function(
    dummy_intensity,
    interaction_integral) {

  if (!is.numeric(dummy_intensity)) {
    stop(
      "`dummy_intensity` must be numeric.",
      call. = FALSE
    )
  }

  if (!is.numeric(interaction_integral)) {
    stop(
      "`interaction_integral` must be numeric.",
      call. = FALSE
    )
  }

  if (length(dummy_intensity) != length(interaction_integral)) {
    stop(
      paste(
        "`dummy_intensity` and `interaction_integral`",
        "must have the same length."
      ),
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

  module <- .gbpp_lightgbm_module()

  module$make_iterative_logistic_objective(
    dummy_intensity = dummy_intensity,
    interaction_integral = interaction_integral
  )
}


# ============================================================
# Internal helper: default LightGBM parameters
# ============================================================

.gbpp_lightgbm_default_params <- function() {

  list(
    boosting = "gbdt",
    learning_rate = 0.05,
    max_depth = 6L,
    verbosity = -1L
  )
}


# ============================================================
# Fit: Poisson
# ============================================================

#' Fit a GBPP LightGBM Poisson model
#'
#' @param data A `gbpp_data` object.
#' @param params A list of LightGBM parameters.
#' @param num_boost_round Number of boosting rounds.
#'
#' @return A fitted LightGBM Booster.
#'
#' @export
fit_gbpp_lightgbm <- function(
    data,
    params = list(),
    num_boost_round = 100L) {

  validate_gbpp_data(data)

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

  lgb <- reticulate::import("lightgbm")

  dtrain <- lgb$Dataset(
    data = as.matrix(data$X),
    label = data$y,
    feature_name = colnames(data$X)
  )

  objective <- make_lightgbm_poisson_objective(
    data$quadrature_weight
  )

  train_params <- utils::modifyList(
    .gbpp_lightgbm_default_params(),
    params
  )

  train_params$objective <- objective

  model <- lgb$train(
    params = train_params,
    train_set = dtrain,
    num_boost_round = as.integer(num_boost_round)
  )

  mark_gbpp_lightgbm(model)
}


# ============================================================
# Fit: Weighted Poisson
# ============================================================

#' Fit a GBPP LightGBM weighted Poisson model
#'
#' @param data A `gbpp_data` object.
#' @param params A list of LightGBM parameters.
#' @param num_boost_round Number of boosting rounds.
#'
#' @return A fitted LightGBM Booster.
#'
#' @export
fit_gbpp_lightgbm_weighted_poisson <- function(
    data,
    params = list(),
    num_boost_round = 100L) {

  validate_gbpp_data(data)

  if (is.null(data$statistical_weight)) {
    stop(
      "`statistical_weight` is required for weighted Poisson.",
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

  lgb <- reticulate::import("lightgbm")

  dtrain <- lgb$Dataset(
    data = as.matrix(data$X),
    label = data$y,
    feature_name = colnames(data$X)
  )

  objective <- make_lightgbm_weighted_poisson_objective(
    data$quadrature_weight,
    data$statistical_weight
  )

  train_params <- utils::modifyList(
    .gbpp_lightgbm_default_params(),
    params
  )

  train_params$objective <- objective

  model <- lgb$train(
    params = train_params,
    train_set = dtrain,
    num_boost_round = as.integer(num_boost_round)
  )

  mark_gbpp_lightgbm(model)
}


# ============================================================
# Fit: Logistic
# ============================================================

#' Fit a GBPP LightGBM Logistic model
#'
#' @param data A `gbpp_data` object.
#' @param params A list of LightGBM parameters.
#' @param num_boost_round Number of boosting rounds.
#'
#' @return A fitted LightGBM Booster.
#'
#' @export
fit_gbpp_lightgbm_logistic <- function(
    data,
    params = list(),
    num_boost_round = 100L) {

  validate_gbpp_data(data)

  if (is.null(data$dummy_intensity)) {
    stop(
      "`dummy_intensity` is required for Logistic.",
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

  lgb <- reticulate::import("lightgbm")

  dtrain <- lgb$Dataset(
    data = as.matrix(data$X),
    label = data$y,
    feature_name = colnames(data$X)
  )

  objective <- make_lightgbm_logistic_objective(
    data$dummy_intensity
  )

  train_params <- utils::modifyList(
    .gbpp_lightgbm_default_params(),
    params
  )

  train_params$objective <- objective

  model <- lgb$train(
    params = train_params,
    train_set = dtrain,
    num_boost_round = as.integer(num_boost_round)
  )

  mark_gbpp_lightgbm(model)
}


# ============================================================
# Fit: Weighted Logistic
# ============================================================

#' Fit a GBPP LightGBM weighted Logistic model
#'
#' @param data A `gbpp_data` object.
#' @param params A list of LightGBM parameters.
#' @param num_boost_round Number of boosting rounds.
#'
#' @return A fitted LightGBM Booster.
#'
#' @export
fit_gbpp_lightgbm_weighted_logistic <- function(
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

  lgb <- reticulate::import("lightgbm")

  dtrain <- lgb$Dataset(
    data = as.matrix(data$X),
    label = data$y,
    feature_name = colnames(data$X)
  )

  objective <- make_lightgbm_weighted_logistic_objective(
    data$dummy_intensity,
    data$statistical_weight
  )

  train_params <- utils::modifyList(
    .gbpp_lightgbm_default_params(),
    params
  )

  train_params$objective <- objective

  model <- lgb$train(
    params = train_params,
    train_set = dtrain,
    num_boost_round = as.integer(num_boost_round)
  )

  mark_gbpp_lightgbm(model)
}


# ============================================================
# Fit: Iterative Poisson
# ============================================================

#' Fit a GBPP LightGBM iterative Poisson model
#'
#' @param data A `gbpp_data` object.
#' @param params A list of LightGBM parameters.
#' @param num_boost_round Number of boosting rounds.
#'
#' @return A fitted LightGBM Booster.
#'
#' @export
fit_gbpp_lightgbm_iterative_poisson <- function(
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

  lgb <- reticulate::import("lightgbm")

  dtrain <- lgb$Dataset(
    data = as.matrix(data$X),
    label = data$y,
    feature_name = colnames(data$X)
  )

  objective <- make_lightgbm_iterative_poisson_objective(
    data$quadrature_weight,
    data$interaction_integral
  )

  train_params <- utils::modifyList(
    .gbpp_lightgbm_default_params(),
    params
  )

  train_params$objective <- objective

  model <- lgb$train(
    params = train_params,
    train_set = dtrain,
    num_boost_round = as.integer(num_boost_round)
  )

  mark_gbpp_lightgbm(model)
}


# ============================================================
# Fit: Iterative Logistic
# ============================================================

#' Fit a GBPP LightGBM iterative Logistic model
#'
#' @param data A `gbpp_data` object.
#' @param params A list of LightGBM parameters.
#' @param num_boost_round Number of boosting rounds.
#'
#' @return A fitted LightGBM Booster.
#'
#' @export
fit_gbpp_lightgbm_iterative_logistic <- function(
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

  lgb <- reticulate::import("lightgbm")

  dtrain <- lgb$Dataset(
    data = as.matrix(data$X),
    label = data$y,
    feature_name = colnames(data$X)
  )

  objective <- make_lightgbm_iterative_logistic_objective(
    data$dummy_intensity,
    data$interaction_integral
  )

  train_params <- utils::modifyList(
    .gbpp_lightgbm_default_params(),
    params
  )

  train_params$objective <- objective

  model <- lgb$train(
    params = train_params,
    train_set = dtrain,
    num_boost_round = as.integer(num_boost_round)
  )

  mark_gbpp_lightgbm(model)
}

#' Predict from a GBPP LightGBM model
#'
#' @param object A fitted `gbpp_lightgbm` model.
#' @param newdata A numeric matrix or data frame containing predictor
#'   values.
#' @param ... Additional arguments passed to LightGBM prediction.
#'
#' @return A numeric vector of predictions.
#'
#' @export
predict.gbpp_lightgbm <- function(object, newdata, ...) {

  if (!inherits(object, "gbpp_lightgbm")) {
    stop(
      "`object` must be a fitted `gbpp_lightgbm` model.",
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

  if (!is.matrix(newdata)) {
    stop(
      "`newdata` must be a numeric matrix or data frame.",
      call. = FALSE
    )
  }

  if (!is.numeric(newdata)) {
    stop(
      "`newdata` must be numeric.",
      call. = FALSE
    )
  }

  if (any(!is.finite(newdata))) {
    stop(
      "`newdata` must contain only finite numeric values.",
      call. = FALSE
    )
  }

  prediction <- object$predict(
    data = newdata,
    ...
  )

  prediction <- reticulate::py_to_r(prediction)

  as.numeric(prediction)
}
