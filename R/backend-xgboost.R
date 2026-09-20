#' Create an XGBoost custom objective for the GBPP Poisson loss
#'
#' Creates a Python callable implementing the GBPP Poisson
#' gradient and Hessian.
#'
#' @param quadrature_weight Numeric vector of quadrature weights.
#'
#' @return A Python callable suitable for `xgboost::train()`.
#' @keywords internal
make_xgboost_poisson_objective <- function(quadrature_weight) {

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

  configure_gbpp_python()

  python_dir <- gbpp_python_path()

  backend <- reticulate::import_from_path(
    "gbpp_xgboost",
    path = python_dir
  )

  backend$make_poisson_objective(
    reticulate::r_to_py(
      as.numeric(quadrature_weight)
    )
  )
}

#' Create an XGBoost custom objective for the GBPP weighted Poisson loss
#'
#' Creates a Python callable implementing the GBPP weighted Poisson
#' gradient and Hessian.
#'
#' @param quadrature_weight Numeric vector of quadrature weights.
#' @param statistical_weight Numeric vector of statistical weights.
#'
#' @return A Python callable suitable for `xgboost::train()`.
#' @keywords internal
make_xgboost_weighted_poisson_objective <- function(
    quadrature_weight,
    statistical_weight
) {

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
      "`quadrature_weight` and `statistical_weight` must have ",
      "the same length.",
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

  configure_gbpp_python()

  python_dir <- gbpp_python_path()

  backend <- reticulate::import_from_path(
    "gbpp_xgboost",
    path = python_dir
  )

  backend$make_weighted_poisson_objective(
    reticulate::r_to_py(
      as.numeric(quadrature_weight)
    ),
    reticulate::r_to_py(
      as.numeric(statistical_weight)
    )
  )
}

#' Create an XGBoost custom objective for the GBPP logistic loss
#'
#' Creates a Python callable implementing the GBPP logistic
#' gradient and Hessian.
#'
#' @param dummy_intensity Numeric vector of dummy-process intensities.
#'
#' @return A Python callable suitable for `xgboost::train()`.
#' @keywords internal
make_xgboost_logistic_objective <- function(
    dummy_intensity
) {

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

  configure_gbpp_python()

  python_dir <- gbpp_python_path()

  backend <- reticulate::import_from_path(
    "gbpp_xgboost",
    path = python_dir
  )

  backend$make_logistic_objective(
    reticulate::r_to_py(
      as.numeric(dummy_intensity)
    )
  )
}

#' Create an XGBoost weighted logistic objective for GBPP
#'
#' Creates a custom XGBoost objective function for the weighted logistic
#' point-process model. The objective uses a fixed statistical/spatial
#' weight for each quadrature point.
#'
#' @param dummy_intensity Numeric vector containing the dummy-process
#'   intensity at each quadrature point.
#' @param statistical_weight Numeric vector containing the fixed
#'   statistical/spatial weight at each quadrature point.
#'
#' @return A Python callable suitable for use as a custom XGBoost
#'   objective.
#'
#' @keywords internal
make_xgboost_weighted_logistic_objective <- function(
    dummy_intensity,
    statistical_weight
) {

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
      "`dummy_intensity` and `statistical_weight` must have the same length.",
      call. = FALSE
    )
  }

  if (any(!is.finite(dummy_intensity)) ||
      any(dummy_intensity <= 0)) {
    stop(
      "`dummy_intensity` must contain positive finite values.",
      call. = FALSE
    )
  }

  if (any(!is.finite(statistical_weight))) {
    stop(
      "`statistical_weight` must contain only finite values.",
      call. = FALSE
    )
  }

  configure_gbpp_python()

  python_dir <- gbpp_python_path()

  backend <- reticulate::import_from_path(
    "gbpp_xgboost",
    path = python_dir
  )

  backend$make_weighted_logistic_objective(
    reticulate::r_to_py(
      as.numeric(dummy_intensity)
    ),
    reticulate::r_to_py(
      as.numeric(statistical_weight)
    )
  )
}

#' Create an XGBoost iteratively weighted Poisson objective for GBPP
#'
#' Creates a custom XGBoost objective in which the spatial
#' dependence-adjusted weight is recalculated from the current
#' log-intensity prediction.
#'
#' @param quadrature_weight Numeric vector of Berman-Turner
#'   quadrature weights.
#' @param interaction_integral Numeric vector containing the spatial
#'   interaction integral for each quadrature point.
#'
#' @return A Python callable suitable for use as a custom XGBoost
#'   objective.
#'
#' @keywords internal
make_xgboost_iterative_poisson_objective <- function(
    quadrature_weight,
    interaction_integral
) {

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

  configure_gbpp_python()

  python_dir <- gbpp_python_path()

  backend <- reticulate::import_from_path(
    "gbpp_xgboost",
    path = python_dir
  )

  backend$make_iterative_poisson_objective(
    reticulate::r_to_py(
      as.numeric(quadrature_weight)
    ),
    reticulate::r_to_py(
      as.numeric(interaction_integral)
    )
  )
}

#' Create an XGBoost iteratively weighted Logistic objective for GBPP
#'
#' Creates a custom XGBoost objective in which the spatial
#' dependence-adjusted weight is recalculated from the current
#' log-intensity prediction.
#'
#' @param dummy_intensity Numeric vector containing the dummy-process
#'   intensity at each quadrature point.
#' @param interaction_integral Numeric vector containing the spatial
#'   interaction integral for each quadrature point.
#'
#' @return A Python callable suitable for use as a custom XGBoost
#'   objective.
#'
#' @keywords internal
make_xgboost_iterative_logistic_objective <- function(
    dummy_intensity,
    interaction_integral
) {

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
      "`dummy_intensity` and `interaction_integral` must have the same length.",
      call. = FALSE
    )
  }

  if (any(!is.finite(dummy_intensity)) ||
      any(dummy_intensity <= 0)) {
    stop(
      "`dummy_intensity` must contain positive finite values.",
      call. = FALSE
    )
  }

  if (any(!is.finite(interaction_integral))) {
    stop(
      "`interaction_integral` must contain only finite values.",
      call. = FALSE
    )
  }

  configure_gbpp_python()

  python_dir <- gbpp_python_path()

  backend <- reticulate::import_from_path(
    "gbpp_xgboost",
    path = python_dir
  )

  backend$make_iterative_logistic_objective(
    reticulate::r_to_py(
      as.numeric(dummy_intensity)
    ),
    reticulate::r_to_py(
      as.numeric(interaction_integral)
    )
  )
}

#' Fit a GBPP Poisson model using XGBoost
#'
#' @param data A `gbpp_data` object.
#' @param params Named list of XGBoost parameters.
#' @param nrounds Number of boosting rounds.
#'
#' @return A fitted XGBoost Booster.
#' @keywords internal
fit_gbpp_xgboost <- function(
    data,
    params = list(),
    nrounds = 100
) {

  validate_gbpp_data(
    data,
    objective = "poisson"
  )

  if (length(params) > 0L && is.null(names(params))) {
    stop(
      "`params` must be a named list.",
      call. = FALSE
    )
  }

  if (length(nrounds) != 1L ||
      !is.numeric(nrounds) ||
      !is.finite(nrounds) ||
      nrounds < 1) {
    stop(
      "`nrounds` must be a positive integer.",
      call. = FALSE
    )
  }

  configure_gbpp_python()

  xgb <- reticulate::import("xgboost")

  X <- as.matrix(data$X)
  y <- as.numeric(data$y)

  dtrain <- xgb$DMatrix(
    data = X,
    label = y
  )

  if (!is.null(colnames(data$X))) {
    dtrain$feature_names <- colnames(data$X)
  }

  objective <- make_xgboost_poisson_objective(
    data$quadrature_weight
  )

  default_params <- list(
    booster = "gbtree",
    eta = 0.05,
    max_depth = 6L,
    verbosity = 0L
  )

  final_params <- utils::modifyList(
    default_params,
    params
  )

  model <- xgb$train(
    params = final_params,
    dtrain = dtrain,
    obj = objective,
    num_boost_round = as.integer(nrounds)
  )

  model
}

#' Fit a GBPP weighted Poisson model using XGBoost
#'
#' @param data A `gbpp_data` object.
#' @param params Named list of XGBoost parameters.
#' @param nrounds Number of boosting rounds.
#'
#' @return A fitted XGBoost Booster.
#' @keywords internal
fit_gbpp_xgboost_weighted_poisson <- function(
    data,
    params = list(),
    nrounds = 100
) {

  validate_gbpp_data(
    data,
    objective = "weighted_poisson"
  )

  if (length(params) > 0L && is.null(names(params))) {
    stop(
      "`params` must be a named list.",
      call. = FALSE
    )
  }

  if (length(nrounds) != 1L ||
      !is.numeric(nrounds) ||
      !is.finite(nrounds) ||
      nrounds < 1) {
    stop(
      "`nrounds` must be a positive integer.",
      call. = FALSE
    )
  }

  configure_gbpp_python()

  xgb <- reticulate::import("xgboost")

  X <- as.matrix(data$X)
  y <- as.numeric(data$y)

  dtrain <- xgb$DMatrix(
    data = X,
    label = y
  )

  if (!is.null(colnames(data$X))) {
    dtrain$feature_names <- colnames(data$X)
  }

  objective <- make_xgboost_weighted_poisson_objective(
    quadrature_weight = data$quadrature_weight,
    statistical_weight = data$statistical_weight
  )

  default_params <- list(
    booster = "gbtree",
    eta = 0.05,
    max_depth = 6L,
    verbosity = 0L
  )

  final_params <- utils::modifyList(
    default_params,
    params
  )

  model <- xgb$train(
    params = final_params,
    dtrain = dtrain,
    obj = objective,
    num_boost_round = as.integer(nrounds)
  )

  model
}

#' Fit a GBPP logistic model using XGBoost
#'
#' @param data A `gbpp_data` object.
#' @param params Named list of XGBoost parameters.
#' @param nrounds Number of boosting rounds.
#'
#' @return A fitted XGBoost Booster.
#' @keywords internal
fit_gbpp_xgboost_logistic <- function(
    data,
    params = list(),
    nrounds = 100
) {

  validate_gbpp_data(
    data,
    objective = "logistic"
  )

  if (length(params) > 0L && is.null(names(params))) {
    stop(
      "`params` must be a named list.",
      call. = FALSE
    )
  }

  if (length(nrounds) != 1L ||
      !is.numeric(nrounds) ||
      !is.finite(nrounds) ||
      nrounds < 1) {
    stop(
      "`nrounds` must be a positive integer.",
      call. = FALSE
    )
  }

  configure_gbpp_python()

  xgb <- reticulate::import("xgboost")

  X <- as.matrix(data$X)
  y <- as.numeric(data$y)

  dtrain <- xgb$DMatrix(
    data = X,
    label = y
  )

  if (!is.null(colnames(data$X))) {
    dtrain$feature_names <- colnames(data$X)
  }

  objective <- make_xgboost_logistic_objective(
    dummy_intensity = data$dummy_intensity
  )

  default_params <- list(
    booster = "gbtree",
    eta = 0.05,
    max_depth = 6L,
    verbosity = 0L
  )

  final_params <- utils::modifyList(
    default_params,
    params
  )

  model <- xgb$train(
    params = final_params,
    dtrain = dtrain,
    obj = objective,
    num_boost_round = as.integer(nrounds)
  )

  model
}

#' Fit a GBPP weighted logistic model using XGBoost
#'
#' Fits a gradient boosting model for a spatial point process using
#' the weighted logistic objective implemented as a custom XGBoost
#' objective function.
#'
#' @param data A `gbpp_data` object prepared for the
#'   `"weighted_logistic"` objective.
#' @param params A named list of XGBoost training parameters.
#' @param num_boost_round Number of boosting iterations.
#'
#' @return A Python XGBoost Booster object.
#'
#' @keywords internal
fit_gbpp_xgboost_weighted_logistic <- function(
    data,
    params = list(),
    num_boost_round = 100L
) {

  validate_gbpp_data(
    data,
    objective = "weighted_logistic"
  )

  if (!is.list(params)) {
    stop(
      "`params` must be a list.",
      call. = FALSE
    )
  }

  if (length(num_boost_round) != 1L ||
      !is.numeric(num_boost_round) ||
      !is.finite(num_boost_round) ||
      num_boost_round < 1) {
    stop(
      "`num_boost_round` must be a positive integer.",
      call. = FALSE
    )
  }

  xgb <- reticulate::import("xgboost")

  dtrain <- xgb$DMatrix(
    data = as.matrix(data$X),
    label = as.numeric(data$y)
  )

  dtrain$feature_names <- colnames(data$X)

  objective <- make_xgboost_weighted_logistic_objective(
    dummy_intensity = data$dummy_intensity,
    statistical_weight = data$statistical_weight
  )

  default_params <- list(
    booster = "gbtree",
    eta = 0.05,
    max_depth = 6L,
    verbosity = 0L
  )

  final_params <- utils::modifyList(
    default_params,
    params
  )

  model <- xgb$train(
    params = final_params,
    dtrain = dtrain,
    obj = objective,
    num_boost_round = as.integer(num_boost_round)
  )

  model
}

#' Fit an iteratively weighted Poisson GBPP model using XGBoost
#'
#' Fits a spatial point-process intensity model using the iteratively
#' weighted Poisson objective. The spatial dependence-adjusted weight
#' is recalculated from the current model prediction during boosting.
#'
#' @param data A `gbpp_data` object prepared for the
#'   `"iterative_poisson"` objective.
#' @param params A named list of XGBoost training parameters.
#' @param num_boost_round Number of boosting iterations.
#'
#' @return A Python XGBoost Booster object.
#'
#' @keywords internal
fit_gbpp_xgboost_iterative_poisson <- function(
    data,
    params = list(),
    num_boost_round = 100L
) {

  validate_gbpp_data(
    data,
    objective = "iterative_poisson"
  )

  if (!is.list(params)) {
    stop(
      "`params` must be a list.",
      call. = FALSE
    )
  }

  if (length(num_boost_round) != 1L ||
      !is.numeric(num_boost_round) ||
      !is.finite(num_boost_round) ||
      num_boost_round < 1) {
    stop(
      "`num_boost_round` must be a positive integer.",
      call. = FALSE
    )
  }

  configure_gbpp_python()

  xgb <- reticulate::import("xgboost")

  dtrain <- xgb$DMatrix(
    data = as.matrix(data$X),
    label = as.numeric(data$y)
  )

  dtrain$feature_names <- colnames(data$X)

  objective <- make_xgboost_iterative_poisson_objective(
    quadrature_weight = data$quadrature_weight,
    interaction_integral = data$interaction_integral
  )

  default_params <- list(
    booster = "gbtree",
    eta = 0.05,
    max_depth = 6L,
    verbosity = 0L
  )

  final_params <- utils::modifyList(
    default_params,
    params
  )

  model <- xgb$train(
    params = final_params,
    dtrain = dtrain,
    obj = objective,
    num_boost_round = as.integer(num_boost_round)
  )

  model
}

#' Fit an iteratively weighted Logistic GBPP model using XGBoost
#'
#' Fits a spatial point-process intensity model using the iteratively
#' weighted Logistic objective. The spatial dependence-adjusted weight
#' is recalculated from the current model prediction during boosting.
#'
#' @param data A `gbpp_data` object prepared for the
#'   `"iterative_logistic"` objective.
#' @param params A named list of XGBoost training parameters.
#' @param num_boost_round Number of boosting iterations.
#'
#' @return A Python XGBoost Booster object.
#'
#' @keywords internal
fit_gbpp_xgboost_iterative_logistic <- function(
    data,
    params = list(),
    num_boost_round = 100L
) {

  validate_gbpp_data(
    data,
    objective = "iterative_logistic"
  )

  if (!is.list(params)) {
    stop(
      "`params` must be a list.",
      call. = FALSE
    )
  }

  if (length(num_boost_round) != 1L ||
      !is.numeric(num_boost_round) ||
      !is.finite(num_boost_round) ||
      num_boost_round < 1) {
    stop(
      "`num_boost_round` must be a positive integer.",
      call. = FALSE
    )
  }

  configure_gbpp_python()

  xgb <- reticulate::import("xgboost")

  dtrain <- xgb$DMatrix(
    data = as.matrix(data$X),
    label = as.numeric(data$y)
  )

  dtrain$feature_names <- colnames(data$X)

  objective <- make_xgboost_iterative_logistic_objective(
    dummy_intensity = data$dummy_intensity,
    interaction_integral = data$interaction_integral
  )

  default_params <- list(
    booster = "gbtree",
    eta = 0.05,
    max_depth = 6L,
    verbosity = 0L
  )

  final_params <- utils::modifyList(
    default_params,
    params
  )

  model <- xgb$train(
    params = final_params,
    dtrain = dtrain,
    obj = objective,
    num_boost_round = as.integer(num_boost_round)
  )

  model
}
