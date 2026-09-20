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
    "gbpp-xgboost",
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

  final_params <- modifyList(
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

  final_params <- modifyList(
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
