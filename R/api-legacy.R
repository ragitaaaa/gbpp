# ============================================================
# GBPP - Backward-Compatible Legacy API Shims
# ============================================================

# ------------------------------------------------------------
# XGBoost Legacy Shims
# ------------------------------------------------------------

#' Fit a GBPP XGBoost Poisson model (Legacy Shim)
#'
#' @inheritParams fit_gbpp
#' @param nrounds Number of boosting rounds.
#' @param num_boost_round Alias for `nrounds`.
#' @return A fitted model of class `gbpp_xgboost`.
#' @export
fit_gbpp_xgboost <- function(data, params = list(), nrounds = 100, num_boost_round = nrounds, objective = "poisson", ...) {
  fit_gbpp(
    data = data,
    engine = "xgboost",
    objective = objective,
    n_trees = num_boost_round,
    params = params,
    ...
  )
}

#' Fit a GBPP XGBoost weighted Poisson model (Legacy Shim)
#'
#' @inheritParams fit_gbpp_xgboost
#' @export
fit_gbpp_xgboost_weighted_poisson <- function(data, params = list(), nrounds = 100, num_boost_round = nrounds, ...) {
  fit_gbpp(
    data = data,
    engine = "xgboost",
    objective = "weighted_poisson",
    n_trees = num_boost_round,
    params = params,
    ...
  )
}

#' Fit a GBPP XGBoost logistic model (Legacy Shim)
#'
#' @inheritParams fit_gbpp_xgboost
#' @export
fit_gbpp_xgboost_logistic <- function(data, params = list(), nrounds = 100, num_boost_round = nrounds, ...) {
  fit_gbpp(
    data = data,
    engine = "xgboost",
    objective = "logistic",
    n_trees = num_boost_round,
    params = params,
    ...
  )
}

#' Fit a GBPP XGBoost weighted logistic model (Legacy Shim)
#'
#' @inheritParams fit_gbpp_xgboost
#' @export
fit_gbpp_xgboost_weighted_logistic <- function(data, params = list(), nrounds = 100, num_boost_round = nrounds, ...) {
  fit_gbpp(
    data = data,
    engine = "xgboost",
    objective = "weighted_logistic",
    n_trees = num_boost_round,
    params = params,
    ...
  )
}

#' Fit a GBPP XGBoost iterative Poisson model (Legacy Shim)
#'
#' @inheritParams fit_gbpp_xgboost
#' @export
fit_gbpp_xgboost_iterative_poisson <- function(data, params = list(), nrounds = 100, num_boost_round = nrounds, ...) {
  fit_gbpp(
    data = data,
    engine = "xgboost",
    objective = "iterative_poisson",
    n_trees = num_boost_round,
    params = params,
    ...
  )
}

#' Fit a GBPP XGBoost iterative logistic model (Legacy Shim)
#'
#' @inheritParams fit_gbpp_xgboost
#' @export
fit_gbpp_xgboost_iterative_logistic <- function(data, params = list(), nrounds = 100, num_boost_round = nrounds, ...) {
  fit_gbpp(
    data = data,
    engine = "xgboost",
    objective = "iterative_logistic",
    n_trees = num_boost_round,
    params = params,
    ...
  )
}

# ------------------------------------------------------------
# LightGBM Legacy Shims
# ------------------------------------------------------------

#' Fit a GBPP LightGBM Poisson model (Legacy Shim)
#'
#' @inheritParams fit_gbpp
#' @param num_boost_round Number of boosting rounds.
#' @param nrounds Alias for `num_boost_round`.
#' @return A fitted model of class `gbpp_lightgbm`.
#' @export
fit_gbpp_lightgbm <- function(data, params = list(), num_boost_round = 100L, nrounds = num_boost_round, objective = "poisson", ...) {
  fit_gbpp(
    data = data,
    engine = "lightgbm",
    objective = objective,
    n_trees = num_boost_round,
    params = params,
    ...
  )
}

#' Fit a GBPP LightGBM weighted Poisson model (Legacy Shim)
#'
#' @inheritParams fit_gbpp_lightgbm
#' @export
fit_gbpp_lightgbm_weighted_poisson <- function(data, params = list(), num_boost_round = 100L, nrounds = num_boost_round, ...) {
  fit_gbpp(
    data = data,
    engine = "lightgbm",
    objective = "weighted_poisson",
    n_trees = num_boost_round,
    params = params,
    ...
  )
}

#' Fit a GBPP LightGBM logistic model (Legacy Shim)
#'
#' @inheritParams fit_gbpp_lightgbm
#' @export
fit_gbpp_lightgbm_logistic <- function(data, params = list(), num_boost_round = 100L, nrounds = num_boost_round, ...) {
  fit_gbpp(
    data = data,
    engine = "lightgbm",
    objective = "logistic",
    n_trees = num_boost_round,
    params = params,
    ...
  )
}

#' Fit a GBPP LightGBM weighted logistic model (Legacy Shim)
#'
#' @inheritParams fit_gbpp_lightgbm
#' @export
fit_gbpp_lightgbm_weighted_logistic <- function(data, params = list(), num_boost_round = 100L, nrounds = num_boost_round, ...) {
  fit_gbpp(
    data = data,
    engine = "lightgbm",
    objective = "weighted_logistic",
    n_trees = num_boost_round,
    params = params,
    ...
  )
}

#' Fit a GBPP LightGBM iterative Poisson model (Legacy Shim)
#'
#' @inheritParams fit_gbpp_lightgbm
#' @export
fit_gbpp_lightgbm_iterative_poisson <- function(data, params = list(), num_boost_round = 100L, nrounds = num_boost_round, ...) {
  fit_gbpp(
    data = data,
    engine = "lightgbm",
    objective = "iterative_poisson",
    n_trees = num_boost_round,
    params = params,
    ...
  )
}

#' Fit a GBPP LightGBM iterative logistic model (Legacy Shim)
#'
#' @inheritParams fit_gbpp_lightgbm
#' @export
fit_gbpp_lightgbm_iterative_logistic <- function(data, params = list(), num_boost_round = 100L, nrounds = num_boost_round, ...) {
  fit_gbpp(
    data = data,
    engine = "lightgbm",
    objective = "iterative_logistic",
    n_trees = num_boost_round,
    params = params,
    ...
  )
}

# ------------------------------------------------------------
# CatBoost Legacy Shims
# ------------------------------------------------------------

#' Fit a GBPP CatBoost Poisson model (Legacy Shim)
#'
#' @inheritParams fit_gbpp
#' @param nrounds Number of boosting rounds.
#' @param num_boost_round Alias for `nrounds`.
#' @return A fitted model of class `gbpp_catboost`.
#' @export
fit_gbpp_catboost <- function(data, params = list(), nrounds = 100L, num_boost_round = nrounds, objective = "poisson", ...) {
  fit_gbpp(
    data = data,
    engine = "catboost",
    objective = objective,
    n_trees = num_boost_round,
    params = params,
    ...
  )
}

#' Fit a GBPP CatBoost weighted Poisson model (Legacy Shim)
#'
#' @inheritParams fit_gbpp_catboost
#' @export
fit_gbpp_catboost_weighted_poisson <- function(data, params = list(), nrounds = 100L, num_boost_round = nrounds, ...) {
  fit_gbpp(
    data = data,
    engine = "catboost",
    objective = "weighted_poisson",
    n_trees = num_boost_round,
    params = params,
    ...
  )
}

#' Fit a GBPP CatBoost logistic model (Legacy Shim)
#'
#' @inheritParams fit_gbpp_catboost
#' @export
fit_gbpp_catboost_logistic <- function(data, params = list(), nrounds = 100L, num_boost_round = nrounds, ...) {
  fit_gbpp(
    data = data,
    engine = "catboost",
    objective = "logistic",
    n_trees = num_boost_round,
    params = params,
    ...
  )
}

#' Fit a GBPP CatBoost weighted logistic model (Legacy Shim)
#'
#' @inheritParams fit_gbpp_catboost
#' @export
fit_gbpp_catboost_weighted_logistic <- function(data, params = list(), nrounds = 100L, num_boost_round = nrounds, ...) {
  fit_gbpp(
    data = data,
    engine = "catboost",
    objective = "weighted_logistic",
    n_trees = num_boost_round,
    params = params,
    ...
  )
}

#' Fit a GBPP CatBoost iterative Poisson model (Legacy Shim)
#'
#' @inheritParams fit_gbpp_catboost
#' @export
fit_gbpp_catboost_iterative_poisson <- function(data, params = list(), nrounds = 100L, num_boost_round = nrounds, ...) {
  fit_gbpp(
    data = data,
    engine = "catboost",
    objective = "iterative_poisson",
    n_trees = num_boost_round,
    params = params,
    ...
  )
}

#' Fit a GBPP CatBoost iterative logistic model (Legacy Shim)
#'
#' @inheritParams fit_gbpp_catboost
#' @export
fit_gbpp_catboost_iterative_logistic <- function(data, params = list(), nrounds = 100L, num_boost_round = nrounds, ...) {
  fit_gbpp(
    data = data,
    engine = "catboost",
    objective = "iterative_logistic",
    n_trees = num_boost_round,
    params = params,
    ...
  )
}
