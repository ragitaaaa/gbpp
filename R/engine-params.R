#' Standardize and translate hyperparameters across boosting engines
#'
#' Normalizes common gradient boosting hyperparameters into engine-specific
#' parameter lists for XGBoost, LightGBM, and CatBoost.
#'
#' @param engine Engine name: `"xgboost"`, `"lightgbm"`, or `"catboost"`.
#' @param n_trees Number of boosting trees/iterations.
#' @param learning_rate Learning rate (shrinkage).
#' @param max_depth Maximum tree depth.
#' @param subsample Row subsampling ratio.
#' @param colsample_bytree Column (feature) subsampling ratio.
#' @param random_seed Random seed for reproducibility.
#' @param verbose Logical flag indicating whether training progress should be printed.
#' @param user_params List of user-supplied backend-specific parameters to override or supplement.
#'
#' @return A named list of engine-ready training parameters.
#' @keywords internal
#' @export
.gbpp_normalize_params <- function(
    engine = c("xgboost", "lightgbm", "catboost"),
    n_trees = 100L,
    learning_rate = 0.05,
    max_depth = 6L,
    subsample = 1.0,
    colsample_bytree = 1.0,
    random_seed = 123L,
    verbose = FALSE,
    user_params = list()
) {
  engine <- match.arg(engine)

  # Check standard parameters
  if (!is.numeric(n_trees) || length(n_trees) != 1L || !is.finite(n_trees) || n_trees < 1) {
    stop("`n_trees` must be a positive integer.", call. = FALSE)
  }
  n_trees <- as.integer(n_trees)

  if (!is.numeric(learning_rate) || length(learning_rate) != 1L || !is.finite(learning_rate) || learning_rate <= 0) {
    stop("`learning_rate` must be a positive number.", call. = FALSE)
  }

  if (!is.numeric(max_depth) || length(max_depth) != 1L || !is.finite(max_depth) || max_depth < 1) {
    stop("`max_depth` must be a positive integer.", call. = FALSE)
  }
  max_depth <- as.integer(max_depth)

  if (!is.numeric(subsample) || length(subsample) != 1L || !is.finite(subsample) || subsample <= 0 || subsample > 1) {
    stop("`subsample` must be a number in (0, 1].", call. = FALSE)
  }

  if (!is.numeric(colsample_bytree) || length(colsample_bytree) != 1L || !is.finite(colsample_bytree) || colsample_bytree <= 0 || colsample_bytree > 1) {
    stop("`colsample_bytree` must be a number in (0, 1].", call. = FALSE)
  }

  random_seed <- as.integer(random_seed)
  verbose <- isTRUE(verbose)

  resolved <- switch(
    engine,

    xgboost = {
      base <- list(
        booster = "gbtree",
        eta = learning_rate,
        max_depth = max_depth,
        subsample = subsample,
        colsample_bytree = colsample_bytree,
        seed = random_seed,
        verbosity = if (verbose) 1L else 0L
      )
      utils::modifyList(base, user_params)
    },

    lightgbm = {
      base <- list(
        learning_rate = learning_rate,
        max_depth = max_depth,
        bagging_fraction = subsample,
        bagging_freq = if (subsample < 1.0) 1L else 0L,
        feature_fraction = colsample_bytree,
        seed = random_seed,
        verbosity = if (verbose) 1L else -1L,
        num_leaves = as.integer(min(2L^max_depth, 131072L)),
        min_data_in_leaf = 1L
      )
      utils::modifyList(base, user_params)
    },

    catboost = {
      base <- list(
        iterations = n_trees,
        learning_rate = learning_rate,
        depth = max_depth,
        subsample = subsample,
        rsm = colsample_bytree,
        random_seed = random_seed,
        verbose = verbose,
        thread_count = 1L,
        eval_metric = "RMSE"
      )
      utils::modifyList(base, user_params)
    }
  )

  list(
    engine = engine,
    n_trees = n_trees,
    params = resolved
  )
}
