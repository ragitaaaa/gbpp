#' Fit a Gradient Boosted Point Process Model
#'
#' Fits a gradient boosting model to spatial point process data on planar
#' or linear network domains using point-process specific objectives and
#' modern gradient boosting backends (XGBoost, LightGBM, CatBoost).
#'
#' @param data An object of class `gbpp_data` constructed via [prepare_gbpp()]
#'   or [prepare_gbpp_network()].
#' @param engine Boosting backend engine: `"xgboost"`, `"lightgbm"`, or `"catboost"`.
#' @param objective Point process objective function: `"poisson"`,
#'   `"weighted_poisson"`, `"logistic"`, `"weighted_logistic"`,
#'   `"iterative_poisson"`, or `"iterative_logistic"`.
#' @param n_trees Number of boosting rounds (trees). Default is `100L`.
#' @param learning_rate Learning rate (shrinkage factor). Default is `0.05`.
#' @param max_depth Maximum tree depth. Default is `6L`.
#' @param subsample Row subsampling ratio in `(0, 1]`. Default is `1.0`.
#' @param colsample_bytree Column (feature) subsampling ratio in `(0, 1]`. Default is `1.0`.
#' @param random_seed Random seed for reproducibility. Default is `123L`.
#' @param verbose Logical flag indicating whether training progress is printed. Default is `FALSE`.
#' @param baseline Optional numeric scalar for the initial log-intensity intercept \eqn{\eta_0}.
#'   If `NULL`, it is automatically initialized to \eqn{\log(\sum y / \sum v)} for Poisson objectives.
#' @param params Named list of backend-specific parameters to override or supplement defaults.
#' @param ... Additional arguments passed to the backend fit call.
#'
#' @return An object of class `c(paste0("gbpp_", engine), "gbpp_model")`.
#' @export
fit_gbpp <- function(
    data,
    engine = c("xgboost", "lightgbm", "catboost"),
    objective = c(
      "poisson",
      "weighted_poisson",
      "logistic",
      "weighted_logistic",
      "iterative_poisson",
      "iterative_logistic"
    ),
    n_trees = 100L,
    learning_rate = 0.05,
    max_depth = 6L,
    subsample = 1.0,
    colsample_bytree = 1.0,
    random_seed = 123L,
    verbose = FALSE,
    baseline = NULL,
    params = list(),
    ...
) {
  engine <- match.arg(engine)
  objective <- match.arg(objective)

  if (!inherits(data, "gbpp_data")) {
    stop("`data` must be a `gbpp_data` object.", call. = FALSE)
  }

  norm <- .gbpp_normalize_params(
    engine = engine,
    n_trees = n_trees,
    learning_rate = learning_rate,
    max_depth = max_depth,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    random_seed = random_seed,
    verbose = verbose,
    user_params = params
  )

  switch(
    engine,
    xgboost = .gbpp_fit_xgboost(
      data = data,
      objective = objective,
      params = norm$params,
      n_trees = norm$n_trees,
      baseline = baseline,
      ...
    ),
    lightgbm = .gbpp_fit_lightgbm(
      data = data,
      objective = objective,
      params = norm$params,
      n_trees = norm$n_trees,
      baseline = baseline,
      ...
    ),
    catboost = .gbpp_fit_catboost(
      data = data,
      objective = objective,
      params = norm$params,
      n_trees = norm$n_trees,
      baseline = baseline,
      ...
    )
  )
}
