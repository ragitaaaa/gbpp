# ============================================================
# GBPP - CatBoost Backend
# ============================================================


# ------------------------------------------------------------
# Python module
# ------------------------------------------------------------

.gbpp_catboost_module <- local({
  module <- NULL

  function() {
    if (!is.null(module)) {
      return(module)
    }

    python_path <- file.path(
      gbpp_python_path(),
      "gbpp_catboost.py"
    )

    if (!file.exists(python_path)) {
      stop(
        "CatBoost Python backend not found at: ",
        python_path,
        call. = FALSE
      )
    }

    module <- reticulate::import_from_path(
      module = "gbpp_catboost",
      path = dirname(python_path),
      delay_load = FALSE
    )

    module
  }
})


# ------------------------------------------------------------
# CatBoost package
# ------------------------------------------------------------

.gbpp_catboost <- local({
  module <- NULL

  function() {
    if (is.null(module)) {
      module <- reticulate::import(
        "catboost",
        delay_load = FALSE
      )
    }

    module
  }
})


# ------------------------------------------------------------
# Model class
# ------------------------------------------------------------

mark_gbpp_catboost <- function(model) {
  class(model) <- c("gbpp_catboost", class(model))
  model
}


# ============================================================
# OBJECTIVE FACTORIES
# ============================================================


# ------------------------------------------------------------
# Ordinary Poisson
# ------------------------------------------------------------

make_catboost_poisson_objective <- function(
    quadrature_weight) {

  module <- .gbpp_catboost_module()

  module$PoissonObjective(
    quadrature_weight = quadrature_weight
  )
}


# ------------------------------------------------------------
# Weighted Poisson
# ------------------------------------------------------------

make_catboost_weighted_poisson_objective <- function(
    quadrature_weight,
    statistical_weight) {

  module <- .gbpp_catboost_module()

  module$WeightedPoissonObjective(
    quadrature_weight = quadrature_weight,
    statistical_weight = statistical_weight
  )
}


# ------------------------------------------------------------
# Logistic
# ------------------------------------------------------------

make_catboost_logistic_objective <- function(
    dummy_intensity) {

  module <- .gbpp_catboost_module()

  module$LogisticObjective(
    dummy_intensity = dummy_intensity
  )
}


# ------------------------------------------------------------
# Weighted Logistic
# ------------------------------------------------------------

make_catboost_weighted_logistic_objective <- function(
    dummy_intensity,
    statistical_weight) {

  module <- .gbpp_catboost_module()

  module$WeightedLogisticObjective(
    dummy_intensity = dummy_intensity,
    statistical_weight = statistical_weight
  )
}


# ------------------------------------------------------------
# Iterative Poisson
# ------------------------------------------------------------

make_catboost_iterative_poisson_objective <- function(
    quadrature_weight,
    interaction_integral) {

  module <- .gbpp_catboost_module()

  module$IterativePoissonObjective(
    quadrature_weight = quadrature_weight,
    interaction_integral = interaction_integral
  )
}


# ------------------------------------------------------------
# Iterative Logistic
# ------------------------------------------------------------

make_catboost_iterative_logistic_objective <- function(
    dummy_intensity,
    interaction_integral) {

  module <- .gbpp_catboost_module()

  module$IterativeLogisticObjective(
    dummy_intensity = dummy_intensity,
    interaction_integral = interaction_integral
  )
}


# ============================================================
# DEFAULT PARAMETERS
# ============================================================

.gbpp_catboost_default_params <- function() {
  list(
    iterations = 100L,
    learning_rate = 0.05,
    depth = 6L,
    verbose = FALSE,
    random_seed = 123L,
    thread_count = 1L,
    eval_metric = "RMSE"
  )
}


# ============================================================
# PARAMETER HELPER
# ============================================================

.gbpp_catboost_prepare_params <- function(
    params,
    objective) {

  defaults <- .gbpp_catboost_default_params()

  if (length(params) > 0L) {
    defaults[names(params)] <- params
  }

  defaults$loss_function <- objective

  defaults
}


# ============================================================
# MODEL FIT HELPER
# ============================================================

.fit_gbpp_catboost <- function(
    data,
    objective,
    params,
    nrounds) {

  if (!inherits(data, "gbpp_data")) {
    stop(
      "`data` must be a `gbpp_data` object.",
      call. = FALSE
    )
  }

  if (!is.matrix(data$X)) {
    stop(
      "`data$X` must be a matrix.",
      call. = FALSE
    )
  }

  if (!is.numeric(data$X)) {
    stop(
      "`data$X` must be numeric.",
      call. = FALSE
    )
  }

  if (any(!is.finite(data$X))) {
    stop(
      "`data$X` must contain only finite values.",
      call. = FALSE
    )
  }

  if (length(data$y) != nrow(data$X)) {
    stop(
      "`data$y` must have the same number of observations as `data$X`.",
      call. = FALSE
    )
  }

  if (!is.numeric(data$y)) {
    stop(
      "`data$y` must be numeric.",
      call. = FALSE
    )
  }

  if (any(!is.finite(data$y))) {
    stop(
      "`data$y` must contain only finite values.",
      call. = FALSE
    )
  }

  if (missing(nrounds) || is.null(nrounds)) {
    nrounds <- 100L
  }

  if (
    length(nrounds) != 1L ||
    !is.numeric(nrounds) ||
    !is.finite(nrounds) ||
    nrounds < 1
  ) {
    stop(
      "`nrounds` must be a positive finite number.",
      call. = FALSE
    )
  }

  nrounds <- as.integer(nrounds)

  train_params <- .gbpp_catboost_prepare_params(
    params = params,
    objective = objective
  )

  # Keep the number of iterations controlled by the explicit
  # nrounds argument, unless the user explicitly supplied
  # `iterations` inside params.
  if (!("iterations" %in% names(params))) {
    train_params$iterations <- nrounds
  }

  catboost <- .gbpp_catboost()

  model <- do.call(
    catboost$CatBoostRegressor,
    train_params
  )

  model$fit(
    X = data$X,
    y = data$y
  )

  mark_gbpp_catboost(model)
}


# ============================================================
# FIT FUNCTIONS
# ============================================================


# ------------------------------------------------------------
# Ordinary Poisson
# ------------------------------------------------------------

fit_gbpp_catboost <- function(
    data,
    params = list(),
    nrounds = 100L) {

  objective <- make_catboost_poisson_objective(
    quadrature_weight = data$quadrature_weight
  )

  .fit_gbpp_catboost(
    data = data,
    objective = objective,
    params = params,
    nrounds = nrounds
  )
}


# ------------------------------------------------------------
# Weighted Poisson
# ------------------------------------------------------------

fit_gbpp_catboost_weighted_poisson <- function(
    data,
    params = list(),
    nrounds = 100L) {

  if (is.null(data$statistical_weight)) {
    stop(
      "`statistical_weight` is required for the weighted Poisson objective.",
      call. = FALSE
    )
  }

  objective <- make_catboost_weighted_poisson_objective(
    quadrature_weight = data$quadrature_weight,
    statistical_weight = data$statistical_weight
  )

  .fit_gbpp_catboost(
    data = data,
    objective = objective,
    params = params,
    nrounds = nrounds
  )
}


# ------------------------------------------------------------
# Logistic
# ------------------------------------------------------------

fit_gbpp_catboost_logistic <- function(
    data,
    params = list(),
    nrounds = 100L) {

  if (is.null(data$dummy_intensity)) {
    stop(
      "`dummy_intensity` is required for the logistic objective.",
      call. = FALSE
    )
  }

  objective <- make_catboost_logistic_objective(
    dummy_intensity = data$dummy_intensity
  )

  .fit_gbpp_catboost(
    data = data,
    objective = objective,
    params = params,
    nrounds = nrounds
  )
}


# ------------------------------------------------------------
# Weighted Logistic
# ------------------------------------------------------------

fit_gbpp_catboost_weighted_logistic <- function(
    data,
    params = list(),
    nrounds = 100L) {

  if (is.null(data$dummy_intensity)) {
    stop(
      "`dummy_intensity` is required for the weighted logistic objective.",
      call. = FALSE
    )
  }

  if (is.null(data$statistical_weight)) {
    stop(
      "`statistical_weight` is required for the weighted logistic objective.",
      call. = FALSE
    )
  }

  objective <- make_catboost_weighted_logistic_objective(
    dummy_intensity = data$dummy_intensity,
    statistical_weight = data$statistical_weight
  )

  .fit_gbpp_catboost(
    data = data,
    objective = objective,
    params = params,
    nrounds = nrounds
  )
}


# ------------------------------------------------------------
# Iterative Poisson
# ------------------------------------------------------------

fit_gbpp_catboost_iterative_poisson <- function(
    data,
    params = list(),
    nrounds = 100L) {

  if (is.null(data$interaction_integral)) {
    stop(
      "`interaction_integral` is required for the iterative Poisson objective.",
      call. = FALSE
    )
  }

  objective <- make_catboost_iterative_poisson_objective(
    quadrature_weight = data$quadrature_weight,
    interaction_integral = data$interaction_integral
  )

  .fit_gbpp_catboost(
    data = data,
    objective = objective,
    params = params,
    nrounds = nrounds
  )
}


# ------------------------------------------------------------
# Iterative Logistic
# ------------------------------------------------------------

fit_gbpp_catboost_iterative_logistic <- function(
    data,
    params = list(),
    nrounds = 100L) {

  if (is.null(data$dummy_intensity)) {
    stop(
      "`dummy_intensity` is required for the iterative logistic objective.",
      call. = FALSE
    )
  }

  if (is.null(data$interaction_integral)) {
    stop(
      "`interaction_integral` is required for the iterative logistic objective.",
      call. = FALSE
    )
  }

  objective <- make_catboost_iterative_logistic_objective(
    dummy_intensity = data$dummy_intensity,
    interaction_integral = data$interaction_integral
  )

  .fit_gbpp_catboost(
    data = data,
    objective = objective,
    params = params,
    nrounds = nrounds
  )
}


# ============================================================
# PREDICTION
# ============================================================

#' @export
predict.gbpp_catboost <- function(
    object,
    newdata,
    ...) {

  if (!inherits(object, "gbpp_catboost")) {
    stop(
      "`object` must be a `gbpp_catboost` model.",
      call. = FALSE
    )
  }

  if (is.data.frame(newdata)) {
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
      "`newdata` must contain only finite values.",
      call. = FALSE
    )
  }

  prediction <- object$predict(
    data = newdata,
    prediction_type = "RawFormulaVal",
    # ...
  )

  prediction <- reticulate::py_to_r(prediction)

  as.numeric(prediction)
}
