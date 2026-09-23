# ============================================================
# GBPP - CatBoost Backend Engine
# ============================================================

.gbpp_catboost_module <- local({
  module <- NULL

  function() {
    if (!is.null(module)) {
      return(module)
    }

    configure_gbpp_python()

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

.gbpp_catboost <- local({
  module <- NULL

  function() {
    if (is.null(module)) {
      configure_gbpp_python()
      module <- reticulate::import(
        "catboost",
        delay_load = FALSE
      )
    }

    module
  }
})

# ============================================================
# Objective factories
# ============================================================

#' @keywords internal
make_catboost_poisson_objective <- function(quadrature_weight) {
  module <- .gbpp_catboost_module()
  module$PoissonObjective(quadrature_weight = quadrature_weight)
}

#' @keywords internal
make_catboost_weighted_poisson_objective <- function(quadrature_weight, statistical_weight) {
  module <- .gbpp_catboost_module()
  module$WeightedPoissonObjective(
    quadrature_weight = quadrature_weight,
    statistical_weight = statistical_weight
  )
}

#' @keywords internal
make_catboost_logistic_objective <- function(dummy_intensity) {
  module <- .gbpp_catboost_module()
  module$LogisticObjective(dummy_intensity = dummy_intensity)
}

#' @keywords internal
make_catboost_weighted_logistic_objective <- function(dummy_intensity, statistical_weight) {
  module <- .gbpp_catboost_module()
  module$WeightedLogisticObjective(
    dummy_intensity = dummy_intensity,
    statistical_weight = statistical_weight
  )
}

#' @keywords internal
make_catboost_iterative_poisson_objective <- function(quadrature_weight, interaction_integral) {
  module <- .gbpp_catboost_module()
  module$IterativePoissonObjective(
    quadrature_weight = quadrature_weight,
    interaction_integral = interaction_integral
  )
}

#' @keywords internal
make_catboost_iterative_logistic_objective <- function(dummy_intensity, interaction_integral) {
  module <- .gbpp_catboost_module()
  module$IterativeLogisticObjective(
    dummy_intensity = dummy_intensity,
    interaction_integral = interaction_integral
  )
}

# ============================================================
# Internal Engine Fitting Function
# ============================================================

.gbpp_fit_catboost <- function(
    data,
    objective,
    params,
    n_trees,
    baseline = NULL,
    ...
) {
  validate_gbpp_data(data, objective = objective)
  configure_gbpp_python()

  catboost <- .gbpp_catboost()
  X <- data$X

  # Handle data frame vs matrix and categorical columns
  if (is.matrix(X)) {
    if (!is.numeric(X)) {
      stop("`data$X` matrix must be numeric.", call. = FALSE)
    }
    if (any(!is.finite(X))) {
      stop("`data$X` must contain only finite values.", call. = FALSE)
    }
  } else if (is.data.frame(X)) {
    categorical_cols <- which(
      vapply(X, function(z) is.factor(z) || is.character(z), logical(1))
    )
    numeric_cols <- which(vapply(X, is.numeric, logical(1)))

    if (length(numeric_cols) > 0L) {
      numeric_values <- X[, numeric_cols, drop = FALSE]
      if (any(!is.finite(as.matrix(numeric_values)))) {
        stop("Numeric columns in `data$X` must contain only finite values.", call. = FALSE)
      }
    }

    if (length(categorical_cols) > 0L) {
      for (j in categorical_cols) {
        if (anyNA(X[[j]])) {
          stop("Categorical columns in `data$X` must not contain missing values.", call. = FALSE)
        }
        if (is.factor(X[[j]])) {
          X[[j]] <- as.character(X[[j]])
        }
      }

      if (!("cat_features" %in% names(params))) {
        params$cat_features <- reticulate::r_to_py(
          as.list(unname(categorical_cols - 1L))
        )
      }
    }
  }

  if (is.null(baseline)) {
    if (objective %in% c("poisson", "weighted_poisson")) {
      lambda0 <- sum(data$y) / sum(data$quadrature_weight)
      if (lambda0 <= 0 || !is.finite(lambda0)) {
        stop("Initial Poisson intensity must be positive and finite.", call. = FALSE)
      }
      baseline <- log(lambda0)
    } else {
      baseline <- 0.0
    }
  }

  py_objective <- switch(
    objective,
    poisson = make_catboost_poisson_objective(data$quadrature_weight),
    weighted_poisson = make_catboost_weighted_poisson_objective(data$quadrature_weight, data$statistical_weight),
    logistic = make_catboost_logistic_objective(data$dummy_intensity),
    weighted_logistic = make_catboost_weighted_logistic_objective(data$dummy_intensity, data$statistical_weight),
    iterative_poisson = make_catboost_iterative_poisson_objective(data$quadrature_weight, data$interaction_integral),
    iterative_logistic = make_catboost_iterative_logistic_objective(data$dummy_intensity, data$interaction_integral)
  )

  params$loss_function <- py_objective
  params$iterations <- as.integer(n_trees)

  raw_model <- do.call(catboost$CatBoostRegressor, params)

  fit_args <- list(
    X = X,
    y = data$y
  )

  if (!is.null(baseline) && baseline != 0.0) {
    fit_args$baseline <- rep(as.numeric(baseline), nrow(X))
  }

  do.call(raw_model$fit, fit_args)

  attr(raw_model, "engine") <- "catboost"
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
  class(raw_model) <- c("gbpp_catboost", "gbpp_model", class(raw_model))
  raw_model
}
