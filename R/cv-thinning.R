#' Independent Stochastic Thinning Cross-Validation
#'
#' Evaluates spatial point process gradient boosted models using independent stochastic thinning.
#' By independently dividing the observed events into K folds and retaining the full spatial
#' quadrature dummy points in all folds, this method produces mathematically exact out-of-sample
#' sub-processes without corrupting spatial integrals.
#'
#' @param data A `gbpp_data` object.
#' @param engine Character string specifying the engine: `"xgboost"`, `"lightgbm"`, or `"catboost"`.
#' @param params List of engine-specific hyperparameters.
#' @param nrounds Number of boosting iterations.
#' @param folds Number of independent thinning folds (K).
#' @param ... Additional arguments passed to the engine fitting function.
#'
#' @return An object of class `gbpp_cv` containing `metrics` and `folds`.
#' @export
cv_gbpp <- function(data, engine = c("xgboost", "lightgbm", "catboost"), params, nrounds, folds = 5L, ...) {
  if (!inherits(data, "gbpp_data")) {
    stop("`data` must be a gbpp_data object.", call. = FALSE)
  }
  
  engine <- match.arg(engine)
  folds <- as.integer(folds)
  
  if (folds < 2L) {
    stop("`folds` must be at least 2.", call. = FALSE)
  }
  
  idx_events <- which(data$y == 1)
  idx_dummy <- which(data$y == 0)
  
  # Randomly assign events to K folds
  set.seed(42) # For reproducibility in parallel runs if needed, or let user set seed
  fold_ids <- sample(rep_len(seq_len(folds), length(idx_events)))
  
  fit_fn <- match.fun(paste0("fit_gbpp_", engine))
  
  res_folds <- vector("list", folds)
  ll_res <- numeric(folds)
  dev_res <- numeric(folds)
  
  for (k in seq_len(folds)) {
    test_events <- idx_events[fold_ids == k]
    train_events <- idx_events[fold_ids != k]
    
    idx_train <- c(train_events, idx_dummy)
    idx_test <- c(test_events, idx_dummy)
    
    # Construct train dataset
    data_train <- new_gbpp_data(
      X = if (is.data.frame(data$X)) data$X[idx_train, , drop = FALSE] else data$X[idx_train, , drop = FALSE],
      y = data$y[idx_train],
      locations = data$locations[idx_train, , drop = FALSE],
      quadrature_weight = data$quadrature_weight[idx_train],
      dummy_intensity = if (!is.null(data$dummy_intensity)) data$dummy_intensity[idx_train] else NULL,
      statistical_weight = if (!is.null(data$statistical_weight)) data$statistical_weight[idx_train] else NULL,
      interaction_integral = if (!is.null(data$interaction_integral)) data$interaction_integral[idx_train] else NULL,
      window = data$window,
      type = data$type,
      metadata = data$metadata
    )
    
    # Construct test dataset
    data_test <- new_gbpp_data(
      X = if (is.data.frame(data$X)) data$X[idx_test, , drop = FALSE] else data$X[idx_test, , drop = FALSE],
      y = data$y[idx_test],
      locations = data$locations[idx_test, , drop = FALSE],
      quadrature_weight = data$quadrature_weight[idx_test],
      dummy_intensity = if (!is.null(data$dummy_intensity)) data$dummy_intensity[idx_test] else NULL,
      statistical_weight = if (!is.null(data$statistical_weight)) data$statistical_weight[idx_test] else NULL,
      interaction_integral = if (!is.null(data$interaction_integral)) data$interaction_integral[idx_test] else NULL,
      window = data$window,
      type = data$type,
      metadata = data$metadata
    )
    
    # Train model on the (K-1)/K sub-process
    model <- fit_fn(data_train, params = params, nrounds = nrounds, ...)
    
    # Adjust the learned baseline to predict the original full intensity
    p_train <- (folds - 1) / folds
    attr(model, "baseline") <- attr(model, "baseline") - log(p_train)
    
    # Predict original full intensity on test locations
    pred_full <- predict(model, newdata = data_test$X, type = "intensity")
    
    p_test <- 1 / folds
    ll <- metric_poisson_loglik(pred_full, data_test$y, data_test$quadrature_weight, p_scale = p_test)
    dev <- metric_poisson_deviance(pred_full, data_test$y, data_test$quadrature_weight, p_scale = p_test)
    
    ll_res[k] <- ll
    dev_res[k] <- dev
    
    res_folds[[k]] <- list(
      predictions = pred_full,
      y = data_test$y,
      w = data_test$quadrature_weight,
      loglik = ll,
      deviance = dev
    )
  }
  
  metrics <- list(
    loglik_mean = mean(ll_res),
    loglik_sd = sd(ll_res),
    deviance_mean = mean(dev_res),
    deviance_sd = sd(dev_res)
  )
  
  structure(
    list(
      metrics = metrics,
      folds = res_folds
    ),
    class = "gbpp_cv"
  )
}
