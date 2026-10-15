test_that("fit_gbpp fits across all engines with unified API", {
  skip_on_cran()
  skip_if_not(reticulate::py_available(initialize = TRUE), message = "Python is not available.")
  configure_gbpp_python()

  data <- make_small_gbpp_data()

  # Fit XGBoost
  m_xgb <- fit_gbpp(
    data = data,
    engine = "xgboost",
    objective = "poisson",
    params = list(max_depth = 2L, learning_rate = 0.1),
    n_trees = 5L
  )
  expect_s3_class(m_xgb, "gbpp_model")
  expect_s3_class(m_xgb, "gbpp_xgboost")
  expect_equal(attr(m_xgb, "engine"), "xgboost")
  expect_equal(attr(m_xgb, "objective"), "poisson")

  # Fit LightGBM
  m_lgb <- fit_gbpp(
    data = data,
    engine = "lightgbm",
    objective = "poisson",
    params = list(max_depth = 2L, learning_rate = 0.1),
    n_trees = 5L
  )
  expect_s3_class(m_lgb, "gbpp_model")
  expect_s3_class(m_lgb, "gbpp_lightgbm")
  expect_equal(attr(m_lgb, "engine"), "lightgbm")
  expect_equal(attr(m_lgb, "objective"), "poisson")

  # Fit CatBoost
  m_cat <- fit_gbpp(
    data = data,
    engine = "catboost",
    objective = "poisson",
    params = list(depth = 2L, learning_rate = 0.1),
    n_trees = 5L
  )
  expect_s3_class(m_cat, "gbpp_model")
  expect_s3_class(m_cat, "gbpp_catboost")
  expect_equal(attr(m_cat, "engine"), "catboost")
  expect_equal(attr(m_cat, "objective"), "poisson")
})

test_that("predict.gbpp_model respects type contract: intensity, link, and raw", {
  skip_on_cran()
  skip_if_not(reticulate::py_available(initialize = TRUE), message = "Python is not available.")
  configure_gbpp_python()

  data <- make_small_gbpp_data()

  m_xgb <- fit_gbpp(
    data = data,
    engine = "xgboost",
    objective = "poisson",
    n_trees = 5L
  )

  p_intensity <- predict(m_xgb, newdata = data$X, type = "intensity")
  p_link <- predict(m_xgb, newdata = data$X, type = "link")
  p_raw <- predict(m_xgb, newdata = data$X, type = "raw")

  expect_type(p_intensity, "double")
  expect_type(p_link, "double")
  expect_type(p_raw, "double")
  expect_length(p_intensity, nrow(data$X))

  # Mathematical consistency: intensity = exp(link)
  expect_equal(p_intensity, exp(p_link), tolerance = 1e-6)

  # Baseline offset inclusion: link = baseline + raw
  baseline <- attr(m_xgb, "baseline")
  if (!is.null(baseline)) {
    expect_equal(p_link, p_raw + baseline, tolerance = 1e-6)
  }
})

test_that("fit_gbpp errors on invalid inputs", {
  data <- make_small_gbpp_data()

  expect_error(
    fit_gbpp(data = data, engine = "invalid_engine")
  )

  expect_error(
    fit_gbpp(data = list(not = "data"), engine = "xgboost")
  )
})
