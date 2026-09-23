test_that(".gbpp_normalize_params standardizes parameters across engines", {
  # XGBoost mapping
  xgb_res <- gbpp:::.gbpp_normalize_params(
    engine = "xgboost",
    n_trees = 50L,
    learning_rate = 0.03,
    max_depth = 4L,
    subsample = 0.8,
    colsample_bytree = 0.7,
    random_seed = 42L,
    verbose = FALSE
  )
  expect_equal(xgb_res$n_trees, 50L)
  expect_equal(xgb_res$params$eta, 0.03)
  expect_equal(xgb_res$params$max_depth, 4L)
  expect_equal(xgb_res$params$subsample, 0.8)
  expect_equal(xgb_res$params$colsample_bytree, 0.7)
  expect_equal(xgb_res$params$seed, 42L)
  expect_equal(xgb_res$params$verbosity, 0L)

  # LightGBM mapping
  lgb_res <- gbpp:::.gbpp_normalize_params(
    engine = "lightgbm",
    n_trees = 75L,
    learning_rate = 0.03,
    max_depth = 4L,
    subsample = 0.8,
    colsample_bytree = 0.7,
    random_seed = 42L,
    verbose = FALSE
  )
  expect_equal(lgb_res$n_trees, 75L)
  expect_equal(lgb_res$params$learning_rate, 0.03)
  expect_equal(lgb_res$params$max_depth, 4L)
  expect_equal(lgb_res$params$bagging_fraction, 0.8)
  expect_equal(lgb_res$params$feature_fraction, 0.7)
  expect_equal(lgb_res$params$seed, 42L)
  expect_equal(lgb_res$params$verbosity, -1L)

  # CatBoost mapping
  cat_res <- gbpp:::.gbpp_normalize_params(
    engine = "catboost",
    n_trees = 100L,
    learning_rate = 0.03,
    max_depth = 4L,
    subsample = 0.8,
    colsample_bytree = 0.7,
    random_seed = 42L,
    verbose = FALSE
  )
  expect_equal(cat_res$n_trees, 100L)
  expect_equal(cat_res$params$learning_rate, 0.03)
  expect_equal(cat_res$params$depth, 4L)
  expect_equal(cat_res$params$subsample, 0.8)
  expect_equal(cat_res$params$rsm, 0.7)
  expect_equal(cat_res$params$random_seed, 42L)
  expect_equal(cat_res$params$verbose, FALSE)
})

test_that(".gbpp_normalize_params preserves engine-specific user parameters", {
  res <- gbpp:::.gbpp_normalize_params(
    engine = "xgboost",
    n_trees = 10L,
    user_params = list(gamma = 0.5, min_child_weight = 2.0)
  )
  expect_equal(res$params$gamma, 0.5)
  expect_equal(res$params$min_child_weight, 2.0)
})

test_that(".gbpp_normalize_params validates arguments properly", {
  expect_error(gbpp:::.gbpp_normalize_params("xgboost", n_trees = -1L), "n_trees")
  expect_error(gbpp:::.gbpp_normalize_params("xgboost", learning_rate = -0.1), "learning_rate")
  expect_error(gbpp:::.gbpp_normalize_params("xgboost", max_depth = 0L), "max_depth")
  expect_error(gbpp:::.gbpp_normalize_params("xgboost", subsample = 1.5), "subsample")
  expect_error(gbpp:::.gbpp_normalize_params("xgboost", colsample_bytree = 0), "colsample_bytree")
})
