test_that("XGBoost DMatrix handles one feature", {
  skip_if_not(
    reticulate::py_available(),
    message = "Python is not available."
  )

  X <- data.frame(
    numeric = c(1, 2, 3, 4, 5)
  )

  y <- c(0, 1, 0, 1, 0)

  dtrain <- gbpp:::.gbpp_xgboost_dmatrix(
    X = X,
    y = y
  )

  expect_equal(dtrain$num_col(), 1L)
  expect_identical(
    reticulate::py_to_r(dtrain$feature_names),
    "numeric"
  )
})


test_that("XGBoost DMatrix handles multiple features", {
  skip_if_not(
    reticulate::py_available(),
    message = "Python is not available."
  )

  X <- data.frame(
    x1 = c(1, 2, 3, 4, 5),
    x2 = c(5, 4, 3, 2, 1)
  )

  y <- c(0, 1, 0, 1, 0)

  dtrain <- gbpp:::.gbpp_xgboost_dmatrix(
    X = X,
    y = y
  )

  expect_equal(dtrain$num_col(), 2L)
  expect_identical(
    reticulate::py_to_r(dtrain$feature_names),
    c("x1", "x2")
  )
})
