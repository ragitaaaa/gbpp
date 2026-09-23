test_that("cv_gbpp executes cleanly for xgboost", {
  skip_if_not_installed("spatstat.geom")
  
  set.seed(42)
  X <- data.frame(x1 = runif(100), x2 = runif(100))
  y <- c(rep(1, 40), rep(0, 60))
  locations <- data.frame(x = runif(100), y = runif(100))
  w <- c(rep(0, 40), runif(60))
  
  data <- new_gbpp_data(X = X, y = y, locations = locations, quadrature_weight = w, type = "planar")
  
  res <- cv_gbpp(data, engine = "xgboost", params = list(eta = 0.1, max_depth = 2L), nrounds = 5L, folds = 3L)
  
  expect_s3_class(res, "gbpp_cv")
  expect_true(is.numeric(res$metrics$loglik_mean))
  expect_true(is.numeric(res$metrics$deviance_mean))
  expect_equal(length(res$folds), 3L)
  
  # Check test proportions
  # Train events should be roughly 2/3, test events 1/3
  # And dummy points should be 60 in all folds
  expect_equal(sum(res$folds[[1]]$y == 0), 60L)
})
