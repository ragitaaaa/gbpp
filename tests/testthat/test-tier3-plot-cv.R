test_that("plot.gbpp_cv executes without error", {
  skip_if_not_installed("spatstat.geom")
  
  set.seed(42)
  X <- data.frame(x1 = runif(100), x2 = runif(100))
  y <- c(rep(1, 40), rep(0, 60))
  locations <- data.frame(x = runif(100), y = runif(100))
  w <- c(rep(0, 40), runif(60))
  
  data <- new_gbpp_data(X = X, y = y, locations = locations, quadrature_weight = w, type = "planar", window = spatstat.geom::square(1))
  
  res <- cv_gbpp(data, engine = "xgboost", params = list(eta = 0.1, max_depth = 2L), nrounds = 5L, folds = 3L)
  
  # Just test that plotting doesn't throw errors
  expect_silent({
    # Send plot to a null device to avoid popping up windows during testing
    pdf(NULL)
    on.exit(dev.off())
    plot(res, separate = TRUE)
    plot(res, separate = FALSE)
  })
})
