test_that("predict(type = 'im') works correctly", {
  skip_on_cran()
  skip_if_not(reticulate::py_available(initialize = TRUE), "Python not available")
  
  configure_gbpp_python()
  
  data <- new_gbpp_data(
    X = data.frame(
      x = c(0.5, 1.5, 2.5),
      y = c(0.5, 1.5, 2.5),
      v1 = c(1, 3, 5)
    ),
    y = c(1, 0, 0),
    locations = data.frame(
      x = c(0.5, 1.5, 2.5),
      y = c(0.5, 1.5, 2.5)
    ),
    quadrature_weight = c(1, 1, 1)
  )

  model <- fit_gbpp_xgboost(data, params = list(eta = 0.1, max_depth = 2L), nrounds = 2L)

  win <- spatstat.geom::owin(c(0, 3), c(0, 3))
  cov1 <- spatstat.geom::as.im(function(x, y) x + y, W = win, eps = 1)
  
  expect_error(
    predict(model, newdata = win, type = "im"),
    "Missing required covariates in `newdata`: v1"
  )

  pred_cov <- predict(model, newdata = list(v1 = cov1), type = "im")
  expect_s3_class(pred_cov, "im")
  expect_equal(pred_cov$dim, c(3, 3))
  expect_true(all(is.finite(pred_cov$v[!is.na(pred_cov$v)])))
  
  # Test with eps param
  pred_cov2 <- predict(model, newdata = list(v1 = cov1), type = "im", eps = 0.5)
  expect_s3_class(pred_cov2, "im")
  expect_equal(pred_cov2$dim, c(6, 6))
})
