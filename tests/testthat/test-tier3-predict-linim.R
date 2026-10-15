test_that("predict(type = 'linim') works correctly", {
  skip_on_cran()
  skip_if_not(reticulate::py_available(initialize = TRUE), "Python not available")
  
  configure_gbpp_python()
  
  L <- spatstat.data::simplenet
  pts <- spatstat.linnet::rpoislpp(5, L)
  
  df_X <- data.frame(
    x = as.numeric(pts$data$x),
    y = as.numeric(pts$data$y),
    v1 = runif(spatstat.geom::npoints(pts))
  )
  
  data <- new_gbpp_data(
    X = df_X,
    y = rep(1, spatstat.geom::npoints(pts)),
    locations = df_X[, c("x", "y")],
    quadrature_weight = rep(1, spatstat.geom::npoints(pts))
  )

  model <- fit_gbpp_xgboost(data, params = list(eta = 0.1, max_depth = 2L), nrounds = 2L)

  cov1 <- spatstat.linnet::as.linim(spatstat.linnet::linfun(function(x,y,seg,tp) x+y, L))
  
  expect_error(
    predict(model, newdata = L, type = "linim"),
    "Missing required covariates in `newdata`: v1"
  )

  pred_cov <- predict(model, newdata = list(v1 = cov1), type = "linim")
  expect_s3_class(pred_cov, "linim")
  expect_true(is.numeric(pred_cov$v))
  expect_true(all(is.finite(pred_cov$v[!is.na(pred_cov$v)])))
  
  # Test with eps param
  pred_cov2 <- predict(model, newdata = list(v1 = cov1), type = "linim", eps = 0.5)
  expect_s3_class(pred_cov2, "linim")
})
