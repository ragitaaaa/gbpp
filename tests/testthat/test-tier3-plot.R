test_that("plot.gbpp_model works for planar im", {
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
  pts <- spatstat.geom::ppp(x = 1, y = 1, window = win)

  # Check basic plot
  pdf(file = NULL) # sink plots
  p1 <- plot(model, newdata = list(v1 = cov1), type = "intensity")
  expect_s3_class(p1, "im")

  p2 <- plot(model, newdata = list(v1 = cov1), type = "link", overlay_points = TRUE, pts = pts)
  expect_s3_class(p2, "im")
  
  # Ensure the link values are indeed log of intensity
  # (Since spatial_obj is invisible returned)
  expect_equal(p2$v, log(p1$v), tolerance = 1e-6)
  
  expect_error(plot(model), "must be supplied")
  
  dev.off()
})
