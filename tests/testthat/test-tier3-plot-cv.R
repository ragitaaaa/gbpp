test_that("plot.gbpp_cv executes without error", {
  res_folds <- list(
    list(expected_events = 10.5, observed_events = 11, deviance = 20),
    list(expected_events = 14.2, observed_events = 14, deviance = 18),
    list(expected_events = 12.0, observed_events = 12, deviance = 19)
  )
  
  metrics <- list(
    loglik_mean = -10,
    loglik_sd = 1,
    deviance_mean = 19,
    deviance_sd = 1
  )
  
  mock_cv <- structure(
    list(metrics = metrics, folds = res_folds),
    class = "gbpp_cv"
  )
  
  # Ensure the plot runs cleanly
  pdf(file = NULL)
  expect_silent(plot(mock_cv))
  dev.off()
})
