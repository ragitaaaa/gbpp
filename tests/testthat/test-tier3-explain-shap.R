test_that(".gbpp_shap works across all backends", {
  skip_if_not(reticulate::py_available(initialize = TRUE), "Python not available")
  
  configure_gbpp_python()
  
  data <- new_gbpp_data(
    X = data.frame(
      x = c(0.5, 1.5, 2.5),
      y = c(0.5, 1.5, 2.5),
      v1 = c(1, 3, 5),
      v2 = c(2, 4, 6)
    ),
    y = c(1, 0, 0),
    locations = data.frame(
      x = c(0.5, 1.5, 2.5),
      y = c(0.5, 1.5, 2.5)
    ),
    quadrature_weight = c(1, 1, 1)
  )

  engines <- c("xgboost", "lightgbm", "catboost")
  
  for (engine in engines) {
    if (engine == "xgboost") {
      model <- fit_gbpp_xgboost(data, params = list(eta = 0.1, max_depth = 2L), nrounds = 2L)
    } else if (engine == "lightgbm") {
      model <- fit_gbpp_lightgbm(data, params = list(learning_rate = 0.1, max_depth = 2L, min_data_in_bin = 1L, min_data_in_leaf = 1L), nrounds = 2L)
    } else if (engine == "catboost") {
      model <- fit_gbpp_catboost(data, params = list(learning_rate = 0.1, depth = 2L), nrounds = 2L)
    }

    # Extract SHAP
    res <- .gbpp_shap(model, data$X)
    
    # Check structure
    expect_type(res, "list")
    expect_named(res, c("shap", "bias"))
    
    # Check bias is a vector
    expect_true(is.numeric(res$bias))
    expect_length(res$bias, 3)
    
    # Check SHAP is a matrix-like or dataframe with correct names
    expect_true(is.matrix(res$shap) || is.data.frame(res$shap))
    expect_equal(colnames(res$shap), c("x", "y", "v1", "v2"))
    expect_equal(nrow(res$shap), 3)
    expect_equal(ncol(res$shap), 4)
  }
})
