test_that("GBPP Python environment can be configured", {

  expect_no_error(
    configure_gbpp_python()
  )

  expect_no_error(
    reticulate::py_config()
  )

  expect_true(
    reticulate::py_available()
  )
})


test_that("GBPP Python environment provides required modelling packages", {

  configure_gbpp_python()

  expect_no_error(
    reticulate::import("xgboost")
  )

  expect_no_error(
    reticulate::import("lightgbm")
  )

  expect_no_error(
    reticulate::import("catboost")
  )
})
