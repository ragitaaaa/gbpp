test_that("estimated network interaction can be attached to gbpp_data", {
  set.seed(123)

  fixture <- make_network_integration_fixture()

  data <- prepare_gbpp_network(
    point_pattern = fixture$point_pattern,
    covariates = fixture$covariates,
    nd = 20L
  )

  result <- attach_network_interaction_integral(
    data = data,
    point_pattern = fixture$point_pattern,
    rmax = 0.1,
    nr = 32L
  )

  expect_s3_class(result, "gbpp_data")
  expect_identical(result$type, "network")

  expect_equal(
    length(result$interaction_integral),
    nrow(result$X)
  )

  expect_true(
    all(is.finite(result$interaction_integral))
  )
})
