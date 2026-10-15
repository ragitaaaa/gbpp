test_that("package metadata is valid", {
  ver <- utils::packageVersion("gbpp")
  expect_true(is.package_version(ver))
})
