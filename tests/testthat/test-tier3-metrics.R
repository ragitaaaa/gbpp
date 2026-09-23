test_that("metric_poisson_loglik calculates correctly without scaling", {
  set.seed(42)
  pred <- runif(10, 0.1, 1.0)
  y <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
  w <- c(0, 0, 0, 0.1, 0.2, 0.1, 0.1, 0.2, 0.2, 0.1)
  
  ll_expected <- sum(log(pred[y == 1])) - sum(w * pred)
  ll_actual <- metric_poisson_loglik(pred, y, w)
  
  expect_equal(ll_actual, ll_expected)
})

test_that("metric_poisson_loglik calculates correctly with p_scale", {
  set.seed(42)
  pred <- runif(10, 0.1, 1.0)
  y <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
  w <- c(0, 0, 0, 0.1, 0.2, 0.1, 0.1, 0.2, 0.2, 0.1)
  
  p <- 0.2
  ll_expected <- sum(log(pred[y == 1] * p)) - sum(w * pred * p)
  ll_actual <- metric_poisson_loglik(pred, y, w, p_scale = p)
  
  expect_equal(ll_actual, ll_expected)
})

test_that("metric_poisson_deviance calculates correctly", {
  set.seed(42)
  pred <- runif(10, 0.1, 1.0)
  y <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0)
  w <- c(0, 0, 0, 0.1, 0.2, 0.1, 0.1, 0.2, 0.2, 0.1)
  
  w_safe <- w
  w_safe[y == 1] <- pmax(w_safe[y == 1], 1e-15)
  mu <- w_safe * pred
  
  dev_expected <- 2 * (sum(-log(mu[y == 1])) - sum(y - mu))
  dev_actual <- metric_poisson_deviance(pred, y, w)
  
  expect_equal(dev_actual, dev_expected)
})
