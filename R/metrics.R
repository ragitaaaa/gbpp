#' Spatial Poisson Log-Likelihood
#'
#' Calculates the Poisson point process log-likelihood using the Berman-Turner quadrature approximation.
#'
#' @param pred_intensity Numeric vector of predicted intensity values.
#' @param y Integer vector indicating events (1) and dummy points (0).
#' @param w Numeric vector of quadrature weights.
#' @param p_scale Numeric scalar representing the thinning retention probability for test sets. Default is 1.
#'
#' @return The spatial Poisson log-likelihood.
#' @export
metric_poisson_loglik <- function(pred_intensity, y, w, p_scale = 1) {
  if (p_scale != 1) {
    pred_intensity <- pred_intensity * p_scale
  }
  
  # Protect against log(0)
  pred_intensity <- pmax(pred_intensity, 1e-15)
  
  ll_events <- sum(log(pred_intensity[y == 1]))
  ll_integral <- sum(w * pred_intensity)
  
  ll_events - ll_integral
}

#' Spatial Poisson Deviance
#'
#' Calculates the Poisson point process deviance using the Berman-Turner quadrature approximation.
#'
#' @param pred_intensity Numeric vector of predicted intensity values.
#' @param y Integer vector indicating events (1) and dummy points (0).
#' @param w Numeric vector of quadrature weights.
#' @param p_scale Numeric scalar representing the thinning retention probability for test sets. Default is 1.
#'
#' @return The spatial Poisson deviance.
#' @export
metric_poisson_deviance <- function(pred_intensity, y, w, p_scale = 1) {
  if (p_scale != 1) {
    pred_intensity <- pred_intensity * p_scale
  }
  
  pred_intensity <- pmax(pred_intensity, 1e-15)
  
  # Protect against exact 0 weights for events to prevent infinite deviance
  w_safe <- w
  w_safe[y == 1] <- pmax(w_safe[y == 1], 1e-15)
  
  mu <- w_safe * pred_intensity
  
  # GLM Poisson deviance: 2 * sum(y * log(y / mu) - (y - mu))
  # When y = 0, y * log(y / mu) = 0
  # When y = 1, y * log(y / mu) = -log(mu)
  
  dev_events <- sum(-log(mu[y == 1]))
  dev_all <- sum(y - mu)
  
  2 * (dev_events - dev_all)
}
