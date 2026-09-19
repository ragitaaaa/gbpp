#' Calculate gradient and Hessian for a GBPP objective
#'
#' @param data A `gbpp_data` object.
#' @param prediction Numeric vector of current predicted log-intensities.
#' @param objective Objective to use. One of `"poisson"`, `"weighted_poisson"`, `"logistic"`, or `"weighted_logistic"`.
#' @return A list containing numeric vectors `gradient` and `hessian`.
#' @keywords internal
gbpp_objective <- function(
    data,
    prediction,
    objective = c(
      "poisson",
      "weighted_poisson",
      "logistic",
      "weighted_logistic"
    )
) {

  objective <- match.arg(objective)

  validate_gbpp_data(
    data,
    objective = objective
  )

  if (!is.numeric(prediction)) {
    stop(
      "`prediction` must be a numeric vector.",
      call. = FALSE
    )
  }

  if (length(prediction) != length(data$y)) {
    stop(
      "`prediction` must have one value for each observation.",
      call. = FALSE
    )
  }

  if (any(!is.finite(prediction))) {
    stop(
      "`prediction` must contain only finite values.",
      call. = FALSE
    )
  }

  switch(
    objective,

    poisson = {
      calculate_poisson_derivatives(
        data = data,
        prediction = prediction
      )
    },

    weighted_poisson = {
      calculate_weighted_poisson_derivatives(
        data = data,
        prediction = prediction
      )
    },

    logistic = {
      calculate_logistic_derivatives(
        data = data,
        prediction = prediction
      )
    },

    weighted_logistic = {
      calculate_weighted_logistic_derivatives(
        data = data,
        prediction = prediction
      )
    }
  )
}

#' @keywords internal
calculate_poisson_derivatives <- function(data, prediction) {
  phi <- pmin(pmax(prediction, -50), 50)
  lambda <- exp(phi)
  gradient <- data$quadrature_weight * lambda - data$y
  hessian <- data$quadrature_weight * lambda

  list(
    gradient = gradient,
    hessian = hessian
  )
}

#' @keywords internal
calculate_weighted_poisson_derivatives <- function(data, prediction) {
  phi <- pmin(pmax(prediction, -50), 50)
  lambda <- exp(phi)

  gradient <- (
    data$quadrature_weight *
      data$statistical_weight *
      lambda
  ) - (
    data$y *
      data$statistical_weight
  )

  hessian <- (
    data$quadrature_weight *
      data$statistical_weight *
      lambda
  )

  list(
    gradient = gradient,
    hessian = hessian
  )
}

#' @keywords internal
calculate_logistic_derivatives <- function(data, prediction) {
  delta <- data$dummy_intensity
  pi <- stats::plogis(
    prediction - log(delta)
  )
  gradient <- pi - data$y
  hessian <- pi * (1 - pi)

  list(
    gradient = gradient,
    hessian = hessian
  )
}

#' @keywords internal
calculate_weighted_logistic_derivatives <- function(data, prediction) {

  delta <- data$dummy_intensity

  pi <- stats::plogis(
    prediction - log(delta)
  )

  gradient <- (
    data$statistical_weight *
      (pi - data$y)
  )

  hessian <- (
    data$statistical_weight *
      pi *
      (1 - pi)
  )

  list(
    gradient = gradient,
    hessian = hessian
  )
}
