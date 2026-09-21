#' Prepare a GBPP planar point-process data object
#'
#' Construct Berman-Turner quadrature data for a planar point process and
#' evaluate spatial covariates at the quadrature locations.
#'
#' @param point_pattern An `ppp` object containing the observed points.
#' @param covariates A named list of `im` covariates whose spatial windows
#'   cover the observation window of `point_pattern`.
#' @param nd Number of dummy quadrature points, as used by
#'   [spatstat.geom::quadscheme.logi()].
#'
#' @return A `gbpp_data` object with `type = "planar"`.
#' @export
prepare_gbpp <- function(
    point_pattern,
    covariates = list(),
    nd = 1000L
) {
  if (!inherits(point_pattern, "ppp")) {
    stop("`point_pattern` must be a `ppp` object.", call. = FALSE)
  }

  if (!is.list(covariates)) {
    stop("`covariates` must be a list of `im` objects.", call. = FALSE)
  }

  if (length(covariates) > 0L) {
    if (is.null(names(covariates)) ||
        anyNA(names(covariates)) ||
        any(names(covariates) == "")) {
      stop("All planar covariates must be named.", call. = FALSE)
    }

    if (anyDuplicated(names(covariates))) {
      stop(
        "Planar covariate names must be unique.",
        call. = FALSE
      )
    }

    invalid <- !vapply(
      covariates,
      inherits,
      logical(1),
      what = "im"
    )

    if (any(invalid)) {
      stop(
        "All planar covariates must be `im` objects.",
        call. = FALSE
      )
    }

    covariate_covers_window <- vapply(
      covariates,
      function(z) {
        spatstat.geom::is.subset.owin(
          point_pattern$window,
          spatstat.geom::Window(z)
        )
      },
      logical(1)
    )

    if (any(!covariate_covers_window)) {
      stop(
        paste0(
          "All planar covariates must cover the observation window ",
          "of `point_pattern`."
        ),
        call. = FALSE
      )
    }
  }

  if (length(nd) != 1L ||
      !is.numeric(nd) ||
      !is.finite(nd) ||
      nd <= 0 ||
      nd != as.integer(nd)) {
    stop(
      "`nd` must be one positive integer.",
      call. = FALSE
    )
  }

  nd <- as.integer(nd)

  Q <- spatstat.geom::quadscheme.logi(
    point_pattern,
    dummytype = "grid",
    nd = nd
  )

  U <- spatstat.geom::union.quad(Q)
  y <- as.integer(spatstat.geom::is.data(Q))

  X <- data.frame(
    row.names = seq_len(length(y)),
    check.names = FALSE
  )

  if (length(covariates) > 0L) {
    X <- as.data.frame(
      lapply(covariates, function(z) {
        z[U]
      }),
      check.names = FALSE
    )

    names(X) <- names(covariates)
  }

  if (nrow(X) != length(y)) {
    stop(
      "Planar covariate evaluation produced an invalid number of rows.",
      call. = FALSE
    )
  }

  if (anyNA(X)) {
    stop(
      "Planar covariates contain missing values at quadrature locations.",
      call. = FALSE
    )
  }

  data <- prepare_pp_data(
    Q = Q,
    X = X,
    objective = "logistic",
    type = "planar"
  )

  data$window <- point_pattern$window

  data$metadata <- c(
    data$metadata,
    list(
      point_pattern = point_pattern,
      quadrature = Q,
      covariates = covariates,
      quadrature_method = "quadscheme.logi"
    )
  )

  data
}

#' Convert a spatstat quadrature scheme to a GBPP data object
#'
#' @param Q A quadrature scheme produced by `spatstat`.
#' @param X Covariate data corresponding to all quadrature points.
#' @param objective Objective used by the model.
#' @param type Spatial domain type: `"planar"` or `"network"`.
#' @param statistical_weight Optional statistical weights.
#'
#' @return An object of class `gbpp_data`.
#' @keywords internal
prepare_pp_data <- function(
    Q,
    X,
    objective = c(
      "poisson",
      "weighted_poisson",
      "logistic",
      "weighted_logistic"
    ),
    type = c("planar", "network"),
    statistical_weight = NULL
) {

  objective <- match.arg(objective)
  type <- match.arg(type)

  if (!is.data.frame(X) && !is.matrix(X)) {
    stop("`X` must be a data frame or matrix.", call. = FALSE)
  }

  y <- as.integer(spatstat.geom::is.data(Q))
  locations <- as.data.frame(spatstat.geom::coords(Q))
  quadrature_weight <- spatstat.geom::w.quad(Q)

  if (nrow(X) != length(y)) {
    stop(
      "`X` must have one row for each point in the quadrature scheme.",
      call. = FALSE
    )
  }

  if (length(quadrature_weight) != length(y)) {
    stop(
      "`quadrature_weight` must have one value for each point.",
      call. = FALSE
    )
  }

  if (objective %in% c("weighted_poisson", "weighted_logistic") &&
      is.null(statistical_weight)) {
    stop(
      "`statistical_weight` is required for weighted objectives.",
      call. = FALSE
    )
  }

  if (!is.null(statistical_weight) &&
      length(statistical_weight) != length(y)) {
    stop(
      "`statistical_weight` must have one value for each point.",
      call. = FALSE
    )
  }

  dummy_intensity <- NULL

  if (objective %in% c("logistic", "weighted_logistic")) {
    dummy_intensity <- extract_dummy_intensity(Q)
  }

  new_gbpp_data(
    X = X,
    y = y,
    locations = locations,
    quadrature_weight = quadrature_weight,
    dummy_intensity = dummy_intensity,
    statistical_weight = statistical_weight,
    type = type,
    metadata = list(
      objective = objective,
      source = "spatstat_quadrature"
    )
  )
}
