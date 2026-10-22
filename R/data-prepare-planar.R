#' Prepare a GBPP planar point-process data object
#'
#' Construct quadrature data for a planar point process and
#' evaluate spatial covariates at the quadrature locations. Supports both
#' classical Berman-Turner quadrature (with cell counting weights for Poisson
#' objectives) and logistic quadrature schemes (with dummy-process intensity
#' for logistic objectives).
#'
#' @param point_pattern A `ppp` object containing the observed points.
#' @param covariates A named list of `im` covariates whose spatial windows
#'   cover the observation window of `point_pattern`. Numeric and factor/categorical
#'   covariates are supported.
#' @param nd Number of dummy quadrature points along each coordinate axis. Default is `1000L`.
#' @param quadrature_method Quadrature construction method: `"berman_turner"`
#'   (default, uses [spatstat.geom::quadscheme()] with counting weights for Poisson likelihood)
#'   or `"logistic"` (uses [spatstat.geom::quadscheme.logi()] for logistic likelihood).
#' @param dummytype Dummy point generation type for logistic quadrature: `"grid"` (default)
#'   or `"poisson"` (random dummy points according to a Poisson process).
#' @param objective Optional objective name (e.g. `"poisson"` or `"logistic"`). If supplied
#'   and `quadrature_method` is omitted, automatically aligns the quadrature scheme:
#'   `"logistic"` for logistic objectives and `"berman_turner"` for Poisson objectives.
#' @param ... Additional arguments passed to [spatstat.geom::quadscheme()] or
#'   [spatstat.geom::quadscheme.logi()].
#'
#' @return A `gbpp_data` object with `type = "planar"`.
#' @export
prepare_gbpp <- function(
    point_pattern,
    covariates = list(),
    nd = 1000L,
    quadrature_method = c("berman_turner", "logistic"),
    dummytype = c("grid", "poisson"),
    objective = NULL,
    ...
) {
  if (!inherits(point_pattern, "ppp")) {
    stop("`point_pattern` must be a `ppp` object.", call. = FALSE)
  }

  if (!is.list(covariates)) {
    stop("`covariates` must be a list of `im` objects.", call. = FALSE)
  }

  if (length(covariates) > 0L) {
    covariates <- lapply(covariates, function(z) {
      if (inherits(z, "imlist") && length(z) == 1L) z[[1L]] else z
    })

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

  if (missing(quadrature_method) && !is.null(objective)) {
    quadrature_method <- if (objective %in% c("logistic", "weighted_logistic", "iterative_logistic")) {
      "logistic"
    } else {
      "berman_turner"
    }
  } else {
    quadrature_method <- match.arg(quadrature_method)
  }

  dummytype <- match.arg(dummytype)

  if (quadrature_method == "berman_turner") {
    Q <- spatstat.geom::quadscheme(
      point_pattern,
      nd = nd,
      ...
    )
    U <- spatstat.geom::union.quad(Q)
    y <- as.integer(spatstat.geom::is.data(Q))
    v <- spatstat.geom::w.quad(Q)
    delta <- calculate_planar_dummy_intensity(
      y = y,
      quadrature_weight = point_pattern$window
    )
    dummy_intensity <- rep(delta, length(y))
  } else {
    Q <- spatstat.geom::quadscheme.logi(
      point_pattern,
      dummytype = dummytype,
      nd = nd,
      ...
    )
    U <- spatstat.geom::union.quad(Q)
    y <- as.integer(spatstat.geom::is.data(Q))
    v <- spatstat.geom::w.quad(Q)
    dummy_intensity <- extract_dummy_intensity(Q)
  }

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

  locations <- as.data.frame(spatstat.geom::coords(U))

  new_gbpp_data(
    X = X,
    y = y,
    locations = locations,
    quadrature_weight = v,
    dummy_intensity = dummy_intensity,
    window = point_pattern$window,
    type = "planar",
    metadata = list(
      point_pattern = point_pattern,
      quadrature = Q,
      covariates = covariates,
      quadrature_method = quadrature_method,
      dummytype = if (quadrature_method == "logistic") dummytype else "grid"
    )
  )
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
#' @export
prepare_pp_data <- function(
    Q,
    X,
    objective = c(
      "poisson",
      "weighted_poisson",
      "logistic",
      "weighted_logistic",
      "iterative_poisson",
      "iterative_logistic"
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

  if (objective %in% c("logistic", "weighted_logistic", "iterative_logistic")) {
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
