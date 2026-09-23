#' Prepare a GBPP network data object
#'
#' Construct Berman-Turner quadrature data for a spatial point process on a
#' linear network and evaluate linear-network covariates at the quadrature
#' locations.
#'
#' @param point_pattern An `lpp` object containing the observed points.
#' @param covariates A named list of `linim` or `linfun` covariates defined on
#'   the same linear network as `point_pattern`. Numeric and factor-valued
#'   covariates are supported.
#' @param nd Number of dummy quadrature points per network segment, as used by
#'   [spatstat.linnet::linequad()].
#'
#' @return A `gbpp_data` object with `type = "network"`.
#' @export
prepare_gbpp_network <- function(
    point_pattern,
    covariates = list(),
    nd = 1000L
) {
  if (!inherits(point_pattern, "lpp")) {
    stop("`point_pattern` must be an `lpp` object.", call. = FALSE)
  }

  if (!is.list(covariates)) {
    stop(
      "`covariates` must be a list of `linim` or `linfun` objects.",
      call. = FALSE
    )
  }

  if (length(covariates) > 0L) {
    if (is.null(names(covariates)) ||
        anyNA(names(covariates)) ||
        any(names(covariates) == "")) {
      stop("All network covariates must be named.", call. = FALSE)
    }

    if (anyDuplicated(names(covariates))) {
      stop(
        "Network covariate names must be unique.",
        call. = FALSE
      )
    }

    invalid <- !vapply(
      covariates,
      function(z) {
        inherits(z, "linim") || inherits(z, "linfun")
      },
      logical(1)
    )

    if (any(invalid)) {
      stop(
        "All network covariates must be `linim` or `linfun` objects.",
        call. = FALSE
      )
    }

    network <- spatstat.linnet::as.linnet(point_pattern)

    same_network <- vapply(
      covariates,
      function(z) {
        identical(
          spatstat.linnet::as.linnet(z),
          network
        )
      },
      logical(1)
    )

    if (any(!same_network)) {
      stop(
        paste0(
          "All network covariates must be defined on the same ",
          "linear network as `point_pattern`."
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

  Q <- spatstat.linnet::linequad(
    point_pattern,
    nd = nd
  )

  U <- spatstat.geom::union.quad(Q)
  y <- as.integer(spatstat.geom::is.data(Q))
  v <- spatstat.geom::w.quad(Q)
  dummy_intensity <- calculate_network_dummy_intensity(
    y = y,
    quadrature_weight = v
  )

  X <- data.frame(
    row.names = seq_len(length(y)),
    check.names = FALSE
  )

  if (length(covariates) > 0L) {
    X <- as.data.frame(
      lapply(covariates, function(z) {
        f <- as.function(z)
        f(U)
      }),
      check.names = FALSE
    )

    names(X) <- names(covariates)
  }

  if (nrow(X) != length(y)) {
    stop(
      "Network covariate evaluation produced an invalid number of rows.",
      call. = FALSE
    )
  }

  if (anyNA(X)) {
    stop(
      "Network covariates contain missing values at quadrature locations.",
      call. = FALSE
    )
  }

  new_gbpp_data(
    X = X,
    y = y,
    locations = U,
    quadrature_weight = v,
    dummy_intensity = rep(dummy_intensity, length(y)),
    window = point_pattern$domain,
    type = "network",
    metadata = list(
      point_pattern = point_pattern,
      quadrature = Q,
      covariates = covariates,
      quadrature_method = "linequad"
    )
  )
}
