test_that(
  "interaction integral can be estimated from Kinhom",
  {

    point_pattern <- spatstat.geom::ppp(
      x = c(
        0.2,
        0.3,
        0.7,
        0.8,
        0.5
      ),
      y = c(
        0.2,
        0.8,
        0.3,
        0.7,
        0.5
      ),
      window = spatstat.geom::owin(
        xrange = c(0, 1),
        yrange = c(0, 1)
      )
    )

    rmax <- 0.1

    lambda <- rep(
      5,
      spatstat.geom::npoints(point_pattern)
    )

    K <- spatstat.explore::Kinhom(
      point_pattern,
      lambda = lambda,
      r = c(0, rmax),
      correction = "isotropic"
    )

    K_data <- as.data.frame(K)

    r_index <- which.min(
      abs(
        K_data$r - rmax
      )
    )

    expected <- calculate_interaction_integral(
      K_value = K_data$iso[r_index],
      rmax = rmax
    )

    result <- estimate_interaction_integral(
      point_pattern = point_pattern,
      rmax = rmax,
      lambda = lambda,
      correction = "isotropic"
    )

    expect_equal(
      result,
      expected
    )

    expect_true(
      is.finite(result)
    )
  }
)


test_that(
  "interaction integral estimator accepts different edge corrections",
  {

    point_pattern <- spatstat.geom::ppp(
      x = c(
        0.2,
        0.3,
        0.7,
        0.8,
        0.5
      ),
      y = c(
        0.2,
        0.8,
        0.3,
        0.7,
        0.5
      ),
      window = spatstat.geom::owin(
        xrange = c(0, 1),
        yrange = c(0, 1)
      )
    )

    lambda <- rep(
      5,
      spatstat.geom::npoints(point_pattern)
    )

    expect_no_error(
      estimate_interaction_integral(
        point_pattern = point_pattern,
        rmax = 0.1,
        lambda = lambda,
        correction = "border"
      )
    )

    expect_no_error(
      estimate_interaction_integral(
        point_pattern = point_pattern,
        rmax = 0.1,
        lambda = lambda,
        correction = "translate"
      )
    )
  }
)


test_that(
  "interaction integral estimator validates lambda",
  {

    point_pattern <- spatstat.geom::ppp(
      x = c(
        0.2,
        0.3,
        0.7,
        0.8,
        0.5
      ),
      y = c(
        0.2,
        0.8,
        0.3,
        0.7,
        0.5
      ),
      window = spatstat.geom::owin(
        xrange = c(0, 1),
        yrange = c(0, 1)
      )
    )

    expect_error(
      estimate_interaction_integral(
        point_pattern = point_pattern,
        rmax = 0.1,
        lambda = c(1, 2)
      ),
      "one value for each point"
    )

    expect_error(
      estimate_interaction_integral(
        point_pattern = point_pattern,
        rmax = 0.1,
        lambda = c(
          1,
          2,
          3,
          4,
          NA
        )
      ),
      "positive finite values"
    )
  }
)
