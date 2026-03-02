test_that("Merlin et al method works", {
  set.seed(8675309)
  true_impedance = -1.25

  # this is the same method for calculating example flows that is used in Merlin et al
  # except we are using an unconstrained gravity model for simplicity
  workers = rnorm(400, 1000, 300)
  workers = workers * 400000 / sum(workers)

  jobs = rexp(400, 1 / 1000)
  jobs = jobs * 400000 / sum(jobs)

  times = outer(0:399, 0:399, function(o, d) {
    ox = floor(o / 20)
    oy = o %% 20
    dx = floor(d / 20)
    dy = d %% 20

    (abs(ox - dx) + abs(oy - dy)) * 5
  })

  # assume 20 km/h
  dists = (times + round(runif(400, -2, 2)) + 5) / 20

  # unconstrained negxp
  flows = workers %o% jobs * dists^true_impedance
  flows = flows * 400000 / sum(flows)
  median_trip_dist = Hmisc::wtd.quantile(dists, flows, c(0.5))

  timedf = as_tibble(dists) |>
    mutate(orig_geoid = as.character(1:400)) |>
    tidyr::pivot_longer(-orig_geoid, names_prefix = "V", names_to = "dest_geoid", values_to = "dist_km")

  est_impedance = calibrate_trip_distance_beta(
    tibble::tibble(geoid = as.character(1:400), n_trips = workers),
    tibble::tibble(geoid = as.character(1:400), n_trips = jobs),
    median_trip_dist,
    timedf
  )

  expect_gt(est_impedance, -1.3)
  expect_lt(est_impedance, -1.2)
})
