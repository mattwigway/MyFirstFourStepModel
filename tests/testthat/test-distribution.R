test_that("distribution works", {
  marginals = list(
    areas = tibble::tribble(
      ~geoid  , ~lon  , ~lat , ~area_sqmi             ,
      # points are evenly spaced, ~11.1 km apart
      # internal trips will be assumed to have a distance of 2, 3, and 4 kms
      "one"   , -79   ,    0 , (2 / 0.52)^2 / 1.609^2 ,
      "two"   , -79.1 ,    0 , (3 / 0.52)^2 / 1.609^2 ,
      "three" , -79.2 ,    0 , (4 / 0.52)^2 / 1.609^2
    )
  )

  productions_attractions = list(
    productions = tibble::tribble(
      ~geoid  , ~trip_type , ~time_period , ~n_trips ,
      "one"   , "HBW"      , "AM Peak"    ,       10 ,
      "two"   , "HBW"      , "AM Peak"    ,       20 ,
      "three" , "HBW"      , "AM Peak"    ,       30 ,
      "one"   , "HBO"      , "AM Peak"    ,        5 ,
      "two"   , "HBO"      , "AM Peak"    ,       10 ,
      "three" , "HBO"      , "AM Peak"    ,       15 ,
      "one"   , "HBW"      , "PM Peak"    ,      110 ,
      "two"   , "HBW"      , "PM Peak"    ,      120 ,
      "three" , "HBW"      , "PM Peak"    ,      130 ,
      "one"   , "HBO"      , "PM Peak"    ,       15 ,
      "two"   , "HBO"      , "PM Peak"    ,      110 ,
      "three" , "HBO"      , "PM Peak"    ,      115
    ),
    attractions = tibble::tribble(
      ~geoid  , ~trip_type , ~time_period , ~n_trips ,
      "one"   , "HBW"      , "AM Peak"    ,       20 ,
      "two"   , "HBW"      , "AM Peak"    ,       10 ,
      "three" , "HBW"      , "AM Peak"    ,       30 ,
      "one"   , "HBO"      , "AM Peak"    ,        5 ,
      "two"   , "HBO"      , "AM Peak"    ,       15 ,
      "three" , "HBO"      , "AM Peak"    ,       10 ,
      "one"   , "HBW"      , "PM Peak"    ,       55 ,
      "two"   , "HBW"      , "PM Peak"    ,      165 ,
      "three" , "HBW"      , "PM Peak"    ,      140 ,
      "one"   , "HBO"      , "PM Peak"    ,       60 ,
      "two"   , "HBO"      , "PM Peak"    ,       80 ,
      "three" , "HBO"      , "PM Peak"    ,      100
    )
  )

  betas = list(
    HBW = -1.2,
    HBO = -1.2
  )

  flows = get_flows(productions_attractions, marginals, betas)

  # test single constrained model
  flows |>
    group_by(orig_geoid, trip_type, time_period) |>
    summarize(n_trips_pred = sum(n_trips)) |>
    ungroup() |>
    dplyr::full_join(
      productions_attractions$productions,
      by = c("orig_geoid" = "geoid", "trip_type", "time_period")
    ) |>
    with(expect_true(all.equal(n_trips_pred, n_trips)))

  spacing = 11.11951 # length of 0.1 degrees longitude at equator
  dists = tibble::tribble(
    ~orig_geoid , ~dest_geoid , ~dist_km    ,
    "one"       , "one"       ,           2 , # by construction, see comments on marginals$area_sqmi
    "one"       , "two"       , spacing     ,
    "one"       , "three"     , 2 * spacing ,
    "two"       , "one"       , spacing     ,
    "two"       , "two"       ,           3 ,
    "two"       , "three"     , spacing     ,
    "three"     , "one"       , 2 * spacing ,
    "three"     , "two"       , spacing     ,
    "three"     , "three"     ,           4
  )

  expected_trip_counts = productions_attractions$productions |>
    rename(orig_geoid = "geoid") |>
    # will create one row per location pair
    dplyr::full_join(
      rename(productions_attractions$attractions, dest_geoid = "geoid"),
      by = c("trip_type", "time_period"),
      relationship = "many-to-many"
    ) |>
    left_join(dists, by = c("orig_geoid", "dest_geoid")) |>
    dplyr::rowwise() |>
    mutate(n_trips = n_trips.x * n_trips.y * dist_km^betas[[trip_type]]) |>
    group_by(orig_geoid, trip_type, time_period) |>
    mutate(n_trips = n_trips / sum(n_trips) * n_trips.x) |>
    arrange(orig_geoid, trip_type, time_period)

  flows = flows |> arrange(orig_geoid, trip_type, time_period)

  expect_equal(
    flows$n_trips,
    expected_trip_counts$n_trips,
    tolerance = 0.01
  )
})
