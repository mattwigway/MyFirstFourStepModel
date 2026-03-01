# This runs the homework assignment baseline case, and ensures results are stable
test_that("Integration test - homework is correct", {
  # TODO pulling this over HTTP is hacky
  model = load_model("https://files.indicatrix.org/chatham_park.model")

  productions_attractions = trip_generation(model, model$scenarios$baseline)
  expect_snapshot_value(
    readr::format_csv(arrange(
      productions_attractions$productions,
      geoid,
      trip_type,
      time_period
    ))
  )

  expect_snapshot_value(
    readr::format_csv(arrange(
      productions_attractions$attractions,
      geoid,
      trip_type,
      time_period
    ))
  )

  expect_snapshot_value(
    readr::format_csv(arrange(
      productions_attractions$balance_factors,
      trip_type,
      time_period
    ))
  )

  flows = trip_distribution(
    model,
    model$scenarios$baseline,
    productions_attractions
  )
  expect_snapshot_value(readr::format_csv(arrange(
    flows,
    orig_geoid,
    dest_geoid,
    trip_type,
    time_period
  )))

  flows_by_mode = mode_choice(model, model$scenarios$baseline, flows)
  expect_snapshot_value(readr::format_csv(arrange(
    flows_by_mode,
    orig_geoid,
    dest_geoid,
    trip_type,
    time_period
  )))

  mode_shares = get_mode_shares(flows_by_mode)
  expect_snapshot_value(readr::format_csv(mode_shares))

  # Lastly, we can assign all PM peak trips to the network.
  # Every so often it will print a status update, something like
  # Iteration 1 relative gap: 0.40825352014596
  # When that number falls below 0.01, the algorithm will finish.
  # This may 10 minutes or more depending on the speed of your computer.
  link_flows = network_assignment(
    model,
    model$scenarios$baseline,
    model$networks$baseline,
    flows_by_mode,
    "PM Peak"
  )
  expect_snapshot_value(link_flows, style = "serialize")
})
