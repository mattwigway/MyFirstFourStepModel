# This runs the homework assignment baseline case, and ensures results are stable
test_that("Integration test - homework is correct", {
  # TODO pulling this over HTTP is hacky
  model = load_model("https://files.indicatrix.org/chatham_park.model")

  productions_attractions = trip_generation(model, model$scenarios$baseline)
  expect_snapshot(productions_attractions)

  flows = trip_distribution(
    model,
    model$scenarios$baseline,
    productions_attractions
  )
  expect_snapshot(flows)

  flows_by_mode = mode_choice(model, model$scenarios$baseline, flows)
  expect_snapshot(flows_by_mode)

  mode_shares = get_mode_shares(flows_by_mode)
  expect_snapshot(mode_shares)

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
  expect_snapshot(link_flows)
})
