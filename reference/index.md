# Package index

## All functions

- [`add_households()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/add_households.md)
  : Add households to particular Census tracts in the region

- [`calibrate_trip_distance_beta()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/calibrate_trip_distance_beta.md)
  : This calibrates the trip distance beta, using the method described
  in Merlin (2020) A new method using medians to calibrate
  single-parameter spatial interaction models, JTLU. The basic idea is
  that we adjust the beta for the decay function until half the
  distance-weighted accessibility occurs in the median travel distance.
  For NHB, productions and attractions will be the same

- [`calibrate_trip_distance_betas()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/calibrate_trip_distance_betas.md)
  : Calibrate betas for all trip types median_distances should be a list
  with element HBW, HBO, and NHB with median crow-flies trip distances
  in each.

- [`estimate()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/estimate.md)
  : Estimate a four step model for later use, based on 2017 NHTS data
  and PSRC household survey data (for distribution functions).

- [`estimate_vmt()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/estimate_vmt.md)
  : Calculates VMT based on flows.

- [`get_mode_shares()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/get_mode_shares.md)
  :

  This function uses the output of
  [`mode_choice()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/mode_choice.md)
  to calculate mode shares.

- [`load_landuse_scenario()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/load_landuse_scenario.md)
  : Load a land use scenario in Excel format

- [`load_model()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/load_model.md)
  : Load a model

- [`load_model_v0()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/load_model_v0.md)
  : Load a model in model format 0 (used with pre-2026 releases)

- [`load_nhts()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/load_nhts.md)
  : Load 2017 NHTS data, handling types appropriately

- [`map_congestion()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/map_congestion.md)
  : Produce a congestion map

- [`map_trip_distribution()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/map_trip_distribution.md)
  : Map trip distribution from a single trip.

- [`map_trip_generation()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/map_trip_generation.md)
  : Map trip generation.

- [`mode_choice()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/mode_choice.md)
  : This runs the mode choice step of the model, and returns flows
  differentiated by mode for each trip type and time of day.

- [`modify_ways()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/modify_ways.md)
  : Modify network attributes of existing OSM ways. Note that this
  currently only updates them for modeling; visualizations and GIS
  exports are not changed.

- [`network_assignment()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/network_assignment.md)
  : This runs the network assignment step of the model.

- [`network_to_gis()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/network_to_gis.md)
  : Convenience function to export a network scenario to a GIS file

- [`save_landuse_scenario()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/save_landuse_scenario.md)
  : Save a land use scenario in Excel format

- [`save_model()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/save_model.md)
  : Save a model

- [`trip_distribution()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/trip_distribution.md)
  : This runs the trip distribution step of the model

- [`trip_generation()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/trip_generation.md)
  : This runs the trip generation step of the model.

- [`write_lm()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/write_lm.md)
  : Write the minimal information to be able to reconstruct enough of an
  lm to be able to do prediction.

- [`write_mnl()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/write_mnl.md)
  : Write just enough of a multinomial logit model that we can
  deserialize and apply it
