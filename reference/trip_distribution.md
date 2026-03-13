# This runs the trip distribution step of the model

Trip distribution is based on the relative locations of the tracts (in
`marginals`), the betas estimated during the estimation phase of the
model, and the total trip productions and attractions.

## Usage

``` r
trip_distribution(model, scenario, balanced)
```

## Arguments

- model:

  the model object, loaded via
  [`load_model()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/load_model.md)

- scenario:

  the scenario to use, often `model$scenarios$baseline` or a scenario
  created with
  [`add_households()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/add_households.md)

- balanced:

  the balanced productions and attractions, returned by
  [`trip_generation()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/trip_generation.md)
