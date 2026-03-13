# This runs the trip generation step of the model.

Trip production is based on the marginal distributions for vehicles,
workers, household size, and income. Trip attraction is based on job
counts. Attractions are balanced to match productions before being
returned.

## Usage

``` r
trip_generation(model, scenario)
```

## Arguments

- model:

  the model object, loaded via
  [`load_model()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/load_model.md)

- scenario:

  the scenario to use, often `model$scenarios$baseline` or a scenario
  created with
  [`add_households()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/add_households.md)
