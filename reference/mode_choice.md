# This runs the mode choice step of the model, and returns flows differentiated by mode for each trip type and time of day.

This runs the mode choice step of the model, and returns flows
differentiated by mode for each trip type and time of day.

## Usage

``` r
mode_choice(model, scenario, flows)
```

## Arguments

- model:

  The estimated model object

- scenario:

  The scenario to use

- flows:

  The output of trip_distribution
