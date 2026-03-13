# This function uses the output of [`mode_choice()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/mode_choice.md) to calculate mode shares.

This function uses the output of
[`mode_choice()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/mode_choice.md)
to calculate mode shares.

## Usage

``` r
get_mode_shares(flows_by_mode)
```

## Arguments

- flows_by_mode:

  output of
  [`mode_choice()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/mode_choice.md)

## Value

a data frame with columns for car, bike, walk, and transit.
