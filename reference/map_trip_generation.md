# Map trip generation.

Produces a map showing where trips are produced or attracted.

## Usage

``` r
map_trip_generation(model, trip_counts, end, timeperiod, triptype)
```

## Arguments

- model:

  the model to use

- end:

  "productions" or "attractions"

- timeperiod:

  Time period to map (AM Peak, Midday, PM Peak, Overnight)

- triptype:

  Trip type to map (HBO, HBW, NHB)

- trip_count:

  [`trip_generation()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/trip_generation.md)
  results
