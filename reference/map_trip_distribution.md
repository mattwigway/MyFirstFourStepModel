# Map trip distribution from a single trip.

Produces a map showing where trips produced in `origin_tract` are
attracted.

## Usage

``` r
map_trip_distribution(model, flows, timeperiod, triptype, origin_tract)
```

## Arguments

- model:

  the model to use

- flows:

  `trip_distribuion()` results

- timeperiod:

  Time period to map (AM Peak, Midday, PM Peak, Overnight)

- triptype:

  Trip type to map (HBO, HBW, NHB)

- origin_tract:

  Tract to map productions of
