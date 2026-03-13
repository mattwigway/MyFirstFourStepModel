# This generates trip counts by tract and time period, based on marginals and trip regressions. It expects marginals in the format described in the docs for get_hh_counts, and areas with a geoid and area_sqmi column with the area of each geographic unit

This generates trip counts by tract and time period, based on marginals
and trip regressions. It expects marginals in the format described in
the docs for get_hh_counts, and areas with a geoid and area_sqmi column
with the area of each geographic unit

## Usage

``` r
get_production_counts(marginals, generation_functions, seed)
```
