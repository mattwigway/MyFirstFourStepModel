# This estimates 20 trip production regressions - home based work (from home), home based work (to home), home based other (from home), home based other (to home), and non-home-based, stratified by time of day (AM peak, PM peak, midday, overnight).

Note that everything is ultimately joined to the households table; if
you wish to filter households, you can filter that table and not any of
the others.

## Usage

``` r
estimate_production_functions(nhts)
```
