# This generates the number of households in each category in a geographic area given marginal distributions.

The marginal distributions we expect are

- Household size (hhsize), topcoded at HHSIZE_TOPCODE

- Vehicles available (vehicles), topcoded at VEHICLES_TOPCODE

- Number of workers (workers), topcoded at WORKERS_TOPCODE

- Income category (income), see load_nhts.R

## Usage

``` r
get_hh_counts(marginals, seed)
```

## Details

These should be formatted as a table with columns geoid, marginal,
value, count, where geoid is a tract ID, marginal is the name of one of
the marginals (above), value is the value for that marginal (e.g.
vehicles = 2 for two-vehicle households), and count is the expected
count in that category
