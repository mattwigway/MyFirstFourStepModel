# We implement IPF on our own in base R and Rust.

We implement IPF on our own in base R and Rust.

## Usage

``` r
ipf(seed, marginals)
```

## Arguments

- seed:

  should be a data frame with a row for each household, and a column for
  each marginal indicating what value that household takes on (e.g. age,
  race, etc columns). It should also have a weight column indicating the
  seed weight for that household (or type of household).

- marginals:

  should be a data frame with a row for each marginal/value combination,
  and a column `marginal` indicating which marginal this is (should
  match column names in `seed`), a column `value` indicating what value
  of the marginal this is (should match column values in `seed`), and a
  column `count` indicating the target households in this marginal/value
  combination.
