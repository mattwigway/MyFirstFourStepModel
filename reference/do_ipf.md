# Internal IPF code, similar to what was previously done in R. For efficiency, marginal names and values are converted to integers before calling.

Internal IPF code, similar to what was previously done in R. For
efficiency, marginal names and values are converted to integers before
calling.

## Usage

``` r
do_ipf(
  orig_counts,
  marginal_values,
  target_marginals,
  target_values,
  target_counts,
  convergence
)
```

## Arguments

- orig_counts:

  numeric vector with number of each household sampled in seed matrix

- marginal_values:

  integer matrix with one row per household and one column per marginal,
  indicating which value that household has for each marginal.

- target_marginals:

  This along with target_values and target_counts indicate what the
  target values for each marginal should be.

- target_values:

  The value of this marginal (e.g. for income, might be 2 to represent
  \$25-50,000)

- target_counts:

  The number of households expected with this value.

- convergence:

  The largest error in absolute terms that can be tolerated, e.g. 0.01
  -\> errors of no more than 0.02 hhs on any marginal.
