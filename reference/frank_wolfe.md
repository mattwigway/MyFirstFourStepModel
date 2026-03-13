# This does the network assignment The default relgap_tol is 1% instead of the recommended 0.01% to speed convergence in the teaching environment.

This does the network assignment The default relgap_tol is 1% instead of
the recommended 0.01% to speed convergence in the teaching environment.

## Usage

``` r
frank_wolfe(
  odflow_hourly,
  marginals,
  network,
  maxiter = 100,
  relgap_tol = 0.01
)
```
