# Calculates VMT based on flows.

Note that this is total VMT for the period the flows are for; you would
have to do this for each period to get total daily VMT.

## Usage

``` r
estimate_vmt(model, network, link_flows, period)
```

## Arguments

- model:

  The estimated model object

- network:

  The network to use (should be the same one used in
  [`network_assignment()`](https://projects.indicatrix.org/MyFirstFourStepModel/reference/network_assignment.md)

- link_flows:

  Estimated link flows from the network assignment function
