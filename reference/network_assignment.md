# This runs the network assignment step of the model.

Network assignment is based on a Frank-Wolfe static traffic assignment
algorithm. This returns a list of link-level flows. It applies peaking
factors and average occupancy before doing assignment to convert period
person-trips to hourly vehicle-trips.

## Usage

``` r
network_assignment(model, scenario, network, mode_flows, period)
```

## Arguments

- model:

  The estimated model

- scenario:

  The land-use scenario to use

- network:

  The network scenario to use

- mode_flows:

  Flows by mode and time of day, output of mode_choice function

- period:

  Time period to assign, can be "AM Peak", "Midday", "PM Peak", or
  "Overnight".
