# Add households to particular Census tracts in the region

Add households to particular Census tracts in the region

## Usage

``` r
add_households(scenario, tract, hhs)
```

## Arguments

- scenario:

  An existing land-use scenario

- tract:

  a Census tract ID (or vector of tract IDs)-should be strings, not
  numbers

- hhs:

  Households to add to the tract

## Value

a copy of the scenario with the requested households added

The hhs parameter should be a table/tibble/data.frame of households with
the following columns:

- income: Annual household income, dollars

- hhsize: Household size

- vehicles: Number of vehicles owned by the household

- workers: Number of workers in the household

- n: Number of households like this to add

If there are multiple tracts specified, the households will be evenly
split among them
