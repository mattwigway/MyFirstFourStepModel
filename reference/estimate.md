# Estimate a four step model for later use, based on 2017 NHTS data and PSRC household survey data (for distribution functions).

Load the NHTS data with load_nhts(), and if desired filter the
households table to just the households you want to use in estimation.
Julia must already be installed. If you get an error that Julia is not
found, you need to set the JULIA_HOME environment variable to the
directory containing the Julia executable.

## Usage

``` r
estimate(
  nhts,
  osm,
  state,
  county,
  year,
  highway_types = c("motorway", "motorway_link", "trunk", "trunk_link", "primary",
    "primary_link")
)
```

## Arguments

- nhts:

  Path to 2017 NHTS CSV files

- osm:

  Path to OSM .pbf file

- state:

  State to estimate model for

- county:

  County (or vector of counties) to estimate model for

- year:

  Year of ACS and LODES data to use (if you get 404 errors, you are
  probably trying to use a year that LODES is not available for)

- highway_types:

  OSM highway= tags to include in network, default "motorway",
  "motorway_link", "trunk", "trunk_link", "primary", "primary_link"
