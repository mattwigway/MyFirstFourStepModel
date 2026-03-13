# Modify network attributes of existing OSM ways. Note that this currently only updates them for modeling; visualizations and GIS exports are not changed.

Modify network attributes of existing OSM ways. Note that this currently
only updates them for modeling; visualizations and GIS exports are not
changed.

## Usage

``` r
modify_ways(
  network,
  ways,
  lanes_per_direction = NULL,
  highway_type = NULL,
  maxspeed_kph = NULL
)
```

## Arguments

- network:

  Network to modify

- ways:

  OSM way ID or character vector of way IDs. Since OSM IDs are 64 bit
  integers and R does not support 64 bit integers, these should be
  strings

- lanes_per_direction:

  Number of lanes per direction to set on the way (optional)

- highway_type:

  Highway type of the way. Currently motorway has higher capacity and
  all other values are treated as the same.

- maxspeed_kph:

  Maximum speed on the way, in kilometers per hour
