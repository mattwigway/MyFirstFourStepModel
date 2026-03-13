# Build the OSM network (using Julia)

Build the OSM network (using Julia)

## Usage

``` r
build_network(osm, highway_types, installJulia = T)
```

## Arguments

- osm:

  The OSM file to use

- highway_types:

  Character vector of highway=\* tags to retain

- installJulia:

  Whether to install Julia if it is not found (default TRUE)

## Value

A list with members \$network (igraph network) and \$network_geo (sf
DataFrame with geographic outline of network)
