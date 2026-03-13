# This links all tracts in the marginals to the nearest node in the network. We only link to a single node for simplicity. We assume zero centroid-connector travel time. It sets the node index as node_idx in marginals\$areas, and returns the updated marginals.

This links all tracts in the marginals to the nearest node in the
network. We only link to a single node for simplicity. We assume zero
centroid-connector travel time. It sets the node index as node_idx in
marginals\$areas, and returns the updated marginals.

## Usage

``` r
link_tracts(network, marginals)
```
