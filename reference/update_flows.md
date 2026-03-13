# This function takes the output from igraph and updates the flows using it. Doing this in R requires too many levels of indirection and is really slow, so we hand this little piece off to Rust (though this does require a complation step so may ultimately not be worth it).

This function takes the output from igraph and updates the flows using
it. Doing this in R requires too many levels of indirection and is
really slow, so we hand this little piece off to Rust (though this does
require a complation step so may ultimately not be worth it).

## Usage

``` r
update_flows(edge_flows, flows_to_node, predecessor, incoming_edge, origin)
```
