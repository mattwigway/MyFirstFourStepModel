test_that("assignment works", {
  # Graph looks like this:
  #.    2
  #   /   \
  # 1      4
  #  \.   /
  #.   3
  # All edges are directed and point right.
  # 2,000 people wish to go from 1 to 4. They should be split evenly among paths
  # 124 and 134
  net = igraph::make_empty_graph(directed = T) +
    igraph::vertex(1:4) +
    igraph::edge(1, 2) +
    igraph::edge(1, 3) +
    igraph::edge(2, 4) +
    igraph::edge(3, 4)

  E(net)$length_m = 1000
  E(net)$maxspeed_kph = 30
  E(net)$highway_type = "primary"
  E(net)$lanes_per_direction = 1

  odflow = tibble::tribble(
    ~orig_geoid , ~dest_geoid , ~Car ,
              1 ,           4 , 2000
  )

  marginals = list(
    areas = tibble::tribble(
      ~geoid , ~node_idx ,
           1 ,         1 ,
           4 ,         4
    )
  )

  flows = frank_wolfe(odflow, marginals, net)

  expect_all_equal(flows, 1000)
})
