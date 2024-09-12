#' Modify network attributes of existing OSM ways. Note that this currently only updates them for
#' modeling; visualizations and GIS exports are not changed.
#' 
#' @param network Network to modify
#' @param ways OSM way ID or character vector of way IDs. Since OSM IDs are 64 bit integers and R does not support 64 bit integers, these should be strings
#' @param lanes_per_direction Number of lanes per direction to set on the way (optional)
#' @param highway_type Highway type of the way. Currently motorway has higher capacity and all other values are treated as the same.
#' @param maxspeed_kph Maximum speed on the way, in kilometers per hour
#' 
#' @export
modify_ways = function (network, ways, lanes_per_direction=NULL, highway_type=NULL, maxspeed_kph=NULL) {
    if (!is.vector(ways)) {
        ways = c(ways)
    }

    if (!is.character(ways[[1]])) {
        stop("Way IDs must be strings")
    }

    # copy graph
    graph = subgraph(network$network, V(network$network))

    ways_in_graph = ways %in% E(graph)$way_id

    if (!all(ways_in_graph)) {
        missing_ways = ways[!ways_in_graph]
        stop(glue("Ways {missing_ways} not in network!"))
    }

    edges_to_modify = E(graph)$way_id %in% ways

    if (!is.null(lanes_per_direction)) {
        E(graph)[edges_to_modify]$lanes_per_direction = lanes_per_direction
    }

    if (!is.null(highway_type)) {
        E(graph)[edges_to_modify]$highway_type = highway_type
    }

    if (!is.null(maxspeed_kph)) {
        E(graph)[edges_to_modify]$maxspeed_kph = maxspeed_kph
    }

    return(list(
        network=graph,
        # TODO update attributes
        network_geo=network$network_geo
    ))
}

