# Assignment: this code implements a simple Frank-Wolfe static traffic assignment algorithm

#' Up through the mode choice step, everything is done with productions at home and attractions at work.
#' For assignment, we need origins and destinations.
#' This calculates factors to figure out how many trips of each home-based type travel in each direction
#' during a particular time period
calculate_direction_factors = function (nhts) {
    nhts$trips %>%
        mutate(
            trip_type=get_trip_type(WHYFROM, WHYTO),
            time_period=get_time_period(STRTTIME),
            # the number of people who likely reported this trip
            # because youngchildren don't have trip records but are in NUMONTRP
            persons_reported_trip=rowSums(across(starts_with("ONTD_P"), \(x) x == 1))
        ) %>%
        filter(trip_type != "NHB") %>%
        group_by(trip_type, time_period) %>%
        summarize(outbound=weighted.mean(WHYFROM %in% HOME_PURPOSES, WTTRDFIN / persons_reported_trip)) %>%
        return()
}

#' This calculates vehicle occupancy factors by trip type and time period
calculate_occupancy_factors = function (nhts) {
    nhts$trips %>%
        mutate(
            trip_type=get_trip_type(WHYFROM, WHYTO),
            time_period=get_time_period(STRTTIME),
            # the number of people who likely reported this trip
            # because youngchildren don't have trip records but are in NUMONTRP
            persons_reported_trip=rowSums(across(starts_with("ONTD_P"), \(x) x == 1))
        ) %>%
        filter(TRPTRANS %in% NHTS_CAR_MODES) %>%
        group_by(trip_type, time_period) %>%
        summarize(average_occupancy=weighted.mean(persons_reported_trip, WTTRDFIN)) %>%
        return()
}

#' This applies direction factors to an origin-destination matrix
apply_direction_factors = function (matrix, direction_factors) {
    mat_with_directions = matrix %>%
        filter(trip_type != "NHB") %>%
        left_join(direction_factors, by=c("trip_type", "time_period"))

    # calculate the outbound and inbound
    outbound = mat_with_directions %>%
        mutate(across(c("Car", "Bike", "Walk", "Transit"), \(x) x * outbound))

    inbound = mat_with_directions %>%
        mutate(across(c("Car", "Bike", "Walk", "Transit"), \(x) x * (1 - outbound)))

    # swap columns
    inbound[,c("orig_geoid", "dest_geoid")] = inbound[,c("dest_geoid", "orig_geoid")]

    # Put it all together, and bring back NHB
    bind_rows(inbound, outbound, filter(matrix, trip_type=="NHB")) %>%
        group_by(orig_geoid, dest_geoid, time_period, trip_type) %>%
        summarize(across(c("Car", "Bike", "Walk", "Transit"), sum)) %>%
        return()
}

# From the SCAG ABM
BPR_ALPHA = 0.6
BPR_BETA = 5.0

#' This links all tracts in the marginals to the nearest node in the network. We only link to
#' a single node for simplicity. We assume zero centroid-connector travel time. It sets the
#' node index as node_idx in marginals$areas, and returns the updated marginals.
link_tracts = function (network, marginals) {
    all_nodes = tibble(lat=vertex_attr(network, "lat"), lon=vertex_attr(network, "lon")) %>%
        st_as_sf(coords=c("lon", "lat"), crs=4326)

    all_tracts = marginals$areas %>%
        st_as_sf(coords=c("lon", "lat"), crs=4269) %>%
        st_transform(4326)

    dmat = st_distance(all_nodes, all_tracts)

    marginals$areas$node_idx = apply(dmat, 2, which.min)

    return(marginals)
}

#' The all-or-nothing assignment step, where all vehicles traveling from A to B are assigned to the
#' same route. This is done repeatedly with new weights and averaged to distribute vehicles across
#' the network.
all_or_nothing = function (nodeflow_hourly, marginals, network, weights) {
    flows = rep(0.0, ecount(network))

    all_to = unique(nodeflow_hourly$dest_node)

    for (fr in unique(nodeflow_hourly$orig_node)) {
        #print(glue::glue("processing node {fr}"))
        paths = shortest_paths(
            network,
            fr,
            to = all_to,
            mode="out",
            weights=weights,
            predecessors = TRUE
        )

        #print("routing complete")

        frflows = filter(nodeflow_hourly, orig_node == fr)

        # the algorithm works like this, to take advantage of R vectorized operations
        # we calculate the flows to each node at the end of the process, which are just
        # the flows flows from the origin to each dest node (many may be zero).
        # we increment the edges from the predecessor node.
        # then we recalculate flows_to_node as all of the flows that left that node,
        # which must be all of the flows into that node b/c of the tree structure.

        # sum up all flows into each node on the last edge traversed
        flows_to_node = rep(0, vcount(network))
        flows_to_node[frflows$dest_node] = frflows$Car

        predecessors = as.integer(paths$predecessors)
        nas = which(is.na(predecessors))
        predecessors[nas] = nas

        # incoming edge for each vertex in this SPT (0 if not in SPT)
        incoming_edge = as.integer(get_edge_ids(network, as.vector(rbind(predecessors, 1:vcount(network)))))

        update_flows(flows, flows_to_node, predecessors, incoming_edge, as.integer(fr))
    }

    return(flows)
}

get_freeflow_weights = function (network) {
    return(edge_attr(network, "length_m") / 1000 / edge_attr(network, "maxspeed_kph") * 60)
}

#' Gets lane capacity based on OSM highway type
#' Capacities come from Mannering and Washburn, Principles of Highway Engineering and Traffic Analysis, 7th ed.
get_lane_capacity = function (highway_types) {
    return(
        case_match(
            highway_types,
            "motorway" ~ 2350, # 65 mph freeway
            .default=1200 # 1900 veh/lane/hr on 45 mph multilane segments, assumed average 63% green time
        )
    )
}

get_congested_tt = function (network, flows) {
    ff_time = get_freeflow_weights(network)
    
    capacity_per_lane = get_lane_capacity(edge_attr(network, "highway_type"))
    capacity = capacity_per_lane * edge_attr(network, "lanes_per_direction")
    # Simple BPR function
    return(ff_time * (1 + BPR_ALPHA * (flows / capacity) ^ BPR_BETA))
}

#' Get the aggregate cost to all travelers of the current assignment
get_aggregate_cost = function (network, flows) {
    return(sum(flows * get_congested_tt(network, flows)))
}

#' Find the optimal lambda value
find_optimal_lambda = function (network, old_flows, aon_flows) {
    opt_res = optim(c(0.5), function (par) {
        lambda = par[[1]]
        combined_flows = aon_flows * lambda + old_flows * (1 - lambda)
        congested_tt = get_congested_tt(network, combined_flows)
        # square it because optim minimizes rather than root-finding
        return(sum(congested_tt * (aon_flows - old_flows)) ^ 2)
    }, method="Brent", upper=1, lower=0)

    return(opt_res$par[[1]])
}

#' This does the network assignment
#' The default relgap_tol is 1% instead of the recommended 0.01% to speed convergence in the
#' teaching environment.
frank_wolfe = function(odflow_hourly, marginals, network, maxiter=100, relgap_tol=1e-2) {
    # convert odflow_hourly to be node-to-node rather than zone-to-zone
    nodeflow_hourly = odflow_hourly |>
        left_join(select(marginals$areas, orig_geoid=geoid, orig_node=node_idx), by="orig_geoid") |>
        left_join(select(marginals$areas, dest_geoid=geoid, dest_node=node_idx), by="dest_geoid") |>
        group_by(orig_node, dest_node) |>
        summarize(Car=sum(Car)) |>
        ungroup()

    # start with all-or-nothing flows
    current_flows = all_or_nothing(nodeflow_hourly, marginals, network, get_freeflow_weights(network))
    converged = F
    relgap = 0
    for (iteration in 1:maxiter) {
        # in each iteration, we create new all-or-nothing flows with the congested weights, and
        # average them into the flows. We iterate until the result is stable.
        aon_flows = all_or_nothing(nodeflow_hourly, marginals, network, get_congested_tt(network, current_flows))
        lambda = find_optimal_lambda(network, current_flows, aon_flows)
        new_flows = aon_flows * lambda + current_flows * (1 - lambda)
        relgap = get_relative_gap(network, current_flows, new_flows)
        current_flows = new_flows
        if (relgap <= relgap_tol) {
            print(paste("Assignment converged at iteration", iteration, "with relative gap", relgap))
            converged = T
            break
        } else {
            print(paste("Iteration", iteration, "relative gap:", relgap))
        }
    }

    if (!converged) {
        print("Assignment did not converge, returning flows with relative gap", relgap)
    }

    return(current_flows)
}

get_relative_gap = function (network, old_flows, new_flows) {
    old_agg_cost = get_aggregate_cost(network, old_flows)
    new_agg_cost = get_aggregate_cost(network, new_flows)
    return((old_agg_cost - new_agg_cost) / old_agg_cost)
}

map_flows = function (flows, network, geo) {
    flow_tibble = tibble(
        eid = as.integer(edge_attr(network, "id")),
        flow = flows
    )
    
    geo %>%
        left_join(flow_tibble, by="eid") %>%
        ggplot(aes(linewidth=flow, color=flow)) +
            ggplot2::geom_sf() +
            ggplot2::scale_linewidth_binned() +
            ggplot2::scale_color_fermenter(palette="RdBu")
}
