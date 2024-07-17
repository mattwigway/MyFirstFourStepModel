#' @export
map_trip_generation = function (model, trip_counts, end, timeperiod, triptype) {
    if (str_to_lower(end) == "productions") {
        counts = trip_counts$productions
    } else if (str_to_lower(end) == "attractions") {
       counts = trip_counts$attractions
    } else {
        stop("End must be `productions` or `attractions`")
    }

    counts = counts %>%
        filter(time_period == timeperiod & trip_type == triptype)
    
    model$tazs_geo %>%
        st_transform(3857) %>%
        left_join(counts, by=c("GEOID"="geoid")) %>%
        ggplot(aes(fill=n_trips / ALAND * (1000 * 1000))) +
            geom_sf() +
            scale_fill_fermenter(palette="Blues", direction=1) +
            labs(fill="Trips per\nsquare kilometer") +
            ggtitle(paste0(triptype, " ", str_to_lower(end), ", ", timeperiod)) +
            geom_sf(data=model$network_geo, fill="black", linewidth=0.35) +
            theme_minimal() +
            theme(axis.text = element_blank(), panel.grid = element_blank())
}

#' @export
map_trip_distribution = function (model, flows, timeperiod, triptype, origin_tract) {
    from_tract = flows %>%
        filter(time_period == timeperiod & trip_type == triptype & orig_geoid == origin_tract)

    model$tazs_geo %>%
        st_transform(3857) %>%
        left_join(from_tract, by=c("GEOID"="dest_geoid")) %>%
        ggplot(aes(fill=n_trips)) +
            geom_sf() +
            scale_fill_fermenter(palette="Greens", direction=1) +
            labs(fill=paste0("Number of trips\ndestined for tract\n", "(total: ",  format(round(sum(from_tract$n_trips)), scientific=F), ")")) +
            ggtitle(paste(triptype, "trips,", timeperiod, "from tract", origin_tract)) +
            geom_sf(data=filter(model$tazs_geo, GEOID==origin_tract), fill="blue") +
            geom_sf(data=model$network_geo, fill="black", size=0.35) +
            theme_minimal() +
            theme(axis.text = element_blank(), panel.grid = element_blank())

}

#' @export
map_congestion = function (model, flows) {
    ff_tt = get_freeflow_weights(model$network)
    con_tt = get_congested_tt(model$network, flows)
    ff_to_con_ratio = ff_tt / con_tt

    flow_tibble = tibble(
        eid = as.integer(edge_attr(model$network, "id")),
        ff_to_con_ratio = ff_to_con_ratio
    )
    
    model$network_geo %>%
        st_transform(3857) %>%
        left_join(flow_tibble, by="eid") %>%
        ggplot(aes(color=ff_to_con_ratio, linewidth=as.numeric(highway_type == "motorway"))) +
            ggplot2::geom_sf() +
            ggplot2::scale_linewidth_continuous(range=c(0.5, 0.75)) +
            ggplot2::scale_color_fermenter(palette="RdBu", labels=scales::percent, direction=1) +
            ggplot2::labs(color="Percent of free-flow speed") +
            theme_minimal() +
            theme(axis.text = element_blank(), panel.grid = element_blank()) +
            guides(linewidth="none")

}