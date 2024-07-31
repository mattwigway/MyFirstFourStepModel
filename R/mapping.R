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
            label_cities(model) +
            theme_minimal() +
            theme(axis.text = ggplot2::element_blank(), panel.grid = ggplot2::element_blank())
}

#' @export
map_trip_distribution = function (model, flows, timeperiod, triptype, origin_tract) {
    from_tract = flows %>%
        filter(time_period == timeperiod & trip_type == triptype & orig_geoid == origin_tract)

    plot_data = model$tazs_geo %>%
        st_transform(3857) %>%
        left_join(from_tract, by=c("GEOID"="dest_geoid")) %>%
        mutate(n_trips=replace_na(n_trips, 0))

    # use quantile breaks so that there is some variation on the map and it's not dominated by nearby destinations
    # Quantiles are weighted by the number of trips, so that tracts above the 80th percentile (for example) represent
    # 20% of the trips, not 20% of the tracts
    breaks = round(signif(wtd.quantile(plot_data$n_trips, plot_data$n_trips, c(0, 0.2, 0.4, 0.6, 0.8, 1)), digits=1), digits=0)
    names(breaks) = NULL
    breaks = pmax(breaks, c(0:4, ceiling(max(plot_data$n_trips))))
    
    plot_data %>%
        ggplot(aes(fill=n_trips)) +
            geom_sf() +
            scale_fill_fermenter(palette="Greens", direction=1, breaks=breaks) +
            labs(fill=paste0("Number of trips\ndestined for tract\n", "(total: ",  format(round(sum(from_tract$n_trips)), scientific=F), ")")) +
            ggtitle(paste(triptype, "trips,", timeperiod, "from tract", origin_tract)) +
            geom_sf(data=filter(model$tazs_geo, GEOID==origin_tract), fill="blue") +
            geom_sf(data=model$network_geo, fill="black", size=0.35) +
            label_cities(model) +
            theme_minimal() +
            theme(axis.text = ggplot2::element_blank(), panel.grid = ggplot2::element_blank())
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
            ggplot2::scale_color_fermenter(palette="RdBu", labels=scales::percent, direction=1, breaks=c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) +
            ggplot2::labs(color="Percent of free-flow speed") +
            label_cities(model) +
            theme_minimal() +
            theme(axis.text = ggplot2::element_blank(), panel.grid = ggplot2::element_blank()) +
            ggplot2::guides(linewidth="none")

}

# burn NE cities into compiled R package (this is evaluated at compile time)
NE_CITIES = ne_download(scale=10, type="populated_places_simple", returnclass = "sf")

#' Add city labels from Natural Earth
label_cities = function (model, buffer = 300) { # buffer in meters for web mercator
    # find relevant cities
    cities = st_join(st_transform(NE_CITIES, st_crs(model$tazs_geo)), model$tazs_geo, left=F) %>%
        st_transform(3857)

    r = list()

    # draw buffer around text by repeated offsets -
    # inspired by https://github.com/GuangchuangYu/shadowtext/blob/master/R/shadowtext-grob.R
    # but for sf objects
    for (xquad in c(-1, 1)) {
        for (yquad in c(-1, 1)) {
            for (angle in c(0, 22.5, 45, 67.5)) {
                xoff = xquad * cos(angle * pi / 180) * buffer
                yoff = yquad * sin(angle * pi / 180) * buffer

                r = append(r, geom_sf_text(data=cities, aes(label=name, fill=NULL), color="white", position=position_nudge(xoff, yoff)))
            }
        }
    }

    r = append(r, geom_sf_text(data=cities, aes(label=name, fill=NULL), color="black"))

    return(r)
}