#' We use a singly-constrained (at origin) gravity model. For simplicity, we use straight-line
#' (great circle) distance between points.
#' This function estimates that distance matrix.
get_distance_matrix = function (marginals) {
    locs = sf::st_as_sf(marginals$areas, coords=c("lon", "lat"), crs=4269)
    dists = sf::st_distance(locs, locs) %>% as_tibble()
    colnames(dists) = marginals$areas$geoid
    dists$orig_geoid = marginals$areas$geoid
    dists$orig_area_sqmi = marginals$areas$area_sqmi
    dists %>%
        pivot_longer(-c("orig_geoid", "orig_area_sqmi"), names_to="dest_geoid", values_to="dist_m") %>%
        mutate(
            dist_km=ifelse(
                    orig_geoid == dest_geoid,
                    # within-zone trips have an average distance longer than zero meters, but figuring out what it is
                    # is tricky. We use a "magic number" of 0.52 * sqrt(area). This is derived from a Monte Carlo simulation
                    # of the average Euclidean distance between randomly chosen points in a square. There are two factors that
                    # make this incorrect, but we hope they cancel out:
                    # 1. Tracts are not square, they are more complex and less compact generally. This makes these
                    #    estimated distances an underestimate.
                    # 2. Development is not evenly distributed across a tract, but likely concentrated in certain areas. This
                    #    makes these estimated distances an overestimate.
                    0.52 * sqrt(orig_area_sqmi) * MILES_TO_KILOMETERS,
                    as.numeric(dist_m) / 1000
                )
        ) %>%
        select(-orig_area_sqmi, -dist_m) %>%
        return()
}

#' This calibrates the trip distance beta, using the method described in Merlin (2020)
#' A new method using medians to calibrate single-parameter spatial interaction models, JTLU.
#' The basic idea is that we adjust the beta for the decay function until half the distance-weighted
#' accessibility occurs in the median travel distance.
#' For NHB, productions and attractions will be the same
calibrate_trip_distance_beta = function (productions, attractions, median_dist_km, dmat) {
    # we assume that the betas for each trip type are the same at all time periods
    origins = productions %>%
        group_by(geoid) %>%
        summarize(n_trips=sum(n_trips)) %>%
        rename(orig_geoid="geoid", orig_ntrips="n_trips")

    dests = attractions %>%
        group_by(geoid) %>%
        summarize(n_trips=sum(n_trips)) %>%
        rename(dest_geoid="geoid", dest_ntrips="n_trips")


    access = origins %>%
        cross_join(dests) %>%
        # avoid overflow
        mutate(weighted_attractions=
            as.double(dest_ntrips) *
            (as.double(orig_ntrips) / as.double(mean(orig_ntrips)))
         ) %>%
        left_join(dmat, by=c("orig_geoid", "dest_geoid"))

    stopifnot(all(access$weighted_attractions > 0))

    opt_res = optim(c(-2.0), function (params) {
        beta = params[[1]]

        # calculate the objective function - the squared difference between
        # the access before and the access after the median.
        # The Merlin paper uses abs(), but we use squared so the derivative is smooth
        # at the optimum
        decayed_attractions = access$weighted_attractions * access$dist_km ^ beta
        decayed_below = sum(decayed_attractions[access$dist_km < median_dist_km])
        decayed_above = sum(decayed_attractions[access$dist_km >= median_dist_km])
        return((decayed_below - decayed_above) ^ 2)
        # Per Merlin's paper, the function should be globally con(cave or vex, I forget)
        # so the constraints shouldn't matter, TODO that's not what I observe. I suspect
        # it has to do with underflow when parameters get significantly negative. So constrain here.
    }, method="Brent", lower=-3, upper=5)

    stopifnot(opt_res$convergence == 0)

    stopifnot(opt_res$value < 1e-3)
    beta = opt_res$par[[1]]
    stopifnot(beta < 0)

    # make sure it's not a corner solution
    stopifnot(beta > -3)
    
    return(beta)
}

#' Calibrate betas for all trip types
#' median_distances should be a list with element HBW, HBO, and NHB with median crow-flies
#' trip distances in each.
calibrate_trip_distance_betas = function (balanced, marginals, median_distances_m) {
    dmat = get_distance_matrix(marginals)
    return(list(
        HBW=calibrate_trip_distance_beta(
            filter(balanced$productions, trip_type=="HBW"),
            filter(balanced$attractions, trip_type=="HBW"),
            median_distances_m$HBW,
            dmat
        ),

        HBO=calibrate_trip_distance_beta(
            filter(balanced$productions, trip_type=="HBO"),
            filter(balanced$attractions, trip_type=="HBO"),
            median_distances_m$HBO,
            dmat
        ),

        # NHB is a little different. The NHB productions are linked to the home location, so we
        # use the attractions for both ends of the trip.
        NHB=calibrate_trip_distance_beta(
            filter(balanced$attractions, trip_type=="NHB"),
            filter(balanced$attractions, trip_type=="NHB"),
            median_distances_m$NHB,
            dmat
        )
    ))
}

#' Estimates the median crow-flies distance from the NHTS in kilometers, by dividing the horse-flies distance
#' by the square root of two. The theoretical basis for this is that this is the worst-case ratio
#' between the Euclidean and Manhattan distance. This would seem like too aggressive of a correction,
#' but not every location has a grid. I determine this factor empirically from a dataset of network distances
#' (I don't recall the details) when I was in undergrad (or possibly high school).
estimate_median_crow_flies_distance = function (nhts) {
    meddist = nhts$trips %>%
        mutate(trip_type=get_trip_type(WHYFROM, WHYTO)) %>%
        group_by(trip_type) %>%
        summarize(dist_m=Hmisc::wtd.quantile(TRPMILES, WTTRDFIN, probs=c(0.5)) * MILES_TO_KILOMETERS / sqrt(2)) %>%
        pivot_wider(names_from=trip_type, values_from=dist_m)

    return(list(
        HBW = meddist$HBW[[1]],
        HBO = meddist$HBO[[1]],
        NHB = meddist$NHB[[1]]
    ))
}

#' Perform the singly-constrained gravity model to get zone-to-zone flows based on balanced
#' trip tables and estimated betas from above.
get_flows = function (balanced, marginals, betas) {
    dmat = get_distance_matrix(marginals)
    purrr::map(unique(balanced$productions$trip_type), function (tt) {
        purrr::map(unique(balanced$productions$time_period), function (tp) {
            # get trip counts for this trip type and time period
            # for NHB, we use attractions for both trip ends
            if (tt == "NHB") {
                prod = filter(balanced$attractions,
                    trip_type == tt & time_period == tp) %>%
                    rename(orig_geoid="geoid", orig_ntrips="n_trips")
            } else {
                prod = filter(balanced$productions,
                    trip_type == tt & time_period == tp) %>%
                    rename(orig_geoid="geoid", orig_ntrips="n_trips")
            }

            
            att = filter(balanced$attractions, trip_type == tt & time_period == tp) %>%
                rename(dest_geoid="geoid", dest_ntrips="n_trips")
            
            prod %>%
                cross_join(att) %>%
                left_join(dmat, by=c("orig_geoid", "dest_geoid")) %>%
                group_by(orig_geoid) %>%
                mutate(
                    # singly constrained gravity model: https://tfresource.org/topics/Destination_Choice_Theoretical_Foundations.html
                    n_trips=(orig_ntrips * dest_ntrips * dist_km ^ betas[[tt]]) / sum(dest_ntrips * dist_km ^ betas[[tt]])
                ) %>%
                select(orig_geoid, dest_geoid, n_trips, dist_km) %>%
                mutate(trip_type=tt, time_period=tp) %>%
                return()
         }) %>%
         list_rbind()
    }) %>%
    list_rbind()
}