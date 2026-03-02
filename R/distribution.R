#' We use a singly-constrained (at origin) gravity model. For simplicity, we use straight-line
#' (great circle) distance between points.
#' This function estimates that distance matrix.
get_distance_matrix = function(marginals) {
  locs = sf::st_as_sf(marginals$areas, coords = c("lon", "lat"), crs = 4269)
  dists = sf::st_distance(locs, locs) %>% as_tibble()
  colnames(dists) = marginals$areas$geoid
  dists$orig_geoid = marginals$areas$geoid
  dists$orig_area_sqmi = marginals$areas$area_sqmi
  dists %>%
    pivot_longer(-c("orig_geoid", "orig_area_sqmi"), names_to = "dest_geoid", values_to = "dist_m") %>%
    mutate(
      dist_km = ifelse(
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

#' Estimates the median crow-flies distance from the NHTS in kilometers, by dividing the horse-flies distance
#' by 1.3. This is based on the paper https://arxiv.org/abs/2406.06490
estimate_median_crow_flies_distance = function(nhts) {
  meddist = nhts$trips %>%
    mutate(
      trip_type = get_trip_type(WHYFROM, WHYTO),
      persons_reported_trip = rowSums(across(starts_with("ONTD_P"), \(x) x == 1))
    ) %>%
    group_by(trip_type) %>%
    # we want the median vehicle trip
    # TODO labeled dist_m but calculated in km
    summarize(
      dist_m = Hmisc::wtd.quantile(TRPMILES, WTTRDFIN / persons_reported_trip, probs = c(0.5)) *
        MILES_TO_KILOMETERS /
        1.3
    ) %>%
    pivot_wider(names_from = trip_type, values_from = dist_m)

  return(list(
    HBW = meddist$HBW[[1]],
    HBO = meddist$HBO[[1]],
    NHB = meddist$NHB[[1]]
  ))
}

#' Perform the singly-constrained gravity model to get zone-to-zone flows based on balanced
#' trip tables and estimated betas from above.
get_flows = function(balanced, marginals, betas) {
  dmat = get_distance_matrix(marginals)
  purrr::map(unique(balanced$productions$trip_type), function(tt) {
    purrr::map(unique(balanced$productions$time_period), function(tp) {
      # get trip counts for this trip type and time period
      # for NHB, we use attractions for both trip ends
      if (tt == "NHB") {
        prod = filter(balanced$attractions, trip_type == tt & time_period == tp) %>%
          rename(orig_geoid = "geoid", orig_ntrips = "n_trips")
      } else {
        prod = filter(balanced$productions, trip_type == tt & time_period == tp) %>%
          rename(orig_geoid = "geoid", orig_ntrips = "n_trips")
      }

      att = filter(balanced$attractions, trip_type == tt & time_period == tp) %>%
        rename(dest_geoid = "geoid", dest_ntrips = "n_trips")

      prod %>%
        cross_join(att) %>%
        left_join(dmat, by = c("orig_geoid", "dest_geoid")) %>%
        group_by(orig_geoid) %>%
        mutate(
          # singly constrained gravity model: https://tfresource.org/topics/Destination_Choice_Theoretical_Foundations.html
          n_trips = (orig_ntrips * dest_ntrips * dist_km^betas[[tt]]) / sum(dest_ntrips * dist_km^betas[[tt]])
        ) %>%
        select(orig_geoid, dest_geoid, n_trips, dist_km) %>%
        mutate(trip_type = tt, time_period = tp) %>%
        return()
    }) %>%
      list_rbind()
  }) %>%
    list_rbind()
}
