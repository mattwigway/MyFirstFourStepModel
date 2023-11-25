#' This estimates mode choice models based on the NHTS
#' This is much simplified from the traditional mode choice models used in demand modeling,
#' because we don't have attributes of the alternatives (or enough information to create them).
#' So we just use demographics and mode-invariant trip characteristics to estimate mode choice.
estimate_mode_choice_model = function (nhts) {
    # we have four mode-choice alternatives: car, transit, walk, bike
    trips_hb = nhts$trips %>%
        inner_join(nhts$households, by="HOUSEID") %>%
        mutate(
            # unique index for trips
            trip_id=1:n(),
            # unique index for persons (shouldn't matter for MNL)
            #per_unique_id=HOUSEID * 100 + PERSONID,
            choice = dplyr::case_match(TRPTRANS,
                1 ~ "Walk",
                2 ~ "Bike",
                c(3, 4, 5, 6, 18) ~ "Car",
                c(10, 11, 12, 13, 14, 15, 16) ~ "Transit",
                .default=NA
            ),
            trip_type=get_trip_type(WHYFROM, WHYTO),
            time_period=get_time_period(STRTTIME),
            dist_km=TRPMILES * MILES_TO_KILOMETERS / sqrt(2)
        ) %>%
        filter(!is.na(choice) & trip_type != "NHB")

    # this is VERY simplistic model, because we don't have zone level stats from the NHTS,
    # or HH level stats from our model. It won't fit well, but that is not the point.
    hbmodel = multinom(choice ~ HTRESDN + dist_km + factor(time_period) + factor(trip_type), trips_hb)

    # NHB uses a different model, because we don't know the density at either end of the trip
    trips_nhb_indexed = nhts$trips %>%
        inner_join(nhts$households, by="HOUSEID") %>%
        mutate(
            # unique index for trips
            trip_id=1:n(),
            # unique index for persons (shouldn't matter for MNL)
            #per_unique_id=HOUSEID * 100 + PERSONID,
            choice = dplyr::case_match(TRPTRANS,
                1 ~ "Walk",
                2 ~ "Bike",
                c(3, 4, 5, 6, 18) ~ "Car",
                c(10, 11, 12, 13, 14, 15, 16) ~ "Transit",
                .default=NA
            ),
            trip_type=get_trip_type(WHYFROM, WHYTO),
            time_period=get_time_period(STRTTIME),
            dist_km=TRPMILES * MILES_TO_KILOMETERS / sqrt(2)
        ) %>%
        filter(!is.na(choice) & trip_type == "NHB")

        nhbmodel = multinom(choice ~ dist_km + factor(time_period), trips_nhb_indexed)

    return(list(
        HB = hbmodel,
        NHB = nhbmodel
    ))
}

#' Estimate numbers of trips by purpose and mode
flow_by_mode = function (odmat, marginals, mode_choice_models) {
    # density at the home end of home-based trips
    densities = get_densities(marginals$marginals, marginals$areas)

    hb_data = filter(odmat, trip_type != "NHB") %>%
        left_join(densities, by=c("orig_geoid"="geoid"))
    
    # predict produces probabilties by mode
    # mutate(across()) converts to trip counts
    hb_data = bind_cols(hb_data, predict(mode_choice_models$HB, hb_data, "probs")) %>%
        mutate(across(c("Car", "Bike", "Walk", "Transit"), \(x) x * n_trips))

    # handle NHB differently since we don't know densities
    nhb_data = filter(odmat, trip_type == "NHB")

    nhb_data = bind_cols(nhb_data, predict(mode_choice_models$NHB, nhb_data, "probs")) %>%
        mutate(across(c("Car", "Bike", "Walk", "Transit"), \(x) x * n_trips))

    return(bind_rows(hb_data, nhb_data))
}