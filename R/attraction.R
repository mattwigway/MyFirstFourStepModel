#' Is a tract ID in the PSRC region?
is_psrc_tract = function (tract_id) {
    return(
        stringr::str_starts(tract_id, "53033") | # King
        stringr::str_starts(tract_id, "53035") | # Kitsap
        stringr::str_starts(tract_id, "53053") | # Pierce
        stringr::str_starts(tract_id, "53061") # Snohomish
    )
}

#' This estimates the distribution models on the Seattle data
#' It expects LODES data already aggregated to tracts
estimate_attraction_functions = function (trips, lodes) {
    jobs = lodes %>%
        aggregate_lodes_to_tracts()

    trips = trips %>%
        # remove trips without origin/destination 
        filter(!is.na(o_tract10) & !is.na(d_tract10)) %>%
        # retain trips we have skims for, remove out of region trips
        filter(is_psrc_tract(o_tract10) & is_psrc_tract(d_tract10)) %>%
        # categorize trips
        mutate(
            trip_type=case_when(
                origin_purpose_cat == "Home" & dest_purpose_cat %in% c("Work", "Work-related") ~ "HBW",
                origin_purpose_cat %in% c("Work", "Work-related") & dest_purpose_cat == "Home" ~ "HBW",
                origin_purpose_cat == "Home" ~ "HBO",
                dest_purpose_cat == "Home" ~ "HBO",
                .default="NHB"
            ),
            # for NHB trips, we're just going to take the average of the productions and generations, so
            # order does not matter
            prod_tract10=ifelse(dest_purpose_cat=="Home", d_tract10, o_tract10),
            attract_tract10=ifelse(dest_purpose_cat=="Home", o_tract10, d_tract10),
            # get the time period - convert string time to NHTS time and use same function
            # using depart time is intentional, for consistency with production - trips are
            # assigned to the time period they start in.
            time_period=depart_time_string %>%
                stringr::str_sub(18, 22) %>%
                stringr::str_replace(":", "") %>%
                as.integer() %>%
                get_time_period()
        )

    # note that we have a sample of trips, but they have expansion weights, so should be unbiased.
    # calculate the number of trips attracted by each tract
    attract_counts = trips %>%
        group_by(attract_tract10, time_period, trip_type) %>%
        summarize(tr_count=sum(trip_weight_2017_2019)) %>%
        pivot_wider(names_from="trip_type", values_from="tr_count") %>%
        left_join(jobs, by=c("attract_tract10"="geoid")) %>%
        mutate(across(starts_with("C"), \(x) replace_na(x, 0)))

    # TODO are there tracts with no trips in the survey?
    
    # NHB models don't have a clearly defined production/attraction end, so we use the average
    # of origins and destinations, and use this for both productions and attractions in the
    # distribution model
    nhb_counts = trips %>%
        filter(trip_type == "NHB") %>%
        pivot_longer(c("prod_tract10", "attract_tract10"), values_to="geoid") %>%
        group_by(geoid, time_period) %>%
        # / 2 to get an average of origins and destinations
        summarize(tr_count=sum(trip_weight_2017_2019) / 2) %>%
        left_join(jobs, by="geoid") %>%
        mutate(across(starts_with("C"), \(x) replace_na(x, 0)))

    result = list()

    for (tp in unique(nhb_counts$time_period)) {
        attract_filtered = filter(attract_counts, time_period==tp)
        nhb_filtered = filter(nhb_counts, time_period==tp)
        result[[tp]] = list(
            HBO = lm(HBO ~ C000 + CNS07 + CNS15 + CNS18, attract_filtered),
            HBW = lm(HBW ~ C000 + CNS07 + CNS15 + CNS18, attract_filtered),
            NHB = lm(tr_count ~ C000 + CNS07 + CNS15 + CNS18, nhb_filtered)
        )
    }

    return(result)
}

#' This generates attraction estimates from LODES data
#' Marginals are only used to get the list of tracts, since
#' LODES does not contain all tracts
get_attraction_counts = function (marginals, attraction_functions) {
    tract_lodes = marginals$marginals %>%
        group_by(geoid) %>%
        summarize() %>% # make geoid unique (many marginals per tract)
        left_join(marginals$jobs, by="geoid") %>%
        mutate(across(starts_with("C"), \(x) replace_na(x, 0)))

    # apply each production model
    purrr::imap(attraction_functions, function (models, time_period) {
        purrr::imap(models, function (model, trip_type) {
                tibble(
                    geoid=tract_lodes$geoid,
                    trip_type=trip_type,
                    time_period=time_period,
                    n_trips=pmax(predict(model, tract_lodes), 0)
                )
            }) %>%
            list_rbind()
    }) %>%
    list_rbind() %>%
    return()
}

#' This adjusts the total attraction numbers to match total production. It returns a list
#' with members attractions containing the balanced attractions, production containing the
#' original productions, and factors containing the adjustment factors
balance_production_attraction = function (production, attraction) {
    total_produced = production %>%
        group_by(trip_type, time_period) %>%
        summarize(n_produced=sum(n_trips))

    total_attracted = attraction %>%
        group_by(trip_type, time_period) %>%
        summarize(n_attracted=sum(n_trips))

    balance_factors = total_produced %>%
        left_join(total_attracted, by=c("trip_type", "time_period")) %>%
        mutate(balance_factor = n_produced / n_attracted)

    balanced_attractions = attraction %>%
        left_join(select(balance_factors, trip_type, time_period, balance_factor), by=c("trip_type", "time_period")) %>%
        mutate(n_trips = n_trips * balance_factor) %>%
        select(-c("balance_factor"))

    return(list(
        productions=production,
        attractions=balanced_attractions,
        balance_factors=balance_factors
    ))
}