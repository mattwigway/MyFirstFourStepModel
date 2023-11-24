#' Aggregate LODES data to tracts
aggregate_lodes_to_tracts = function (lodes) {
    lodes %>%
        mutate(
            # 2 state, 3 county, 6 tract = 11
            tract = stringr::str_sub(w_geocode, 1, 11)
        ) %>%
        group_by(tract) %>%
        summarize(across(starts_with("C"), sum)) %>%
        return()
}

#' This estimates the distribution models on the Seattle data
#' It expects LODES data already aggregated to tracts
estimate_seattle_models = function (trips, lodes, skims) {
    jobs = lodes %>%
        aggregate_lodes_to_tracts() %>%
        select(tract, C000) %>% rename(total_jobs="C000")

    counts = trips %>%
        # remove trips without origin/destination 
        filter(!is.na(o_tract10) & !is.na(d_tract10)) %>%
        # retain trips we have skims for, remove out of region trips
        inner_join(skims, by=c("o_tract10"="orig_geoid", "d_tract10"="dest_geoid")) %>%
        # categorize trips
        mutate(
            # unlike counts, we don't estimate separate inbound and outbound models - we assume impedance is the 
            # same in both directions
            trip_type=case_when(
                origin_purpose_cat == "Home" & dest_purpose_cat %in% c("Work", "Work-related") ~ "HBW",
                origin_purpose_cat %in% c("Work", "Work-related") & dest_purpose_cat == "Home" ~ "HBW",
                origin_purpose_cat == "Home" ~ "HBO",
                dest_purpose_cat == "Home" ~ "HBO",
                .default="NHB"
            ),
            # for NHB trips, home will be the start and nonhome will be the end
            home_tract10=ifelse(dest_purpose_cat=="Home", d_tract10, o_tract10),
            nonhome_tract10=ifelse(dest_purpose_cat=="Home", o_tract10, d_tract10)
        ) %>%
        group_by(home_tract10, nonhome_tract10, trip_type) %>%
        summarize(
            tr_count=sum(trip_weight_2017_2019, na.rm=T),
            # weighted average of trips in each direction
            duration_minutes=weighted.mean(duration_minutes, trip_weight_2017_2019)
        ) %>%
        pivot_wider(names_from=trip_type, values_from=tr_count) %>%
        left_join(rename(jobs, home_total_jobs="total_jobs"), by=c("home_tract10"="tract")) %>%
        left_join(rename(jobs, nonhome_total_jobs="total_jobs"), by=c("nonhome_tract10"="tract"))

    # get hh counts
    hh_counts = tidycensus::get_acs(
        "tract",
        state="WA",
        county=c("King", "Snohomish", "Kitsap", "Pierce"),
        year=2021,
        survey="acs5",
        variables=c("total_hh"="S1901_C01_001"),
        output="wide"
    ) %>%
    select(GEOID, total_hhE)

    counts = counts %>%
        left_join(rename(hh_counts, home_total_hh="total_hhE"), by=c("home_tract10"="GEOID")) %>%
        left_join(rename(hh_counts, nonhome_total_hh="total_hhE"), by=c("nonhome_tract10"="GEOID"))

    # note that we have a sample of trips, but they have expansion weights, so should be unbiased.
    # It's not going to be perfect, as many tract-pairs will have no trips at all.

    # expand to have all tract-pairs
    tract_pairs = hh_counts %>%
        select("GEOID") %>%
        rename(home_tract10="GEOID") %>%
        dplyr::cross_join(select(hh_counts, GEOID)) %>%
        rename(nonhome_tract10="GEOID") %>%
        left_join(counts, by=c("home_tract10", "nonhome_tract10")) %>%
        mutate(across(everything(), \(x) replace_na(x, 0)))

    return(list(
        # home-based work and home-based other: hh counts at the home end and job counts at the nonhome end
        HBW=lm(log(HBW + 1) ~ log(home_total_hh + 1) + log(nonhome_total_jobs + 1) + log(pmax(duration_minutes, 1)), tract_pairs),
        HBO=lm(log(HBO + 1) ~ log(home_total_hh + 1) + log(nonhome_total_jobs + 1) + log(pmax(duration_minutes, 1)), tract_pairs),
        # non-home-based: total jobs at each end. home_ and nonhome_ are misnomers, just refer to orig and dest
        NHB=lm(log(NHB + 1) ~ log(home_total_jobs + 1) + log(nonhome_total_jobs + 1) + log(pmax(duration_minutes, 1)), tract_pairs)
    ))
}
