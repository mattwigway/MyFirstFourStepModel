#' This gets baseline marginals for a region, from the 5-year ACS
get_base_marginals = function (state, county=NULL, year=NULL) {
    vehicles = tidycensus::get_acs(
        geography="tract",
        state=state,
        county=county,
        variables = c(
            # use tenure by vehicles available as it is household level
            vehicles_owned_0 = "B25044_003",
            vehicles_owned_1 = "B25044_004",
            vehicles_owned_2 = "B25044_005",
            vehicles_owned_3 = "B25044_006",
            vehicles_owned_4 = "B25044_007",
            vehicles_owned_5 = "B25044_008",
            vehicles_rented_0 = "B25044_010",
            vehicles_rented_1 = "B25044_011",
            vehicles_rented_2 = "B25044_012",
            vehicles_rented_3 = "B25044_013",
            vehicles_rented_4 = "B25044_014",
            vehicles_rented_5 = "B25044_015"
        ),
        year=year,
        survey="acs5"
    ) %>%
        mutate(
            marginal = stringr::str_split_i(variable, "_", 1),
            tenure = stringr::str_split_i(variable, "_", 2),
            value = pmin(as.integer(stringr::str_split_i(variable, "_", 3)), VEHICLES_TOPCODE),
        ) %>%
        group_by(GEOID, marginal, value) %>%
        summarize(count=sum(estimate), moe=sqrt(sum(moe^2))) %>%
        rename(geoid="GEOID")

    # we force everything to match total hhs from the vehicles table
    total_hh = vehicles %>%
        group_by(geoid) %>%
        summarize(total_hh=sum(count))

    hhsize = tidycensus::get_acs(
        geography="tract",
        state=state,
        county=county,
        year=year,
        survey="acs5",
        variables=c(
            "hhsize_1"="S2501_C01_002",
            "hhsize_2"="S2501_C01_003",
            "hhsize_3"="S2501_C01_004",
            "hhsize_4"="S2501_C01_005"
        )
    ) %>%
    mutate(
        marginal=stringr::str_split_i(variable, "_", 1),
        value=as.integer(stringr::str_split_i(variable, "_", 2))
    ) %>%
    rename(geoid="GEOID", count=estimate) %>%
    left_join(total_hh, by="geoid") %>%
    group_by(geoid) %>%
    mutate(count = count / sum(count) * total_hh) %>%
    ungroup() %>%
    select(geoid, marginal, value, count, moe)

    workers = tidycensus::get_acs(
        geography="tract",
        state=state,
        county=county,
        year=year,
        survey="acs5",
        variables=c(
            "workers_0" = "B08202_002",
            "workers_1" = "B08202_003",
            "workers_2" = "B08202_004",
            "workers_3" = "B08202_005"
        )
    ) %>%
    mutate(
        marginal=stringr::str_split_i(variable, "_", 1),
        value=as.integer(stringr::str_split_i(variable, "_", 2))
    ) %>%
    rename(geoid="GEOID", count="estimate") %>%
    left_join(total_hh, by="geoid") %>%
    group_by(geoid) %>%
    mutate(count = count / sum(count) * total_hh) %>%
    ungroup() %>%
    select(geoid, marginal, value, count, moe)

    income = tidycensus::get_acs(
        geography="tract",
        state=state,
        county=county,
        year=year,
        survey="acs5",
        variables=c(
            "income_0_1"="S1901_C01_002",
            "income_0_2"="S1901_C01_003",
            "income_0_3"="S1901_C01_004",
            "income_0_4"="S1901_C01_005",
            "income_35000_1"="S1901_C01_006",
            "income_35000_2"="S1901_C01_007",
            "income_75000_1"="S1901_C01_008",
            "income_100000_1"="S1901_C01_009",
            "income_100000_2"="S1901_C01_010",
            "income_100000_3"="S1901_C01_011"
        )
    ) %>%
    mutate(
        marginal=stringr::str_split_i(variable, "_", 1),
        value=as.integer(stringr::str_split_i(variable, "_", 2))
    ) %>%
    rename(geoid="GEOID") %>%
    group_by(geoid, marginal, value) %>%
    summarize(
        count=sum(estimate),
        moe=sqrt(sum(moe^2))
    ) %>%
    left_join(total_hh, by="geoid") %>%
    group_by(geoid) %>%
    mutate(count = count / sum(count) * total_hh) %>%
    ungroup() %>%
    select(-total_hh)


    # now, get tract areas - also contain lat/lons of a representative point in the tract
    areas = tigris::tracts(state=state, county=county, year=year) %>%
        as_tibble() %>%
        mutate(area_sqmi=ALAND / (1609^2)) %>%
        rename(geoid="GEOID", lat=INTPTLAT, lon=INTPTLON) %>%
        mutate(lat=as.numeric(lat), lon=as.numeric(lon)) %>%
        select(geoid, area_sqmi, lat, lon)

    marginals = dplyr::bind_rows(hhsize, vehicles, workers, income) %>%
        group_by(geoid) %>%
        filter(!all(replace_na(count, 0) == 0)) %>%
        ungroup() %>%
        arrange(geoid, marginal, value) 
    
    return(list(
        marginals=marginals,
        areas=areas
    ))
}