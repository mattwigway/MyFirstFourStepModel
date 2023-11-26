#' Aggregate LODES data to tracts
aggregate_lodes_to_tracts = function (lodes) {
    lodes %>%
        mutate(
            # 2 state, 3 county, 6 tract = 11
            geoid = stringr::str_sub(w_geocode, 1, 11)
        ) %>%
        group_by(geoid) %>%
        summarize(across(starts_with("C"), sum)) %>%
        return()
}

#' This gets baseline marginals for a region, from the 5-year ACS
#' @export
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

    # get LODES data
    # use LODES8 for 2020 tracts, LODES7 otherwise
    lodes_version = ifelse(year >= 2020, "8", "7")
    lodes_url = paste0("https://lehd.ces.census.gov/data/lodes/LODES", lodes_version, "/", str_to_lower(state),
        "/wac/", str_to_lower(state), "_wac_S000_JT00_", year, ".csv.gz")

    lodes = read_csv(lodes_url) %>%
        aggregate_lodes_to_tracts() %>%
        select(geoid, C000, CNS07, CNS15, CNS18)

    # make jobs and marginals data match
    jobs = marginals %>%
        group_by(geoid) %>%
        summarize() %>%
        left_join(lodes) %>%
        mutate(across(starts_with("C"), \(x) replace_na(x, 0)))
    
    return(list(
        marginals=marginals,
        areas=areas,
        jobs=jobs
    ))
}

#' Save a land use scenario in Excel format
#' @export
save_landuse_scenario = function (marginals, filename) {
    write_xlsx(list(
        Residences=marginals$marginals,
        Jobs=marginals$jobs,
        `Tract Areas`=marginals$areas
    ), filename)
}

#' Load a land use scenario in Excel format
#' @export
load_landuse_scenario = function (filename) {
    return(list(
        marginals=read_excel(filename, sheet="Residences"),
        jobs=read_excel(filename, sheet="Jobs"),
        areas=read_excel(filename, sheet="Tract Areas")
    ))
}