# This contains code to estimate trip generation models. It expects data from the 2017 NHTS to estimate the models,
# but you are welcome to filter the data to your region, etc. before estimation.

# home purposes, for the WHYFROM and WHYTO variables
HOME_PURPOSES = c(1, 2)

# work purposes, for the WHYFROM and WHYTO variables
WORK_PURPOSES = c(3, 4)

#' This returns the trip type for each trip in an NHTS trip table. It will classify trips as
#' HBW_Outbound - Home based work, from home
#' HBW_Return - Home based work, return home
#' HBO_Outbound - Home based other, from home
#' HBO_Return - Home based other, return home
#' NHB - Non-home-based
get_trip_type <- function (whyfrom, whyto) {
    case_when(
        whyfrom %in% HOME_PURPOSES & whyto %in% WORK_PURPOSES ~ "HBW_Outbound",
        whyfrom %in% WORK_PURPOSES & whyto %in% HOME_PURPOSES ~ "HBW_Return",
        whyfrom %in% HOME_PURPOSES ~ "HBO_Outbound",
        whyto %in% HOME_PURPOSES ~ "HBO_Return",
        # unknown purposes wind up here
        .default = "NHB"
    ) %>%
    factor() %>%
    return()
}

#' Get the trip counts in each category and time period for each household
get_nhts_trip_counts <- function (trips) {
    trips %>%
        mutate(
            trip_type=get_trip_type(WHYFROM, WHYTO),
            time_period=get_time_period(STRTTIME),
            # the number of people who likely reported this trip
            # because youngchildren don't have trip records but are in NUMONTRP
            persons_reported_trip=rowSums(across(starts_with("ONTD_P"), \(x) x == 1))
        ) %>%
        group_by(HOUSEID, trip_type, time_period) %>%
        summarize(trip_count = sum(1 / persons_reported_trip)) %>%
        return()
}

#' This estimates 20 trip generation regressions - home based work (from home), home based work (to home),
#' home based other (from home), home based other (to home), and non-home-based, stratified by time of day
#' (AM peak, PM peak, midday, overnight).
#' 
#' Note that everything is ultimately joined to the households table; if you wish to filter households, you
#' can filter that table and not any of the others.
estimate_generation_functions <- function (nhts) {
    # calculate trip counts by purpose and time of day
    hh_trip_counts = get_nhts_trip_counts(nhts$trips)

    result = list()

    for (tp in unique(hh_trip_counts$time_period)) {
        result[[tp]] = list()
        for (tt in unique(hh_trip_counts$trip_type)) {
            regdata = nhts$households %>%
                left_join(filter(hh_trip_counts, trip_type == tt & time_period == tp)) %>%
                mutate(
                    trip_count = replace_na(trip_count, 0),
                    vehicles = pmin(HHVEHCNT, VEHICLES_TOPCODE),
                    workers = pmin(WRKCOUNT, WORKER_TOPCODE),
                    hhsize = pmin(HHSIZE, HHSIZE_TOPCODE)
                )

            if (all(regdata$trip_count == 0)) error(paste("No trips for", tt, tp))

            result[[tp]][[tt]] = lm(trip_count ~ vehicles + hhsize + factor(income) + HTRESDN + workers, regdata)
        }
    }

    return(result)
}

#' This generates a seed matrix for IPF of the variables in the regression.
#' This seed matrix is used to disaggregate ACS marginals to household numbers so we can
#' apply the regression. The software ships with a precalculated seed matrix based on the
#' 2017-2021 PUMS five-year sample (see ipf_seed_matrix.qmd for calculations). This used the full 
#' US, to ensure coverage in every category. This doesn't imply that every tract looks like the full
#' US, only that the relationships between the marginals are constant across the US.
get_seed_matrix = function () {
    #return(read_csv(system.file("extdata/seed.csv", package="bf4sm")))
    # FIXME path hardwirder
    return(read_csv("R/inst/extdata/seed.csv"))
}

#' This generates the number of households in each category for a specific geographic area (e.g. tract).
get_hh_counts_for_tract = function(grp, id, seed) {
    # convert group to marginals, in ipfr format (named list with )
    marginals = grp %>%
        select(marginal, value, count) %>%
        # use split to get a named list: https://github.com/tidyverse/dplyr/issues/4223
        split(.$marginal) %>%
        lapply(\(g) pivot_wider(g, names_from=value, values_from=count) %>% select(-marginal))

    if (any(is.na(marginals$vehicles))) stop(paste("At TAZ", id, "some vehicle marginals are NA"))
    if (any(is.na(marginals$income))) stop(paste("At TAZ", id, "some income marginals are NA"))
    if (any(is.na(marginals$hhsize))) stop(paste("At TAZ", id, "some hhsize marginals are NA"))
    if (any(is.na(marginals$workers))) stop(paste("At TAZ", id, "some workers marginals are NA"))

    # running IPF on every tract is pretty slow, but this is a demo
    result = ipfr::ipu(seed, marginals)
    return(select(result$weight_tbl, -c("avg_weight", "weight_factor")) %>% rename(count="weight"))
}

#' This generates the number of households in each category in a geographic area
#' given marginal distributions.
#' 
#' The marginal distributions we expect are
#' - Household size (hhsize), topcoded at HHSIZE_TOPCODE
#' - Vehicles available (vehicles), topcoded at VEHICLES_TOPCODE
#' - Number of workers (workers), topcoded at WORKERS_TOPCODE
#' - Income category (income), see load_nhts.R
#' 
#' These should be formatted as a table with columns geoid, marginal, value, count,
#' where geoid is a tract ID, marginal is the name of one of the marginals (above),
#' value is the value for that marginal (e.g. vehicles = 2 for two-vehicle households),
#' and count is the expected count in that category
get_hh_counts = function (marginals) {
    seed = get_seed_matrix()

    marginals %>%
        group_by(geoid) %>%
        group_modify(\(g, k) get_hh_counts_for_tract(g, k, seed)) %>%
        return()
}

prepare_regression_data = function (marginals, areas) {
    # figure out densities based on marginals
    densities = marginals %>%
        filter(marginal == "vehicles") %>%
        group_by(geoid) %>%
        summarize(total_hh=sum(count)) %>%
        ungroup() %>%
        left_join(areas, by="geoid") %>%
        # NB this is assuming 100% occupancy, because we have households not housing units
        mutate(HTRESDN=total_hh / area_sqmi) %>%
        select(geoid, HTRESDN)

    get_hh_counts(marginals) %>%
        left_join(densities, by="geoid") %>%
        return()
}

#' This generates trip counts by tract and time period, based on marginals and
#' trip regressions. It expects marginals in the format described in the docs for
#' get_hh_counts, and areas with a geoid and area_sqmi column with the area of each
#' geographic unit
get_trip_counts = function (marginals, areas, generation_functions) {
    hh_counts = prepare_regression_data(marginals, areas)

    # now, apply each model
    result = purrr::imap(generation_functions, function (models, period) {
        purrr::imap(models, function (model, ttype) {
            tibble(
                geoid = hh_counts$geoid,
                trip_type = ttype,
                time_period = period,
                # predict trips for one household, multiply by hhs in category
                n_trips = predict(model, hh_counts) * hh_counts$count
            )
        }) %>%
        list_rbind() %>%
        return()
    }) %>%
    list_rbind() %>%
    group_by(geoid, trip_type, time_period) %>%
    summarize(n_trips=sum(n_trips)) %>%
    return()
}