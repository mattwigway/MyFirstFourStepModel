# This contains code to estimate trip production models. It expects data from the 2017 NHTS to estimate the models,
# but you are welcome to filter the data to your region, etc. before estimation.

# home purposes, for the WHYFROM and WHYTO variables
HOME_PURPOSES = c(1, 2)

# work purposes, for the WHYFROM and WHYTO variables
WORK_PURPOSES = c(3, 4)

#' This returns the trip type for each trip in an NHTS trip table. It will classify trips as
#' HBW - Home based work
#' HBO - Home based other
#' NHB - Non-home-based
get_trip_type <- function (whyfrom, whyto) {
    case_when(
        whyfrom %in% HOME_PURPOSES & whyto %in% WORK_PURPOSES ~ "HBW",
        whyfrom %in% WORK_PURPOSES & whyto %in% HOME_PURPOSES ~ "HBW",
        whyfrom %in% HOME_PURPOSES ~ "HBO",
        whyto %in% HOME_PURPOSES ~ "HBO",
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

#' This estimates 20 trip production regressions - home based work (from home), home based work (to home),
#' home based other (from home), home based other (to home), and non-home-based, stratified by time of day
#' (AM peak, PM peak, midday, overnight).
#' 
#' Note that everything is ultimately joined to the households table; if you wish to filter households, you
#' can filter that table and not any of the others.
estimate_production_functions <- function (nhts) {
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
                    hhsize = pmin(HHSIZE, HHSIZE_TOPCODE),
                    income = format(income, scientific=F)
                )

            if (all(regdata$trip_count == 0)) error(paste("No trips for", tt, tp))

            result[[tp]][[tt]] = lm(trip_count ~ vehicles + hhsize + factor(income) + HTRESDN + workers, regdata)
        }
    }

    return(result)
}

# This generates a seed matrix for IPF of the variables in the regression.
# This seed matrix is used to disaggregate ACS marginals to household numbers so we can
# apply the regression. The software ships with a precalculated seed matrix based on the
# 2017-2021 PUMS five-year sample (see ipf_seed_matrix.qmd for calculations). This used the full 
# US, to ensure coverage in every category. This doesn't imply that every tract looks like the full
# US, only that the relationships between the marginals are constant across the US.


#' This generates the number of households in each category for a specific geographic area (e.g. tract).
get_hh_counts_for_tract = function(grp, id, seed) {
    # marginals = grp %>%
    #     select(marginal, value, count) %>%
    #     # use split to get a named list: https://github.com/tidyverse/dplyr/issues/4223
    #     split(.$marginal) %>%
    #     lapply(\(g) pivot_wider(g, names_from=value, values_from=count) %>% select(-marginal))

    # if (any(is.na(marginals$vehicles))) stop(paste("At TAZ", id, "some vehicle marginals are NA"))
    # if (any(is.na(marginals$income))) stop(paste("At TAZ", id, "some income marginals are NA"))
    # if (any(is.na(marginals$hhsize))) stop(paste("At TAZ", id, "some hhsize marginals are NA"))
    # if (any(is.na(marginals$workers))) stop(paste("At TAZ", id, "some workers marginals are NA"))

    # running IPF on every tract is pretty slow, but this is a demo
    result = ipf(seed, grp)
    result %>%
        rename(count="weight") %>%
        return()
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
get_hh_counts = function (marginals, seed) {
    marginals$marginals %>%
        group_by(geoid) %>%
        group_modify(\(g, k) get_hh_counts_for_tract(g, k, seed)) %>%
        # don't use scientific notation
        mutate(income=format(income, scientific=F)) %>%
        return()
}

get_densities = function (marginals) {
    marginals$marginals %>%
        filter(marginal == "vehicles") %>%
        group_by(geoid) %>%
        summarize(total_hh=sum(count)) %>%
        ungroup() %>%
        left_join(marginals$areas, by="geoid") %>%
        # NB this is assuming 100% occupancy, because we have households not housing units
        mutate(HTRESDN=total_hh / area_sqmi) %>%
        select(geoid, HTRESDN) %>%
        return()
}

prepare_regression_data = function (marginals, seed) {
    # figure out densities based on marginals
    densities = get_densities(marginals)

    get_hh_counts(marginals, seed) %>%
        left_join(densities, by="geoid") %>%
        return()
}

#' This generates trip counts by tract and time period, based on marginals and
#' trip regressions. It expects marginals in the format described in the docs for
#' get_hh_counts, and areas with a geoid and area_sqmi column with the area of each
#' geographic unit
get_production_counts = function (marginals, generation_functions, seed) {
    hh_counts = prepare_regression_data(marginals, seed)

    # now, apply each model
    purrr::imap(generation_functions, function (models, period) {
        purrr::imap(models, function (model, ttype) {
            tibble(
                geoid = hh_counts$geoid,
                trip_type = ttype,
                time_period = period,
                # predict trips for one household, multiply by hhs in category
                # TODO: should we do the pmax here, or below when summarizing by tract? Could this be
                # why congestion comes up worse in our model than it actually is?
                n_trips = pmax(predict(model, hh_counts), 0) * hh_counts$count
            ) %>%
            return()
        }) %>%
        list_rbind() %>%
        return()
    }) %>%
    list_rbind() %>%
    group_by(geoid, trip_type, time_period) %>%
    summarize(n_trips=sum(n_trips)) %>%
    return()
}