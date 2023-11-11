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
get_trip_counts <- function (trips) {
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
    hh_trip_counts = get_trip_counts(nhts$trips)

    result = list()

    for (tp in unique(hh_trip_counts$time_period)) {
        result[[tp]] = list()
        for (tt in unique(hh_trip_counts$trip_type)) {
            regdata = nhts$households %>%
                left_join(filter(hh_trip_counts, trip_type == tt & time_period == tp)) %>%
                mutate(trip_count = replace_na(trip_count, 0)) %>%
                mutate(HHFAMINC=factor(HHFAMINC)) # income as dummy variable (todo put cats together?)

            if (all(regdata$trip_count == 0)) error(paste("No trips for", tt, tp))

            result[[tp]][[tt]] = lm(trip_count ~ HHVEHCNT + HHSIZE + HHFAMINC + HTRESDN + WRKCOUNT, regdata)
        }
    }

    return(result)
}

