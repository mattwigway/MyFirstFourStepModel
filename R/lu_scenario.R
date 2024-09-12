#' Add households to particular Census tracts in the region
#'
#' @param scenario An existing land-use scenario 
#' @param tract a Census tract ID (or vector of tract IDs)-should be strings, not numbers
#' @param hhs Households to add to the tract
#' @returns a copy of the scenario with the requested households added
#' 
#' The hhs parameter should be a table/tibble/data.frame of households with the following columns:
#'   - income: Annual household income, dollars
#'   - hhsize: Household size
#'   - vehicles: Number of vehicles owned by the household
#'   - workers: Number of workers in the household
#'   - n: Number of households like this to add
#' 
#' If there are multiple tracts specified, the households will be evenly split among them
#' 
#' @export
add_households = function (scenario, tract, hhs) {
    if (is.vector(tract)) {
        hhs = hhs %>%
            mutate(n = n / length(tract))
    } else {
        # after this we can treat it as a vector always
        tract = c(tract)
    }

    stopifnot(all(tract %in% scenario$marginals$geoid))

    # Compute the new marginals in each tract
    new_tract_marginals = hhs %>%
        # topcode and bin to match model marginals
        mutate(
            hhsize = pmin(hhsize, HHSIZE_TOPCODE),
            vehicles = pmin(vehicles, VEHICLES_TOPCODE),
            workers = pmin(workers, WORKER_TOPCODE),
            income = case_when(
                income < 35000 ~ 0,
                income < 75000 ~ 35000,
                income < 100000 ~ 75000,
                income >= 100000 ~ 100000,
                .default = NA
            )
        ) %>%
        # convert to marginals
        pivot_longer(all_of(c("income", "vehicles", "workers", "hhsize")),
            names_to="marginal", values_to="value")

    # Expand marginals out to all tracts
    new_marginals = map(tract, function (tr) {
            return(mutate(new_tract_marginals, geoid=tr))
        }) %>% list_rbind() %>%
        # ensure only one record per marginal
        group_by(geoid, marginal, value) %>%
        summarize(n = sum(n)) %>%
        ungroup()

    result = scenario$marginals %>%
        left_join(rename(new_marginals, addl_n="n"), by=c("geoid", "marginal", "value")) %>%
        # not modifying MOEs, assume no error (MOEs not used in model anyways)
        mutate(count = count + replace_na(addl_n, 0)) %>%
        select(-addl_n)

    return(list(
        marginals=result,
        jobs=scenario$jobs,
        areas=scenario$areas
    ))    
}