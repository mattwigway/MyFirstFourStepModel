# This file contains the notebook interface to the four step model
# True to form, there are four functions here to use in the notebook, which correspond
# to the four steps.
#
#  - trip_generation
#  - trip_distribution
#  - mode_choice
#  - network_assignment
#
# The model object is a named list that contains the following:
#   production_functions - trip production models
#   attraction_function - trip attraction models
#   distribution_betas - list containing gravity model betas for HBO, HBW, and NHB
#   mode_choice_models - list containing mode choice models for HB and NHB
#   direction_factors - tibble with "direction factors" -
#      how many trips in each direction to expect at different time period for HB trips
#   network - an igraph graph object representing the road network
#   network_geo - an sf object representing the road network (for plotting)
# 
# There is additionally a function estimate(), which will create the model object


#' Estimate a four step model for later use
estimate = function (nhts, seed_matrix, psrc, psrc_lodes, base_marginals, network, network_geo) {
    production_functions = estimate_production_functions(nhts)
    attraction_functions = estimate_attraction_functions(psrc, psrc_lodes)
    median_distances = estimate_median_crow_flies_distance(nhts)
    mode_choice_models = estimate_mode_choice_model(nhts)
    direction_factors = calculate_direction_factors(nhts)

    # calibrating the betas is harder, as we need to run the generation and distribution steps
    # before we can calibrate
    productions = get_production_counts(base_marginals, production_functions, seed_matrix)
    attractions = get_attraction_counts(base_marginals, attraction_functions)
    balanced = balance_production_attraction(productions, attractions)

    distribution_betas = calibrate_trip_distance_betas(balanced, base_marginals, median_distances)

    return(list(
        production_functions=production_functions,
        attraction_functions=attraction_functions,
        seed_matrix=seed_matrix,
        distribution_betas=distribution_betas,
        mode_choice_models=mode_choice_models,
        direction_factors=direction_factors,
        network=network,
        network_geo=network_geo
    ))
}

#' Save a model as an RData file
save_model = function (model, filename) {
    saveRDS(model, filename)
}

#' Load a model from an RData file
load_model = function (filename) {
    return(readRDS(filename))
}

#' This runs the trip generation step of the model.
#' 
#' Trip production is based on the marginal distributions for vehicles, workers, household size, and 
#' income. Trip attraction is based on job counts. Attractions are balanced to match productions before
#' being returned.
trip_generation = function (model, marginals) {
    productions = get_production_counts(marginals, model$production_functions, model$seed_matrix)
    attractions = get_attraction_counts(marginals, model$attraction_functions)
    return(balance_production_attraction(productions, attractions))
}

#' This runs the trip distribution step of the model
#' 
#' Trip distribution is based on the relative locations of the tracts (in `marginals`),
#' the betas estimated during the estimation phase of the model, and the total trip productions
#' and attractions.
trip_distribution = function (model, marginals, balanced) {
    return(get_flows(balanced, marginals, model$distribution_betas))
}

#' This runs the mode choice step of the model, and returns flows differentiated
#' by mode for each trip type and time of day.
mode_choice = function (model, marginals, flows) {
    return(flow_by_mode(flows, marginals, model$mode_choice_models))
}

#' This runs the network assignment step of the model.
#' 
#' Network assignment is based on a Frank-Wolfe static traffic assignment algorithm.
#' This returns a list of link-level flows.
#' Period can be "AM Peak", "Midday", "PM Peak", "Overnight".
network_assignment = function (model, marginals, mode_flows, period) {
    hourly_flows = mode_flows %>%
        filter(time_period == period) %>%
        mutate(across(c("Car", "Transit", "Walk", "Bike"), \(x) x * PEAKING_FACTORS[[period]]))

    marginals = link_tracts(model$network, marginals)

    return(frank_wolfe(hourly_flows, marginals, model$network))
}
