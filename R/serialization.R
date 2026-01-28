
#' Write the minimal information to be able to reconstruct enough of an
#' lm to be able to do estimation.
write_lm = function (model, archive, name) {
    archive$write_entry(paste0(name, ".json"), toJSON(as.list(coefficients(model))))
    archive$write_entry(paste0(name, ".txt"), capture_output(print(summary(model))))
}

write_lm_list = function (models, archive, prefix) {
    for (name in names(models)) {
        model = models[[name]]
        if (class(model) == "lm") {
            write_lm(model, archive, paste0(prefix, "/", name))
        } else if (class(model) == "list") {
            write_lm_list (model, archive, paste0(prefix, "/", name))
        }
    }
}

read_lm = function (archive, name) {
    result = list(
        coefficients = fromJSON(archive$get_entry_as_string(paste0(name, ".json"))),
        summary = archive$get_entry_as_string(paste0(name, ".txt"))
    )
    
    class(result) = "lm_simple"

    return(result)
}

#' @method summary lm_simple
#' @export 
summary.lm_simple = function (model, ...) {
    cat(model$summary[[1]])
}

write_mnl = function (model, archive, name) {
    simple = list(
        summary = capture_output(print(summary(model))),
        coef = coefficients(model)
    )

    archive$write_entry(paste0(name, ".json"), toJSON(simple))
}

read_mnl = function (archive, name) {
    mod = fromJSON(archive$get_entry_as_string(paste0(name, ".json")))
    class(mod) = "mnl_simple"
    return(mod)
}

#' @method summary mnl_simple
#' @export 
summary.mnl_simple = function (model, ...) {
    cat(model$summary[[1]])
}

write_sf_to_model = function (layer, archive, name) {
    tf = tempfile(fileext = ".gpkg")
    st_write(layer, tf)
    archive$write_file(paste0(name, ".gpkg"), tf)
    file.remove(tf)
}

read_sf_from_model = function (archive, name) {
    tf = tempfile(fileext = ".gpkg")
    archive$extract_entry(paste0(name, ".gpkg"), tf)
    val = st_read(tf)
    file.remove(tf)
    return(val)
}

write_network = function (network, archive, name) {
    # okay to use same name for both, extensions differ
    write_sf_to_model(network$network_geo, archive, paste0("networks/", name))

    tf = tempfile(fileext = ".graphml")
    write_graph(network$network, tf, format = "graphml")
    archive$write_file(paste0("networks/", name, ".graphml"), tf)
    file.remove(tf)
}

read_network = function (archive, name) {
    # okay to use same name for both, extensions differ
    geo = read_sf_from_model(archive, paste0("networks/", name))
    tf = tempfile(fileext = ".graphml")
    graphml = archive$extract_entry(paste0("networks/", name, ".graphml"), tf)
    net = read_graph(tf, format="graphml")
    file.remove(tf)
    
    return(list(
        network = net,
        network_geo = geo
    ))
}


#' Save a model
#' @export
save_model = function (model, filename) {
    out = ArchiveWriter$new(filename)

    out$write_entry("seed_matrix.csv", format_csv(model$seed_matrix))
    out$write_entry("distribution_betas.json", toJSON(model$distribution_betas))
    out$write_entry("direction_factors.csv", format_csv(model$direction_factors))
    out$write_entry("occupancy_factors.csv", format_csv(model$occupancy_factors))

    write_lm_list(model$production_functions, out, "production_functions")
    write_lm_list(model$attraction_functions, out, "attraction_functions")

    write_sf_to_model(model$tazs, out, "tazs")

    write_mnl(model$mode_choice_models$NHB, out, "mode_choice_models/NHB")
    write_mnl(model$mode_choice_models$HB, out, "mode_choice_models/HB")

    out$write_entry("scenarios.json", toJSON(model$scenarios))

    for (netname in names(model$networks)) {
        if (!str_detect(netname, "^[a-zA-Z0-9_\\.]+$")) stop("network names must only contain a-z, A-Z, 0-9, _, and .")
        write_network(model$networks[[netname]], out, netname)
    }

    out$finish()
}

#' Load a model
#' @export
load_model = function (filename) {
    # http/https handled on Rust side
    inp = ArchiveReader$new(filename)
    res = list()

    res$seed_matrix = read_csv(inp$get_entry_as_string("seed_matrix.csv"))
    res$distribution_betas = fromJSON(inp$get_entry_as_string("distribution_betas.json"))
    res$direction_factors = read_csv(inp$get_entry_as_string("direction_factors.csv"))
    res$occupancy_factors = read_csv(inp$get_entry_as_string("occupancy_factors.csv"))

    res$tazs_ges = read_sf_from_model(inp, "tazs")

    res$production_functions = list()
    res$attraction_functions = list()

    for (time_period in c("AM Peak", "Midday", "PM Peak", "Overnight")) {
        for (purpose in c("HBW", "HBO", "NHB")) {
            res$production_functions[[time_period]][[purpose]] =
                read_lm(inp, paste("production_functions", time_period, purpose, sep="/"))

            res$attraction_functions[[time_period]][[purpose]] =
                read_lm(inp, paste("attraction_functions", time_period, purpose, sep="/"))
        }
    }

    res$mode_choice_models = list(
        HB = read_mnl(inp, "mode_choice_models/HB"),
        HB = read_mnl(inp, "mode_choice_models/NHB")
    )

    res$scenarios = fromJSON(inp$get_entry_as_string("scenarios.json"))

    networks = inp$entries()
    networks = str_extract(networks, "networks/([a-zA-Z0-9_\\.]+).gpkg", 1)
    networks = networks[!is.na(networks)]

    res$networks = list()

    for (network in networks) {
        print(network)
        res$networks[[network]] = read_network(inp, network)
    }

    return(res)
}