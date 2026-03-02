write_sf_to_model = function(layer, archive, name) {
  tf = tempfile(fileext = ".gpkg")
  st_write(layer, tf)
  abort_on_error(archive$write_file(paste0(name, ".gpkg"), tf))
  file.remove(tf)
}

read_sf_from_model = function(archive, name) {
  tf = tempfile(fileext = ".gpkg")
  abort_on_error(archive$extract_entry(paste0(name, ".gpkg"), tf))
  val = st_read(tf)
  file.remove(tf)
  return(val)
}

write_network = function(network, archive, name) {
  # okay to use same name for both, extensions differ
  write_sf_to_model(network$network_geo, archive, paste0("networks/", name))

  tf = tempfile(fileext = ".graphml")
  write_graph(network$network, tf, format = "graphml")
  abort_on_error(archive$write_file(paste0("networks/", name, ".graphml"), tf))
  file.remove(tf)
}

read_network = function(archive, name) {
  # okay to use same name for both, extensions differ
  geo = read_sf_from_model(archive, paste0("networks/", name))
  tf = tempfile(fileext = ".graphml")
  graphml = abort_on_error(archive$extract_entry(paste0("networks/", name, ".graphml"), tf))
  net = read_graph(tf, format = "graphml")
  file.remove(tf)

  return(list(
    network = net,
    network_geo = geo
  ))
}


#' Save a model
#' @export
save_model = function(model, filename) {
  invisible(capture.output({
    out = abort_on_error(ArchiveWriter$new(filename))

    abort_on_error(out$write_entry("seed_matrix.csv", format_csv(model$seed_matrix)))
    abort_on_error(out$write_entry("distribution_betas.json", toJSON(model$distribution_betas, digits = NA)))
    abort_on_error(out$write_entry("direction_factors.csv", format_csv(model$direction_factors)))
    abort_on_error(out$write_entry("occupancy_factors.csv", format_csv(model$occupancy_factors)))

    write_lm_list(model$production_functions, out, "production_functions")
    write_lm_list(model$attraction_functions, out, "attraction_functions")

    write_sf_to_model(model$tazs, out, "tazs")

    write_mnl(model$mode_choice_models$NHB, out, "mode_choice_models/NHB")
    write_mnl(model$mode_choice_models$HB, out, "mode_choice_models/HB")

    abort_on_error(out$write_entry("scenarios.json", toJSON(model$scenarios, digits = NA)))

    for (netname in names(model$networks)) {
      if (!str_detect(netname, "^[a-zA-Z0-9_\\.]+$")) {
        stop("network names must only contain a-z, A-Z, 0-9, _, and .")
      }
      write_network(model$networks[[netname]], out, netname)
    }

    abort_on_error(out$finish())
  }))
}

#' Load a model
#' @export
load_model = function(filename) {
  capture.output(suppressWarnings({
    # http/https handled on Rust side
    inp = abort_on_error(ArchiveReader$new(filename))
    res = list()

    res$seed_matrix = read_csv(abort_on_error(inp$get_entry_as_string("seed_matrix.csv")))
    res$distribution_betas = fromJSON(abort_on_error(inp$get_entry_as_string("distribution_betas.json")))
    res$direction_factors = read_csv(abort_on_error(inp$get_entry_as_string("direction_factors.csv")))
    res$occupancy_factors = read_csv(abort_on_error(inp$get_entry_as_string("occupancy_factors.csv")))

    res$tazs_geo = read_sf_from_model(inp, "tazs")

    res$production_functions = list()
    res$attraction_functions = list()

    for (time_period in c("AM Peak", "Midday", "PM Peak", "Overnight")) {
      for (purpose in c("HBW", "HBO", "NHB")) {
        res$production_functions[[time_period]][[purpose]] =
          read_lm(inp, paste("production_functions", time_period, purpose, sep = "/"))

        res$attraction_functions[[time_period]][[purpose]] =
          read_lm(inp, paste("attraction_functions", time_period, purpose, sep = "/"))
      }
    }

    res$mode_choice_models = list(
      HB = read_mnl(inp, "mode_choice_models/HB"),
      NHB = read_mnl(inp, "mode_choice_models/NHB")
    )

    res$scenarios = fromJSON(abort_on_error(inp$get_entry_as_string("scenarios.json")))

    networks = abort_on_error(inp$entries())
    networks = str_extract(networks, "networks/([a-zA-Z0-9_\\.]+).gpkg", 1)
    networks = networks[!is.na(networks)]

    res$networks = list()

    for (network in networks) {
      res$networks[[network]] = read_network(inp, network)
    }
  }))

  return(res)
}

#' Load a model in model format 1
load_model_v1 = function(filename) {
  if (str_starts(filename, "https://")) {
    return(readRDS(gzcon(url(filename))))
  } else {
    return(readRDS(filename))
  }
}
