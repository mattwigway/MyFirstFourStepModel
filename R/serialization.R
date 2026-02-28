#' Write the minimal information to be able to reconstruct enough of an
#' lm to be able to do estimation.
write_lm = function(model, archive, name) {
  archive$write_entry(paste0(name, ".json"), toJSON(as.list(coefficients(model))))
  archive$write_entry(paste0(name, ".txt"), capture_output(print(summary(model))))
}

write_lm_list = function(models, archive, prefix) {
  for (name in names(models)) {
    model = models[[name]]
    if (class(model) == "lm") {
      write_lm(model, archive, paste0(prefix, "/", name))
    } else if (class(model) == "list") {
      write_lm_list(model, archive, paste0(prefix, "/", name))
    }
  }
}

read_lm = function(archive, name) {
  result = list(
    coefficients = fromJSON(archive$get_entry_as_string(paste0(name, ".json"))),
    summary = archive$get_entry_as_string(paste0(name, ".txt"))
  )

  class(result) = "lm_simple"

  return(result)
}

#' @method summary lm_simple
#' @export
summary.lm_simple = function(model, ...) {
  cat(model$summary[[1]])
}

#' @method coefficients lm_simple
#' @export
coefficients.lm_simple = function(model, ...) {
  unlist(model$coefficients)
}

#' @method predict lm_simple
#' @export
predict.lm_simple = function(model, data, ...) {
  pred = rep(0.0, nrow(data))

  for (name in names(model$coefficients)) {
    if (str_detect(name, "^factor")) {
      var = str_extract(name, "factor\\(([^\\)]+)\\)", 1)
      val = trimws(str_extract(name, "factor\\(.*\\)(.*)", 1))

      if (is.numeric(data[[var]])) {
        val = as.numeric(val)
        col = data[, var]
      } else {
        col = trimws(data[, var])
      }

      if (!(var %in% names(data))) {
        stop(paste("Column", var, "not found in data"))
      }
      if (!any(col == val)) {
        warning(paste("Column", var, "has no value", val, "(used in regression)"))
      }

      pred = pred + (col == val) * model$coefficients[[name]]
    } else if (name == "(Intercept)") {
      pred = pred + model$coefficients[[name]]
    } else {
      if (!(name %in% names(data))) {
        stop(paste("Column", name, "not found in data"))
      }
      pred = pred + model$coefficients[[name]] * data[[name]]
    }
  }

  return(unname(pred))
}

write_mnl = function(model, archive, name) {
  simple = list(
    summary = capture_output(print(summary(model))),
    coef = as.data.frame(coefficients(model)),
    lev = model$lev
  )

  archive$write_entry(paste0(name, ".json"), toJSON(simple))
}

read_mnl = function(archive, name) {
  mod = fromJSON(archive$get_entry_as_string(paste0(name, ".json")))
  mod$coef = as.matrix(mod$coef)
  class(mod) = "mnl_simple"
  return(mod)
}

#' @method summary mnl_simple
#' @export
summary.mnl_simple = function(model, ...) {
  cat(model$summary[[1]])
}

#' @method coefficients mnl_simple
#' @export
coefficients.mnl_simple = function(model, ...) {
  model$coef
}

#' @method predict mnl_simple
#' @export
predict.mnl_simple = function(model, data, type, ...) {
  if (type == "utils") {
    result = matrix(0.0, nrow(data), length(model$lev))
    colnames(result) = model$lev

    for (name in colnames(model$coef)) {
      if (str_detect(name, "^factor")) {
        var = str_extract(name, "factor\\(([^\\)]+)\\)", 1)
        val = trimws(str_extract(name, "factor\\(.*\\)(.*)", 1))

        if (is.numeric(data[[var]])) {
          val = as.numeric(val)
        }

        # cannot set whole row at once because there is a level
        # not in the coefficients (reference level)
        for (outcome in rownames(model$coef)) {
          result[, outcome] = result[, outcome] + (data[[var]] == val) * model$coef[[outcome, name]]
        }
      } else if (name == "(Intercept)") {
        for (outcome in rownames(model$coef)) {
          result[, outcome] = result[, outcome] + model$coef[[outcome, name]]
        }
      } else {
        for (outcome in rownames(model$coef)) {
          result[, outcome] = result[, outcome] + model$coef[[outcome, name]] * data[[name]]
        }
      }
    }
    return(result)
  } else if (type == "probs") {
    util = predict.mnl_simple(model, data, "utils", ...)
    exputil = exp(util)
    exputil / rowSums(exputil)
  }
}

write_sf_to_model = function(layer, archive, name) {
  tf = tempfile(fileext = ".gpkg")
  st_write(layer, tf)
  archive$write_file(paste0(name, ".gpkg"), tf)
  file.remove(tf)
}

read_sf_from_model = function(archive, name) {
  tf = tempfile(fileext = ".gpkg")
  archive$extract_entry(paste0(name, ".gpkg"), tf)
  val = st_read(tf)
  file.remove(tf)
  return(val)
}

write_network = function(network, archive, name) {
  # okay to use same name for both, extensions differ
  write_sf_to_model(network$network_geo, archive, paste0("networks/", name))

  tf = tempfile(fileext = ".graphml")
  write_graph(network$network, tf, format = "graphml")
  archive$write_file(paste0("networks/", name, ".graphml"), tf)
  file.remove(tf)
}

read_network = function(archive, name) {
  # okay to use same name for both, extensions differ
  geo = read_sf_from_model(archive, paste0("networks/", name))
  tf = tempfile(fileext = ".graphml")
  graphml = archive$extract_entry(paste0("networks/", name, ".graphml"), tf)
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
    if (!str_detect(netname, "^[a-zA-Z0-9_\\.]+$")) {
      stop("network names must only contain a-z, A-Z, 0-9, _, and .")
    }
    write_network(model$networks[[netname]], out, netname)
  }

  out$finish()
}

#' Load a model
#' @export
load_model = function(filename) {
  # http/https handled on Rust side
  inp = ArchiveReader$new(filename)
  res = list()

  res$seed_matrix = read_csv(inp$get_entry_as_string("seed_matrix.csv"))
  res$distribution_betas = fromJSON(inp$get_entry_as_string("distribution_betas.json"))
  res$direction_factors = read_csv(inp$get_entry_as_string("direction_factors.csv"))
  res$occupancy_factors = read_csv(inp$get_entry_as_string("occupancy_factors.csv"))

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

  res$scenarios = fromJSON(inp$get_entry_as_string("scenarios.json"))

  networks = inp$entries()
  networks = str_extract(networks, "networks/([a-zA-Z0-9_\\.]+).gpkg", 1)
  networks = networks[!is.na(networks)]

  res$networks = list()

  for (network in networks) {
    res$networks[[network]] = read_network(inp, network)
  }

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
