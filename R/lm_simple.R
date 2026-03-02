#' Write the minimal information to be able to reconstruct enough of an
#' lm to be able to do prediction.
write_lm = function(model, archive, name) {
  abort_on_error(archive$write_entry(paste0(name, ".json"), toJSON(as.list(coefficients(model)), digits = NA)))
  abort_on_error(archive$write_entry(paste0(name, ".txt"), capture_output(print(summary(model)))))
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
    coefficients = fromJSON(abort_on_error(archive$get_entry_as_string(paste0(name, ".json")))),
    summary = abort_on_error(archive$get_entry_as_string(paste0(name, ".txt")))
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
