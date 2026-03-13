#' Write just enough of a multinomial logit model that we can deserialize and apply it
write_mnl = function(model, archive, name) {
  simple = list(
    summary = capture_output(print(summary(model))),
    coef = as.data.frame(coefficients(model)),
    lev = model$lev
  )

  abort_on_error(archive$write_entry(paste0(name, ".json"), toJSON(simple, digits = NA)))
}

read_mnl = function(archive, name) {
  mod = fromJSON(abort_on_error(archive$get_entry_as_string(paste0(name, ".json"))))
  mod$coef = as.matrix(mod$coef)
  class(mod) = "mnl_simple"
  return(mod)
}

#' @method summary mnl_simple
#' @export
summary.mnl_simple = function(object, ...) {
  cat(object$summary[[1]])
}

#' @method coefficients mnl_simple
#' @export
coefficients.mnl_simple = function(object, ...) {
  object$coef
}

#' @method predict mnl_simple
#' @export
predict.mnl_simple = function(object, data, type, ...) {
  if (type == "utils") {
    result = matrix(0.0, nrow(data), length(object$lev))
    colnames(result) = object$lev

    for (name in colnames(object$coef)) {
      if (str_detect(name, "^factor")) {
        var = str_extract(name, "factor\\(([^\\)]+)\\)", 1)
        val = trimws(str_extract(name, "factor\\(.*\\)(.*)", 1))

        if (is.numeric(data[[var]])) {
          val = as.numeric(val)
        }

        # cannot set whole row at once because there is a level
        # not in the coefficients (reference level)
        for (outcome in rownames(object$coef)) {
          result[, outcome] = result[, outcome] + (data[[var]] == val) * object$coef[[outcome, name]]
        }
      } else if (name == "(Intercept)") {
        for (outcome in rownames(object$coef)) {
          result[, outcome] = result[, outcome] + object$coef[[outcome, name]]
        }
      } else {
        for (outcome in rownames(object$coef)) {
          result[, outcome] = result[, outcome] + object$coef[[outcome, name]] * data[[name]]
        }
      }
    }
    return(result)
  } else if (type == "probs") {
    util = predict.mnl_simple(object, data, "utils", ...)
    exputil = exp(util)
    exputil / rowSums(exputil)
  }
}
