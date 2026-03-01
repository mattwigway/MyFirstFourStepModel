# We implement IPF on our own in base R, because it's too slow in ipfr/tidyverse
ipf = function(seed, marginals) {
  # all of this just converts from text marginals to each marginal and value having a number
  margin_names = unique(marginals$marginal)
  marginals$marginal_number = 0L
  marginals$marginal_value = 0L

  # what value each household has for each marginal
  hh_marginal_values = matrix(0L, nrow(seed), length(margin_names))

  for (margin_name in margin_names) {
    margin_number = which.max(margin_names == margin_name)

    marginals[marginals$marginal == margin_name, "marginal_number"] = margin_number

    margin_values = unique(marginals[marginals$marginal_number == margin_number, "value"][[1]])
    for (margin_value in margin_values) {
      margin_val_num = which.max(margin_values == margin_value)
      marginals[marginals$marginal == margin_name & marginals$value == margin_value, "marginal_value"] = margin_val_num
    }

    hh_marginal_values[, margin_number] = vapply(seed[[margin_name]], \(x) which.max(x == margin_values), 1)
  }

  # make sure we do one whole dimension before moving to another
  marginals = arrange(marginals, marginal_number, marginal_value)

  mode(hh_marginal_values) = "integer"

  result = seed |>
    mutate(
      weight = abort_on_error(do_ipf(
        weight,
        hh_marginal_values,
        marginals$marginal_number,
        marginals$marginal_value,
        marginals$count
      ))
    )

  return(result)
}
