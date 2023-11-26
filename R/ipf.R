# We implement IPF on our own in base R, because it's too slow in ipfr/tidyverse
ipf = function(seed, marginals) {
    margins = unique(marginals$marginal)

    # create a copy of the seed matrix, and scale to match the total marginals
    total_marginals = sum(marginals[marginals$marginal == margins[[1]], "count"])
    result = ungroup(seed) %>%
        mutate(weight=weight * total_marginals / sum(weight))

    while (T) {
        for (margin in margins) {
            for (value in unique(marginals$value[marginals$marginal == margin])) {
                mask = result[,margin] == value
                total_expected = sum(marginals[marginals$marginal == margin & marginals$value == value, "count"])
                total_now = sum(result[mask,"weight"])

                if (!(total_expected < 1e-6 & total_now < 1e-6)) {
                    # don't divide by zero if the expected marginal is zero
                    result[mask, "weight"] = result[mask, "weight"] * total_expected / total_now
                }
            }
        }

        # check for convergence
        max_diff = 0.0
        for (margin in margins) {
            for (value in unique(marginals$value[marginals$marginal == margin])) {
                mask = result[,margin] == value
                total_expected = sum(marginals[marginals$marginal == margin & marginals$value == value, "count"])
                total_now = sum(result[mask,"weight"])
                max_diff = max(max_diff, abs(total_expected - total_now))
            }
        }

        if (max_diff < 0.01) {
            return(result)
        }
    }
}