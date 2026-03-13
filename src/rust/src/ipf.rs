use extendr_api::prelude::*;
use ndarray::ArrayView2;

/// Internal IPF code, similar to what was previously done in R. For efficiency, marginal names and values
/// are converted to integers before calling.
///
/// @param orig_counts numeric vector with number of each household sampled in seed matrix
/// @param marginal_values integer matrix with one row per household and one column per marginal, indicating which value that
///   household has for each marginal.
/// @param target_marginals This along with target_values and target_counts indicate what the target values for each marginal should be.
//     target_marginals contains the marginal identifier (from 1 to number of marginals)
/// @param target_values The value of this marginal (e.g. for income, might be 2 to represent $25-50,000)
/// @param target_counts The number of households expected with this value.
/// @param convergence The largest error in absolute terms that can be tolerated, e.g. 0.01 -> errors of no more than 0.02 hhs on any marginal.
///
/// @keywords internal
#[extendr]
fn do_ipf(orig_counts: &[f64], marginal_values: ArrayView2<i32>, target_marginals: &[i32], target_values: &[i32], target_counts: &[f64], convergence: f64) -> Result<Vec<f64>> {
    if orig_counts.len() != marginal_values.dim().0 {
        return Err(Error::Other("{.arg orig_counts} and {.arg marginal_values} should have same length".to_string()));
    }

    match target_marginals.into_iter().max() {
        Some(v) => {
            if *v as usize != marginal_values.dim().1 {
                return Err(Error::Other("width of {.arg marginal_values} should equal max of {.target_marginals}".to_string()));
            }
        },
        None => return Err(Error::Other("{.arg marginal_values} should not be empty".to_string()))
    };

    if target_marginals.len() != target_values.len() || target_marginals.len() != target_counts.len() {
        return Err(Error::Other("{.arg target_marginals}, {.arg target_values}, and {.arg target_counts} should all have the same length".to_string()));
    }

    let mut counts: Vec<f64> = orig_counts.into();

    loop {
        let mut updated = 0;

        for marginalix in 0..target_marginals.len() {
            let marginal = target_marginals[marginalix] as usize;
            let value = target_values[marginalix];
            let count = target_counts[marginalix];
            let mut current_sum = 0.0;

            for hh in 0..counts.len() {
                if marginal_values[[hh, marginal - 1]] == value {
                    current_sum += counts[hh];
                }
            }

            if current_sum < 1e-6 && count > 1e-5 {
                return Err(Error::Other("Marginal totals to zero; IPF cannot recover".to_string()));
            }
            
            // don't divide by zero
            // TODO will this cause an infinite loop if there are zero-valued rows
            if current_sum > 1e-6 && (current_sum - count).abs() > convergence {
                updated += 1;

                let scale_factor = count / current_sum;

                //println!("Current sum: {} Target: {} Scale factor: {}", current_sum, count, scale_factor);
                for hh in 0..counts.len() {
                    if marginal_values[[hh, marginal - 1]] == value {
                        counts[hh] = counts[hh] * scale_factor;
                    }
                }
            }
        }

        // We can get away without a separate check for convergence because updated == 0 implies we didn't
        // change anything this time through, so all of the checks for convergence of individual
        // marginals are still accurate - the weights have not changed since any were checked.
        if updated == 0 {
            break;
        }
    }

    Ok(counts)
}

extendr_module! {
    mod ipf;
    fn do_ipf;
}