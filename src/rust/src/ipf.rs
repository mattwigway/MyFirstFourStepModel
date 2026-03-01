use extendr_api::prelude::*;
use ndarray::ArrayView2;

#[extendr]
fn do_ipf(orig_counts: &[f64], marginal_values: ArrayView2<i32>, target_marginals: &[i32], target_values: &[i32], target_counts: &[f64]) -> Result<Vec<f64>> {
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

            let scale_factor = count / current_sum;

            //println!("Current sum: {} Target: {} Scale factor: {}", current_sum, count, scale_factor);

            if scale_factor < 0.999 || scale_factor > 1.001 {
                // not converged in this dimension
                updated += 1;
                for hh in 0..counts.len() {
                    if marginal_values[[hh, marginal - 1]] == value {
                        counts[hh] = counts[hh] * scale_factor;
                    }
                }
            }
        }

        if updated == 0 {
            break;
        }

        //println!("{} updated", updated);
    }

    Ok(counts)
}

extendr_module! {
    mod ipf;
    fn do_ipf;
}