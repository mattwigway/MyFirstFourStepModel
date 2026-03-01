use core::slice;
use std::iter::zip;

use extendr_api::prelude::*;
use ndarray::{Array, Array1, ArrayView, ArrayView1, Axis, Dimension, RemoveAxis};

#[extendr]
fn ipf(mtx: Robj, shape: Robj, marginals:Robj) -> Result<Array1<f64>> {

    
    let marginallist = match marginals.as_list() {
        Some(l) => Ok(l),
        None => Err(Error::Other("{.arg marginals} must be a list".to_string()))
    }?;

    let marginalvec  = marginallist
        .iter()
        .map(|m| match m.1.as_real_vector() {
            Some(v) => Ok(v),
            None => {return Err(Error::Other("{.arg marginals} must contain numeric vectors".to_string()))}
        })
        .collect::<Result<Vec<Vec<f64>>>>()?;

    if marginalvec.len() != mtx.ndim() {
        return Err(Error::Other("{.arg marginals} must have the same length as the number of dimensions in {.mtx}".to_string()));
    }

    let mut result = mtx.clone();

    // core algorithm
    loop {
        let mut updated = 0;

        for ax in 1..marginalvec.len() {
            let current = result.sum_axis(Axis(ax));
            let scale_factors: Vec<f64> = zip(&marginalvec[ax], &current).map(|(x, y)| x / y).collect();

            if !scale_factors.iter().all(|x| *x > 0.999 && *x < 1.001) {
                // not converged in this axis
                updated += 1;

                for i in 1..scale_factors.len() {
                    let mut sl = result.slice_axis_mut(Axis(ax), (i..=i).into());
                    sl *= scale_factors[i];
                }
            }
        }

        if updated == 0 {
            break;
        }
    }

    Ok(result)
}