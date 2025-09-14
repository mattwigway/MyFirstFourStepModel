use extendr_api::prelude::*;

/// This function takes the output from igraph and updates the flows using it. Doing this in R requires too many levels
/// of indirection and is really slow, so we hand this little piece off to Rust (though this does require a complation step
/// so may ultimately not be worth it).
/// @export
#[extendr]
fn update_flows(edge_flows: &mut[f64], flows_to_node: &[f64], predecessor: &[i32], incoming_edge: &[i32], origin: i32) -> () {
    for (index, flow) in flows_to_node.iter().enumerate() {
        if (*flow > 0.0) {
            // index is zero based, everything else is one-based
            let mut current_node = index + 1;
            // follow back to origin
            while current_node != origin as usize {
                // both current_node and incoming_edge are one-based, convert to zero-based as needed
                edge_flows[incoming_edge[current_node as usize - 1] as usize - 1] += *flow;
                current_node = predecessor[current_node as usize - 1] as usize;
            }
        }
    }
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod MyFirstFourStepModel;
    fn update_flows;
}
