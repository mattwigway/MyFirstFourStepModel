# Install the Baby's First Four Step Model package (only needed once per machine)
install.packages("devtools")
devtools::install_github("mattwigway/BabysFirstFourStepModel")

# Load the Baby's First Four Step Model package, and the Research Triangle model
library(bf4sm)
model = load_model("https://files.indicatrix.org/rdu.model")

# Print one of the trip production regressions
summary(model$production_functions$`AM Peak`$HBW)

# Run trip generation
trip_ends = trip_generation(model, model$scenarios$baseline)

# Map results
map_trip_generation(model, trip_ends, "Productions", "AM Peak", "HBW")
map_trip_generation(model, trip_ends, "Attractions", "AM Peak", "HBW")

# Run trip distribution
flows = trip_distribution(model, model$scenarios$baseline, trip_ends)

# Map results
map_trip_distribution(model, flows, "AM Peak", "HBW", origin_tract="37135010705")

# Print one of the mode choice models
summary(model$mode_choice_models$HB)

# Run the mode choice model and get mode shares
flows_by_mode = mode_choice(model, model$scenarios$baseline, flows)
get_mode_shares(flows_by_mode)

# Run traffic assignment and map the results
pm_network_flows = network_assignment(model, model$scenarios$baseline, flows_by_mode, "PM Peak")
map_congestion(pm_network_flows, model)