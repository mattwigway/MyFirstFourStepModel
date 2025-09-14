# This R script will download and run a simple four-step travel demand model for the
# Research Triangle region of North Carolina. It runs in sequence a baseline (current
# conditions) scenario, a scenario with the 22,000 households added to Pittsboro to model
# the Chatham Park development, and a scenario where US 15-501 between Chapel Hill and
# Pittsboro is additionally widened to a three-lane-each-way freeway.

# The model is very simplistic, so should not be used for production forecasting, but all
# of the components are there to allow you to see how they come together to produce a forecast.

# Before running this script, you first need to install the MyFirstFourStepModel package, which you
# can do by running this code in the console (this only needs to be done once per computer you run this on).
# Remove the # signs from the start of the line before running this code:
#
#    install.packages('MyFirstFourStepModel', repos = c('https://mattwigway.r-universe.dev', 'https://cloud.r-project.org'))

# First, we load the modeling package into R
#library(MyFirstFourStepModel)
devtools::load_all()
library(ggplot2)

# Next, we load the model itself
model = load_model("https://files.indicatrix.org/chatham_park.model")

############
# BASELINE #
############

###################
# Trip generation #
###################

# The first step of the four step model is trip generation. First, we will print out a summary of the trip generation model
# for AM Peak home-based work trips. Use this to answer question 1.
summary(model$production_functions$`AM Peak`$HBW)

# Next, we run the actual trip generation step
productions_attractions = trip_generation(model, model$scenarios$baseline)

# And map the trip productions for question 2.
map_trip_generation(
  model,
  productions_attractions,
  "Productions",
  "AM Peak",
  "HBW"
)

# Next, we print out the betas for the different trip types-
# Home-based work (HBW), home-based other (HBO), and non-home-based
# These betas are used in a gravity model.
# This is used for question 3.
model$distribution_betas

# Next, we run the trip distribution step.
flows = trip_distribution(
  model,
  model$scenarios$baseline,
  productions_attractions
)

# And we can map the AM peak HBW flows
# from central Carrboro (Question 4)
map_trip_distribution(model, flows, "AM Peak", "HBW", "37135010705")

# Next, we can print out the mode choice model coefficients for
# Question 5. Note that this prints two tables, one of coefficients
# and one of standard errors.
summary(model$mode_choice_models$HB)

# Next, we can run the mode choice model to divide trips up by mode
flows_by_mode = mode_choice(model, model$scenarios$baseline, flows)

# Based on that, we can calculate mode shares (proportion of trips using
# each mode) (Question 6)
get_mode_shares(flows_by_mode)

# Lastly, we can assign all PM peak trips to the network.
# NOTE: This step may take quite some time, possibly as much as an hour
# depending on your computer.
# Every few minutes it will print a status update, something like
# Iteration 1 relative gap: 0.40825352014596
# When that number falls below 0.01, the algorithm will finish.
link_flows = network_assignment(
    model,
    model$scenarios$baseline,
    model$networks$baseline,
    flows_by_mode,
    "PM Peak"
)

# We can then map the congestion resulting from these trips (Question 7).
map_congestion(model, model$networks$baseline, link_flows)

#########################
# CHATHAM PARK SCENARIO #
#########################

# Next, we will run the model for the Chatham Park scenario,
# by repeating the steps above.
cp_productions_attractions = trip_generation(
    model,
    model$scenarios$chatham_park
)

cp_flows = trip_distribution(
    model,
    model$scenarios$chatham_park,
    cp_productions_attractions
)

cp_flows_by_mode = mode_choice(
    model,
    model$scenarios$chatham_park,
    cp_flows
)

# This step may again take 30 minutes or more
cp_link_flows = network_assignment(
    model,
    model$scenarios$chatham_park,
    model$networks$baseline,
    cp_flows_by_mode,
    "PM Peak"
)

# This maps the congestion under the scenario, and also labels the location of Chatham Park (Question 7)
map_congestion(model, model$networks$baseline, cp_link_flows) +
    ggplot2::annotate("text", -8809915, 4267605, label="Chatham Park")



################
# EXTRA CREDIT #
################

# The code below runs the model for the Chatham Park scenario,
# but with 15-501 widened to a three-lane freeway.

widen_productions_attractions = trip_generation(
    model,
    model$scenarios$chatham_park
)

widen_flows = trip_distribution(
    model,
    model$scenarios$chatham_park,
    widen_productions_attractions
)

widen_flows_by_mode = mode_choice(
    model,
    model$scenarios$chatham_park,
    widen_flows
)

# This step may again take 30 minutes or more
widen_link_flows = network_assignment(
    model,
    model$scenarios$chatham_park,
    model$networks$widen_15_501,
    widen_flows_by_mode,
    "PM Peak"
)

# This maps the congestion under the scenario (Extra credit)
map_congestion(model, model$networks$widen_15_501, widen_link_flows) +
    ggplot2::annotate("text", -8809915, 4267605, label="Chatham Park")
