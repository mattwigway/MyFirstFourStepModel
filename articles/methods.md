# Methods and input data

## Trip generation

Trip generation is based on the National Household Travel Survey. The
model uses four time periods: overnight (7:00 pm–5:59 am), AM Peak (6:00
am–9:59 am), midday (10:00 am–3:59 pm), and PM Peak (4:00 pm–6:59 pm).
The model divides trips into three purposes: home-based work (HBW),
home-based other (HBO), and non-home-based (NHB). While this is fewer
trip types and time periods than might be included in production travel
models, it is consistent with the general practice of dividing trips by
time of day and type.

Household-level trip counts are estimated for each time period and trip
type using a simple linear regression with the trip count as the
dependent variable and independent variables for number of vehicles,
household size, household income, Census tract residential density, and
number of workers. This results in 12 regression equations, for each
time period and trip purpose. Household income is represented by dummy
variables for less than \$35,000, \$35,000–\$74,999, \$75,000–\$99,999,
and \$100,000 or more.

These regression models are disaggregate, household-level models,
whereas the four-step model is an aggregate model using marginal data at
the TAZ level (for simplicity, TAZs correspond directly to Census
tracts). Specifically, the model uses household size (topcoded at 4),
number of workers (topcoded at 3), number of vehicles (topcoded at 3),
and income (in the categories used in the regression). To apply the
household-level model to this aggregate data, I disaggregate the data to
household-level records (i.e. create a synthetic population) using
iterative proportional fitting with a seed matrix derived from the
Integrated Public Use Microdata Sample 2021 Five-Year American Community
Survey data for the entire US (Ruggles et al. 2024). This seed matrix
ships with the software. The regressions predict household-level
tripmaking, which is re-aggregated to the tract level.

The NHTS does not provide sufficient spatial detail to estimate trip
attractions. Instead, I use the Puget Sound Household Travel Survey,
which includes origin and destination Census tract in the public-use
dataset (Puget Sound Regional Council 2024). I calculate the number of
HBW and HBO trips in each time period that have the non-home end in each
Census tract in the Puget Sound region. For NHB trips, the production
and attraction ends are not clearly defined. I therefore assume that NHB
attractions and productions are half of the total number of NHB trips
originating from or destined to each tract.

To extrapolate this data to tracts outside the region, I build linear
regression models for each trip type and time period based on total
employment and employment in retail, education, and accomodation/food
services from the US Census Bureau Longitudinal Employer-Household
Dynamics Origin-Destination Employment Statistics. I balance total
attractions by trip type in each time period to match estimated
productions.

## Trip distribution

The trip distribution step uses a singly-constrained (at the production
end) gravity model (Travel Forecasting Resource 2020). The exponent for
the gravity model is calibrated for each trip type using the method
introduced by Merlin (2020) based on median trip length. The method
observes that half of the weighted destinations should be closer to the
origin than the median trip, and half should be further away. I make two
slight changes to the function presented in Merlin (2020); see appendix.

The median trip distance comes from the NHTS. I approximate the
crow-flies distance for each NHTS trip by dividing the network distance
by 1.3, a factor determined by Wang et al. (2024).

For intrazonal trips, I assume a travel distance of $0.52\sqrt{s}$,
where $s$ is the area of the TAZ. This is based on a Monte Carlo
simulation of the average distance between random points in a square.
There are two opposing factors that bias this. TAZ’s are not square,
increasing average travel distance. However, development within a TAZ is
concentrated in certain areas, decreasing average travel distance. I
assume these roughly cancel out.

## Mode choice

The mode choice model is a multinomial logit model based on the NHTS.
Because there is not detailed information about each trip and the
alternatives available in the NHTS, the model is based solely on trip
type, time period, travel distance, and housing unit density in the home
tract. For NHB trips, a separate model is estimated excluding density.
Goodness of fit is poor, but the point of the model is to demonstrate a
simple demand model, not to produce accurate forecasts.

## Network assignment

The network assignment uses a Frank-Wolfe traffic assignment algorithm
(Boyles, Lownes, and Unnikrishnan 2023). First, I convert
production-attraction format home-based trips into origin-destination
format, using directionality factors estimated from the NHTS. I
calculate peak hourly vehicle flows during each period using average
vehicle occupancy for each period and trip type, and an assumed “peaking
factor” that accounts for the proportion of traffic during the time
period that occurs in the busiest hour—for example, I assume 45% of the
traffic in the three-hour PM Peak period occurs during the busiest hour.
Then, I run the assignment algorithm, with impedances based on a Bureau
of Public Roads-style function:
$$t_{congested} = \left( 1 + 0.6\left\lbrack \frac{f}{c} \right\rbrack^{5} \right)t_{freeflow}$$

where $f$ is the predicted flow, $c$ is the link capacity, and
$t_{congested}$ and $t_{freeflow}$ are congested and free-flow link
travel times. The factors 0.6 and 5 are from the Southern California
Association of Governments travel demand model (Southern California
Association of Governments 2012).

I derive the network from OpenStreetMap. By default, I retain only the
most major roads—motorways, trunk, and primary roads (and associated
ramps). Furthermore, I run the assignment algorithm only until the
“relative gap”—a measure of the error in the estimate—is 1%, rather than
the typically recommended 0.01% (Boyce, Ralevic-Dekic, and Bar-Gera
2004), to improve performance.

[This work](https://projects.indicatrix.org/MyFirstFourStepModel) © 2026
by [Matt Bhagat-Conway](https://indicatrix.org) is licensed under [CC BY
4.0](https://creativecommons.org/licenses/by/4.0/)![](https://mirrors.creativecommons.org/presskit/icons/cc.svg)![](https://mirrors.creativecommons.org/presskit/icons/by.svg)

## References

Boyce, David, Biljana Ralevic-Dekic, and Hillel Bar-Gera. 2004.
“Convergence of Traffic Assignments: How Much Is Enough?” *Journal of
Transportation Engineering* 130 (1): 49–55.
<https://doi.org/10.1061/(ASCE)0733-947X(2004)130:1(49)>.

Boyles, Stephen D., Nicholas E. Lownes, and Avinash Unnikrishnan. 2023.
*Transportation Network Analysis*. Vol. 1.

Merlin, Louis A. 2020. “A New Method Using Medians to Calibrate
Single-Parameter Spatial Interaction Models.” *Journal of Transport and
Land Use* 13 (1, 1): 49–70. <https://doi.org/10.5198/jtlu.2020.1614>.

Puget Sound Regional Council. 2024. “Household Travel Survey Trips.”
<https://psrc-psregcncl.hub.arcgis.com/datasets/household-travel-survey-trips/explore>.

Ruggles, Steven, Sarah Flood, Matthew Sobek, Daniel Backman, Annie Chen,
Grace Cooper, Stephanie Richards, Renae Rodgers, and Megan Schouweiler.
2024. “IPUMS USA: Version 15.0.” <https://doi.org/10.18128/D010.V15.0>.

Southern California Association of Governments. 2012. “SCAG Regional
Travel Demand Model and 2012 Model Validation.”

Travel Forecasting Resource. 2020. “Destination Choice: Theoretical
Foundations.” Travel Forecasting Resource. 2020.
<https://tfresource.org>.

Wang, Shanshan, Henrik M. Bette, Michael Schreckenberg, and Thomas Guhr.
2024. “How Much Longer Do You Have to Drive Than the Crow Has to Fly?”
June 10, 2024. <https://doi.org/10.48550/arXiv.2406.06490>.
