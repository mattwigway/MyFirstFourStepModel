---
title: My First Four-Step Model
bibliography: docs/bibliography.bib
---

My First Four-Step Model is an R package and associated scripts that support a minimal [four-step travel demand model](https://transportgeography.org/contents/methods/spatial-interactions-gravity-model/transportation-land-use-four-stages-model/) intended for use in teaching. Simplicity is the primary goal; it is probably not advisable to use this for production travel forecasting, though I have tried to make the model as accurate as possible without compromising simplicity. Some of the simplifying assumptions that limit applicability are explained in the [limitations](#limitations) section.

The overarching goal is to produce a four-step model based primarily on nationwide open datasources (the [National Household Travel Survey](https://nhts.ornl.gov), the American Community Survey, the [LODES dataset](https://lehd.ces.census.gov), and [OpenStreetMap data](https://openstreetmap.org)). It is possible to estimate a model for a new region of the country in just a few lines of code (see [Estimation](#estimation)), and run it in a Quarto notebook usable even by people with minimal R experience.

## Reference implementation

The reference implementation, which I use in teaching, is [a model for the Research Triangle region of North Carolina](https://github.com/mattwigway/rdu-four-step-model).

## Estimation

### Trip generation

Trip generation is estimated using 20 trip generation regressions - home based work (from home), home based work (to home), home based other (from home), home based other (to home), and non-home-based, stratified by time of day (AM peak, PM peak, midday, overnight).

To estimate based on aggregate data, we create a seed matrix based on [IPUMS](https://usa.ipums.org) data for the entire region, and use that match ACS marginals in each tract using IPF. To avoid issues with very small cell counts, we topcode vehicles at 3, household size at 4, and number of workers at 3.

This topcoding would artificially reduce trips. To solve this problem, we then disaggregate each of the topcoded categories using the unconditional distribution within each category from the PUMS. For instance, for 4+ person households, XX% are 4 person, YY percent are 5 person, etc.

Once this process is complete, the regression models are applied to estimate trip counts by type and time period.

### Attraction

The attraction model is the most difficult to calibrate using national datasets of all of the models, because the NHTS does not provide detailed spatial information about trip destinations. To avoid this problem, we first estimate a spatial interaction model using the open [Puget Sound Household Travel Survey](https://psrc-psregcncl.hub.arcgis.com/datasets/household-travel-survey-trips/explore) dataset, which includes trip origins and destinations at the tract level in the public dataset. We then balance attractions to match productions, since productions are generated using national data.

### Destination choice/trip distribution

The destination choice model is a simple gravity model; we calibrate the impedance parameter using the median-based method described in Merlin (2020).

### Mode choice

The mode choice model is similarly difficult to estimate, because attributes of alternatives are impossible to generate from NHTS data. In the future I hope to estimate a mode choice model using the Puget Sound data, but for now a simple multinomial logit model is estimated based on demographics, home tract density, trip purpose, trip length, and period.

### Assignment

We use a simple one-shot Frank-Wolfe static traffic assignment algorithm to assign vehicle flows to roads, using a Bureau of Public Roads type impedance function. The algorithm is written in pure R, so is not particularly performant but will run anywhere R does. For this reason, a simple network is recommended, containing only major roads.

## Limitations

In creating the simplest possible four-step model there are a number of significant limitations. In particular, there are no "extra" models beyond the four steps—no external trips model, no freight model, no college model, etc. This means that all of the traffic modeled will be personal vehicles.

It does not currently link transit trips with transfers in the middle, though many of these should already be reported as a single trip in the NHTS.

### Induced demand

Four-step models are known for not doing well at estimating induced demand, and My First Four Step Model is even worse—it does not consider induced demand at all.

## References

Merlin, Louis A. 2020. “A New Method Using Medians to Calibrate Single-Parameter Spatial Interaction Models.” Journal of Transport and Land Use 13 (1, 1): 49–70. https://doi.org/10.5198/jtlu.2020.1614.