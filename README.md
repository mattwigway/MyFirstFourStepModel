---
bibliography: docs/bibliography.bib
editor:
  markdown:
    references:
      location: block
title: Baby's First Four-Step Model
toc-title: Table of contents
---

Baby's First Four-Step Model is an R package and associated scripts that
support a minimal [four-step travel demand
model](https://transportgeography.org/contents/methods/spatial-interactions-gravity-model/transportation-land-use-four-stages-model/)
intended for use in teaching. Simplicity is the primary goal; it is
probably not advisable to use this for production travel forecasting,
though I have tried to make the model as accurate as possible without
compromising simplicity. Some of the simplifying assumptions that limit
applicability are explained in the [limitations](#limitations) section.

The overarching goal is to produce a four-step model based primarily on
nationwide open datasources (the [National Household Travel
Survey](https://nhts.ornl.gov), the American Community Survey, the
[LODES dataset](https://lehd.ces.census.gov), and [OpenStreetMap
data](https://openstreetmap.org)). It is possible to estimate a model
for a new region of the country in just a few lines of code (see
[Estimation](#estimation)). The resulting model should be runnable in a
single Jupyter notebook, hosted on [Google
Colab](https://colab.research.google.com/) to facilitate teaching. No R
knowledge is necessary to run the model.

## Estimation

### Trip generation

Trip generation is estimated using 20 trip generation regressions - home
based work (from home), home based work (to home), home based other
(from home), home based other (to home), and non-home-based, stratified
by time of day (AM peak, PM peak, midday, overnight).

To estimate based on aggregate data, we create a seed matrix based on
PUMS data for the entire region, and use that match ACS marginals in
each tract.

### Destination choice/trip distribution

The destination choice/trip distribution model is the most difficult to
calibrate using national datasets of all of the models, because the NHTS
does not provide detailed spatial information about trip origins and
destinations. To avoid this problem, we first estimate a spatial
interaction model using the open [Puget Sound Household Travel
Survey](https://psrc-psregcncl.hub.arcgis.com/datasets/household-travel-survey-trips/explore)
dataset, which includes trip origins and destinations at the tract level
in the public dataset. We use this to calibrate a gravity model based on
Census and LODES data.

This model is only valid for the Puget Sound region. To adjust it for
other regions, we retain the scale parameters for the origin and
destination locations, but recalibrate the impedance parameter using the
median-based method described in @merlin_new_2020.

## Limitations

In creating the simplest possible four-step model there are a number of
significant limitations. In particular, there are no "extra" models
beyond the four steps---no external trips model, no freight model, no
college model, etc. This means that all of the traffic modeled will be
personal vehicles.

It does not currently link transit trips with transfers in the middle,
though many of these should already be reported as a single trip in the
NHTS.