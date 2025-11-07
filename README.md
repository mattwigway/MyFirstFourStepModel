---
title: My First Four-Step Model
bibliography: docs/bibliography.bib
---

My First Four-Step Model is an R package and associated scripts that support a minimal [four-step travel demand model](https://transportgeography.org/contents/methods/spatial-interactions-gravity-model/transportation-land-use-four-stages-model/) intended for use in teaching. Simplicity is the primary goal; it is probably not advisable to use this for production travel forecasting, though I have tried to make the model as accurate as possible without compromising simplicity. Some of the simplifying assumptions that limit applicability are explained in the [limitations](#limitations) section.

The overarching goal is to produce a four-step model based primarily on nationwide open datasources (the [National Household Travel Survey](https://nhts.ornl.gov), the American Community Survey, the [LODES dataset](https://lehd.ces.census.gov), and [OpenStreetMap data](https://openstreetmap.org)). It is possible to estimate a model for a new region of the country in just a few lines of code (see [Estimation](#estimation)), and run it in an R script usable by people with minimal coding experience.

## Getting started

The reference implementation, which I use in teaching, is [a model for the Research Triangle region of North Carolina](examples/chatham_park_model.R). That file contains code to install the model and complete a full model run.

## Estimation


Estimating a new model requires only a few lines of code, however it does require the 2017 National Household Travel Survey and an OpenStreetMap PBF file for the region modeled. OpenStreetMap PBF files for any region are easily obtained from <https://slice.openstreetmap.us>. The code to estimate a model for the Research Triangle region is below. First, it loads the relevant libraries, and then the NHTS (`NHTS_PATH` should be replaced with a directory containing the NHTS CSV files). I filter the NHTS to only North Carolina households with a weekday travel day. The final line estimates the model. It requires the (possibly filtered) NHTS, the path to the OpenStreetMap data (written as `OSM_PATH` below but should be replaced with the actual path), the state and a vector of counties to define the region under study, and a year. Currently 2021 is most recent year available, as this is based on American Community Survey and Longitudinal Employer-Household Dynamics data availability.

Parsing the OpenStreetMap data uses Julia [@bezanson_julia_2017] for performance, which can be installed if it is not already by running `JuliaCall::install_julia()` from within R. Julia is only required for estimation; students do not need to install Julia.

```r
#| output: false
#| eval: false
library(MyFirstFourStepModel)
library(tidyverse)

# Load NHTS and filter to North Carolina weekday data
nhts = load_nhts(NHTS_PATH)
nhts$households = filter(
  nhts$households,
  HHSTATE == "NC" & TRAVDAY %in% c(2, 3, 4, 5, 6)
)

# Estimate the model using 2021 Census/LODES data for the Triangle
model = estimate(nhts, OSM_PATH, "NC", c("Durham", "Orange", "Wake", "Chatham"), 2021)
```

Lastly, the model can be saved to a file for distribution to students. 
```r
save_model(model, "my_city.model")
```

This can be loaded by the `load_model` function described above, either from a file or a URL. If any land-use or network scenarios are created or loaded prior to saving the model, they will be included in the saved file.

## More details

Many more details on both usage and architecture are [available in the paper](https://doi.org/10.17615/qzhd-n063) and the [homework assignment](examples/homework.md).