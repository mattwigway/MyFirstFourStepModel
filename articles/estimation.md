# Estimation

Estimating a new model requires only a few lines of code, however it
does require the 2017 National Household Travel Survey (NHTS, Federal
Highway Administration 2017) in CSV format and unzipped ([which can be
obtained here](https://nhts.ornl.gov/downloads)), and an OpenStreetMap
PBF file for the region modeled. There are many sources for
OpenStreetMap PBF files, but one easy source is
<https://slice.openstreetmap.us>. The code to estimate a model for the
Research Triangle region is below. First, it loads the relevant
libraries, and then the NHTS (`NHTS_PATH` should be replaced with a
directory containing the NHTS CSV files). I filter the NHTS to only
North Carolina households with a weekday travel day ($n = 7,146$). The
final line estimates the model. It requires the (possibly filtered)
NHTS, the path to the OpenStreetMap data (written as `OSM_PATH` below
but should be replaced with the actual path), the state and a vector of
counties to define the region under study, and a year. Currently 2021 is
most recent year available, as this is based on American Community
Survey and Longitudinal Employer-Household Dynamics data availability.

Parsing the OpenStreetMap data uses Julia (Bezanson et al. 2017) for
performance, which can be installed if it is not already by running
[`JuliaCall::install_julia()`](https://rdrr.io/pkg/JuliaCall/man/install_julia.html)
from within R. Julia is only required for estimation; students do not
need to install Julia.

``` r
library(MyFirstFourStepModel)
library(tidyverse)

# Load NHTS and filter to North Carolina weekday data
nhts = load_nhts(NHTS_PATH)
nhts$households = filter(
  nhts$households,
  HHSTATE == "NC" & TRAVDAY %in% c(2, 3, 4, 5, 6)
)

# Estimate the model using 2023 Census/LODES data for the Triangle
model = estimate(nhts, OSM_PATH, "NC", c("Durham", "Orange", "Wake", "Chatham"), 2023)
```

Lastly, the model can be saved to a file for distribution to students.

``` r
save_model(model, "chatham_park.mf4sm")
```

This can be loaded by the `load_model` function described above, either
from a file or a URL. If any [land-use or network
scenarios](https://projects.indicatrix.org/MyFirstFourStepModel/articles/scenarios.md)
are created or loaded prior to saving the model, they will be included
in the saved file.

[This work](https://projects.indicatrix.org/MyFirstFourStepModel) © 2026
by [Matt Bhagat-Conway](https://indicatrix.org) is licensed under [CC BY
4.0](https://creativecommons.org/licenses/by/4.0/)![](https://mirrors.creativecommons.org/presskit/icons/cc.svg)![](https://mirrors.creativecommons.org/presskit/icons/by.svg)

## References

Bezanson, Jeff, Alan Edelman, Stefan Karpinski, and Viral B. Shah. 2017.
“Julia: A Fresh Approach to Numerical Computing.” *SIAM Review* 59 (1):
65–98. <https://doi.org/10.1137/141000671>.

Federal Highway Administration. 2017. “2017 National Household Travel
Survey.” <https://nhts.ornl.gov/downloads>.
