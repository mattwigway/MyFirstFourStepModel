---
execute:
    eval: false
---

## Homework assignment

In this assignment, you will work with a simplified but complete four-step model for the central part of the Research Triangle region (Wake, Durham, Orange, and Chatham counties). Specifically, you will be evaluating the [Chatham Park development](https://chathampark.com/), which will eventually add 22,000 new homes to Pittsboro and the surrounding region, increasing the population of this area by several times.

For this project, you will use the [My First Four Step Model](https://doi.org/10.17615/qzhd-n063) R package and a model I have already estimated for the Research Triangle region. You will compare three scenarios:

1. A baseline based on current population and employment locations
1. A Chatham Park scenario, adding 22,000 households to Pittsboro
1. (Extra credit) A Chatham Park scenario that additionally widens Highway 15-501 between Pittsboro and Chapel Hill to three lanes each way

Before you can run the model, you will need to install the package. To do this, run the following R code in RStudio:

```{r}
install.packages('MyFirstFourStepModel',
  repos = c('https://mattwigway.r-universe.dev', 'https://cloud.r-project.org'))
```

All of the code you will need to run the model is available in <a href="https://raw.githubusercontent.com/mattwigway/MyFirstFourStepModel/refs/heads/main/examples/chatham_park_model.R" download>this R file</a>, which you should download and save somewhere you can find it again.

If you plan to leave this assignment until the last minute (I get it, we've all been there), **I highly suggest you at least open the R file and make sure you can run the entire model as soon as possible, to give us time to troubleshoot in case you run into software issues.** Feel free to reach out to me if you have any issues running the model. If you do run into issues, make sure you're using R 4.3 or later. Several of the steps may take thirty minutes to an hour to run on your machine, so be sure to leave enough time to run them.

Work through the `chatham_park_model.R` file you downloaded, running the code one section at a time (like we did with regression) and answer the following questions in a separate document that you upload to Canvas. You should not need to make any modifications to the code.

1. In the AM Peak home-based work trip production model, how many additional trips is an additional worker in the expected to produce? Is this statistically significant? [0.5 point]
1. Include your map of trip generation. Does the map look like you would expect? [0.5 point]
1. In the trip distribution model, the $\beta$'s are not the same across all trip types. Compare the home-based work and home based other betas. Which is smaller? What does this mean? Does this match your expectations? [1 point]
1. Include your map of trip distribution for AM peak HBW trips from central Carrboro. Does the map match your expectations? [0.5 point]
1. In the mode choice model, relative to car (the base mode), does increasing trip distance (dist_km) increase or decrease the probability of choosing walk? [0.5 point] (extra credit: is it statistically significant? [0.5 point]) 
1. What are the forecast mode shares in the baseline scenario? [0.5 point]
1. Include your map of forecasted PM Peak congestion for the baseline and Chatham Park case. How do you expect PM Peak congestion to change when Chatham Park is fully occupied? [1 point]
1. Based on your findings, what changes (if any) would you propose to the Chatham Park development and/or the surrounding transportation system (1-4 sentences)? [0.5 point]

### Extra credit

The scenario we tested above added a lot of households to Pittsboro/Chatham Park, with no improvements to the transportation system in the area. In both of the model runs above, the R code referred to `model$networks$baseline`. Replace all references to `model$networks$baseline` with `model$networks$widen_15_501` in the code that ran the Chatham Park scenario, which models upgrading US 15-501 between Chapel Hill and Pittsboro to a grade-separated, limited access, three lane each way freeway.

9. Include your map of forecast congestion after 15-501 has been widened. How does the widening affect the changes in congestion you observed in the previous question? [1 point]

10. What significant effect of roadway widening does the model not account for, and do you think this undermines the results? [1 point]
