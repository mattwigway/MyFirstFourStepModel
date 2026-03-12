test_that("ipf works", {
  # very simple IPF example
  # example from https://projects.indicatrix.org/abm-class/synthetic_populations.html#/ipf-initial
  seed = tibble::tribble(
    ~age0_17 , ~age18_34 , ~age35_54 , ~age55p , ~race   ,
         125 ,        75 ,        95 ,      80 , "White" ,
          30 ,        15 ,        60 ,      15 , "Black" ,
          16 ,        19 ,        22 ,      18 , "Asian" ,
           7 ,         4 ,         2 ,       4 , "Other"
  ) |>
    tidyr::pivot_longer(starts_with("age"), names_to = "age", values_to = "weight", names_prefix = "age")

  marginals = tibble::tribble(
    ~marginal , ~value  , ~count ,
    "age"     , "0_17"  ,     40 ,
    "age"     , "18_34" ,     60 ,
    "age"     , "35_54" ,     70 ,
    "age"     , "55p"   ,     50 ,
    "race"    , "White" ,    150 ,
    "race"    , "Black" ,     40 ,
    "race"    , "Asian" ,     18 ,
    "race"    , "Other" ,     12
  )

  result = ipf(seed, marginals) |>
    mutate(weight = round(weight, digits = 2)) |>
    tidyr::pivot_wider(names_from = "age", values_from = "weight", names_prefix = "age")

  expected = tibble::tribble(
    # NB a few slight changes from slides due to rounding - White 0-17 was 29.19 and Other 0-17 was 2.97
    # Black 35-54 was 21.57
    ~race   , ~age0_17 , ~age18_34 , ~age35_54 , ~age55p ,
    "White" , 29.2     , 42.8      , 41.51     , 36.5    ,
    "Black" ,  5.76    ,  7.04     , 21.56     ,  5.63   ,
    "Asian" ,  2.08    ,  6.02     ,  5.34     ,  4.56   ,
    "Other" ,  2.96    ,  4.14     ,  1.58     ,  3.31
  )

  expect_equal(result, expected)
})
