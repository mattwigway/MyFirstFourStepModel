#' Get the time period from an NHTS time.
#' Currently these are hardcoded as AM Peak 6-10 AM, Midday 10-4, PM Peak 4-7, Overnight 7PM - 6AM.
get_time_period = function(time) {
    case_when(
        time < 600 ~ "Overnight",
        time < 1000 ~ "AM Peak",
        time < 1600 ~ "Midday",
        time < 1900 ~ "PM Peak",
        .default="Overnight"
    ) %>%
    factor(levels=c("AM Peak", "Midday", "PM Peak", "Overnight")) %>%
    return()
}