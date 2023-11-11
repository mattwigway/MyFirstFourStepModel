default_settings = function () {
    return(
        list(
            # this defines the time periods used in the model. It is not a named list, because of the
            # possibility that the overnight time period will be split in two.
            periods=list(
                list("AM_PEAK", 600, 900),
                list("MIDDAY", 900, 1600),
                list("PM_PEAK", 1600, 1900),
                list("OVERNIGHT", 1900, 2400),
                list("OVERNIGHT", 0, 600)
            )
        )
    )
}