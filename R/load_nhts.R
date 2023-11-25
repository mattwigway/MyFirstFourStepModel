#' Load 2017 NHTS data, handling types appropriately
load_nhts <- function (base_path) {
    list(
        trips = read_csv(file.path(base_path, "trippub.csv"), col_types=cols_only(
            HOUSEID = col_integer(),
            PERSONID = col_integer(),
            TDTRPNUM = col_integer(),
            TRPTRANS = col_integer(),
            ONTD_P1 = col_integer(),
            ONTD_P2 = col_integer(),
            ONTD_P3 = col_integer(),
            ONTD_P4 = col_integer(),
            ONTD_P5 = col_integer(),
            ONTD_P6 = col_integer(),
            ONTD_P7 = col_integer(),
            ONTD_P8 = col_integer(),
            ONTD_P9 = col_integer(),
            ONTD_P10 = col_integer(),
            ONTD_P11 = col_integer(),
            ONTD_P12 = col_integer(),
            WHYFROM = col_integer(),
            WHYTO = col_integer(),
            STRTTIME = col_integer(),
            WTTRDFIN = col_double(),
            TRPMILES = col_double()
        )),
        households = read_csv(file.path(base_path, "hhpub.csv"), col_types=cols_only(
            HOUSEID = col_integer(),
            HHVEHCNT = col_integer(),
            HHSIZE = col_integer(),
            HHFAMINC = col_integer(),
            HTRESDN = col_integer(),
            WRKCOUNT = col_integer(),
            WTHHFIN = col_double(),
            HHSTATE = col_character()
        )) %>%
        filter(HTRESDN > 0 & HHFAMINC > 0) %>%
        mutate(
            income=case_when(
                HHFAMINC <= 3 ~ 0,
                HHFAMINC <= 6 ~ 35000,
                HHFAMINC <= 7 ~ 75000,
                .default=100000
            )
        )
    ) %>% return()
}

