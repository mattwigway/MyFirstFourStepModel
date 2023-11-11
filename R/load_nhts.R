#' Load 2017 NHTS data, handling types appropriately
load_nhts <- function (base_path) {
    list(
        trips = read_csv(file.path(base_path, "trippub.csv"), col_types=cols_only(
            HOUSEID = col_integer(),
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
            STRTTIME = col_integer()
        )),
        households = read_csv(file.path(base_path, "hhpub.csv"), col_types=cols_only(
            HOUSEID = col_integer(),
            HHVEHCNT = col_integer(),
            HHSIZE = col_integer(),
            HHFAMINC = col_integer(),
            HTRESDN = col_integer(),
            WRKCOUNT = col_integer()
        )) %>%
        filter(HTRESDN > 0)
    ) %>% return()
}

