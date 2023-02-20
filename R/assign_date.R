## This function is now in the 
## [`lubritime`](https://github.com/giperbio/lubritime) package.

assign_date <- function(start, end, ambiguity = 0) {
    checkmate::assert_multi_class(start, c("hms", "POSIXt"))
    checkmate::assert_numeric(as.numeric(hms::as_hms(start)),
                              lower = 0, upper = 86400)
    checkmate::assert_multi_class(end, c("hms", "POSIXt"))
    checkmate::assert_numeric(as.numeric(hms::as_hms(end)),
                              lower = 0, upper = 86400)
    assert_identical(start, end, type = "length")
    checkmate::assert_choice(ambiguity, c(0, 24 , NA))

    start <- start %>%
        hms::as_hms() %>%
        as.POSIXct() %>%
        flat_posixt()

    end <- end %>%
        hms::as_hms() %>%
        as.POSIXct() %>%
        flat_posixt()

    out <- dplyr::case_when(
        is.na(start) | is.na(end) ~ lubridate::as.interval(NA),
        start < end ~ lubridate::interval(start, end),
        start > end ~ lubridate::interval(start, end + lubridate::days()),
        is.na(ambiguity) ~ lubridate::as.interval(NA),
        TRUE ~ lubridate::as.interval(lubridate::hours(ambiguity), start)
    )

    out
}
