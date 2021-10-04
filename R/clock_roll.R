clock_roll <- function(time) {
    UseMethod("clock_roll")
}

#' @export
clock_roll.Duration <- function(time) {
    if (all(as.numeric(time) > 0 & as.numeric(time) < 86400, na.rm = TRUE)) {
        time
    } else {
        time %>% lubridate::as_datetime() %>%
            flat_posixt() %>%
            hms::as_hms() %>%
            lubridate::as.duration()
    }
}

#' @export
clock_roll.Period <- function(time) {
    if (all(as.numeric(time) > 0 & as.numeric(time) < 86400, na.rm = TRUE)) {
        time
    } else {
        time %>% lubridate::as_datetime() %>%
            flat_posixt() %>%
            hms::as_hms() %>%
            lubridate::as.period()
    }
}

#' @export
clock_roll.difftime <- function(time) {
    out <- time
    units(out) <- "secs"

    if (all(as.numeric(out) > 0 & as.numeric(out) < 86400, na.rm = TRUE)) {
        units(out) <- units(time)
        out
    } else {
        out <- out %>% hms::as_hms() %>%
            lubridate::as_datetime() %>%
            flat_posixt() %>%
            hms::as_hms() %>%
            as.numeric() %>%
            lubridate::as.difftime(units = "secs")

        units(out) <- units(time)
        out
    }
}

#' @export
clock_roll.hms <- function(time) {
    if (all(as.numeric(time) > 0 & as.numeric(time) < 86400, na.rm = TRUE)) {
        time
    } else {
        time %>% lubridate::as_datetime() %>%
            flat_posixt() %>%
            hms::as_hms()
    }
}
