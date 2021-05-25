clock_roll <- function(x) {
    UseMethod("clock_roll")
}

#' @export
clock_roll.Duration <- function(x) {
    if (all(as.numeric(x) > 0 & as.numeric(x) < 86400, na.rm = TRUE)) {
        x
    } else {
        x %>% lubridate::as_datetime() %>%
            flat_posixt() %>%
            hms::as_hms() %>%
            lubridate::as.duration()
    }
}

#' @export
clock_roll.Period <- function(x) {
    if (all(as.numeric(x) > 0 & as.numeric(x) < 86400, na.rm = TRUE)) {
        x
    } else {
        x %>% lubridate::as_datetime() %>%
            flat_posixt() %>%
            hms::as_hms() %>%
            lubridate::as.period()
    }
}

#' @export
clock_roll.difftime <- function(x) {
    out <- x
    units(out) <- "secs"

    if (all(as.numeric(out) > 0 & as.numeric(out) < 86400, na.rm = TRUE)) {
        units(out) <- units(x)
        out
    } else {
        out <- out %>% hms::as_hms() %>%
            lubridate::as_datetime() %>%
            flat_posixt() %>%
            hms::as_hms() %>%
            as.numeric() %>%
            lubridate::as.difftime(units = "secs")

        units(out) <- units(x)
        out
    }
}

#' @export
clock_roll.hms <- function(x) {
    if (all(as.numeric(x) > 0 & as.numeric(x) < 86400, na.rm = TRUE)) {
        x
    } else {
        x %>% lubridate::as_datetime() %>%
            flat_posixt() %>%
            hms::as_hms()
    }
}
