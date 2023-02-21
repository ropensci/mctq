sum_time <- function(..., cycle = NULL, reverse = TRUE, na_rm = FALSE) {
    sum_time_build(..., vectorize = FALSE, cycle = cycle, reverse = reverse,
                   na_rm = na_rm)
}

vct_sum_time <- function(..., cycle = NULL, reverse = TRUE, na_rm = FALSE) {
    sum_time_build(..., vectorize = TRUE, cycle = cycle, reverse = reverse,
                   na_rm = na_rm)
}

sum_time_build <- function(..., vectorize = FALSE, cycle = NULL,
                           reverse = TRUE, na_rm = FALSE) {
    out <- list(...)
    classes <- c("Duration", "difftime", "hms", "POSIXct", "POSIXlt",
                 "Interval")

    lapply(out, checkmate::assert_multi_class, classes)
    checkmate::assert_flag(vectorize)
    checkmate::assert_multi_class(cycle, c("numeric", "Duration"),
                                  null.ok = TRUE)
    checkmate::assert_number(cycle, lower = 0, null.ok = TRUE)
    checkmate::assert_flag(reverse)
    checkmate::assert_flag(na_rm)

    if (isTRUE(vectorize) &&
        !(length(unique(vapply(out, length, integer(1)))) == 1)) {
        cli::cli_abort("All values in '...' must have the same length.")
    }

    out <- lapply(out, extract_seconds)

    if (isTRUE(na_rm)) {
        out <- lapply(out, function(x) dplyr::if_else(is.na(x), 0, x))
    }

    if (isTRUE(vectorize)) {
        out <- Reduce("+", out)
    } else {
        out <- do.call("c", out)
        out <- sum(out, na.rm = na_rm)
    }

    if (!is.null(cycle)) out <- out %>% cycle_time(cycle, reverse)

    lubridate::duration(out)
}
