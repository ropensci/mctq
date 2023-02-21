round_time <- function(x) {
    classes <- c("Duration", "difftime", "hms", "POSIXct", "POSIXlt")
    checkmate::assert_multi_class(x, classes)

    UseMethod("round_time")
}

#' @export
round_time.Duration <- function(x) {
    x %>%
        as.numeric() %>%
        round() %>%
        lubridate::dseconds()
}

#' @export
round_time.difftime <- function(x) {
    out <- x
    units(out) <- "secs"

    out <- out %>%
        as.numeric() %>%
        round() %>%
        as.difftime(units = "secs")

    units(out) <- units(x)

    out
}

#' @export
round_time.hms <- function(x) {
    x %>%
        as.numeric() %>%
        round() %>%
        hms::as_hms()
}

#' @export
round_time.POSIXct <- function(x) {
    out <- x %>%
        as.numeric() %>%
        round()

    attributes(out) <- attributes(x)

    out
}

#' @export
round_time.POSIXlt <- function(x) {
    out <- unclass(x)

    if (round(out$sec) >= 60) {
        out$sec <- round(out$sec) - 60
        out$min <- out$min + 1
    } else {
        out$sec <- round(out$sec)
    }

    class(out) <- class(x)

    out
}
