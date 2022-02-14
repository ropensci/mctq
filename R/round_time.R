#' Round time values
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `round_time()` takes a `Duration`, `difftime`, `hms`, `POSIXct`, or
#' `POSIXlt` object and round it at the level of seconds.
#'
#' @details
#'
#' ## Round standard
#'
#' `round_time()` uses [base::round()] for rounding. That is to say that
#' `round_time()` uses the same IEC 60559 standard (_"go to the even digit"_)
#' for rounding off a 5. Therefore, `round(0.5)` is equal to 0 and `round(-1.5)`
#' is equal to -2. See `?round` to learn more.
#'
#' ## `Period` objects
#'
#' [`Period`][lubridate::period()] objects are a special type of object
#' developed by the [lubridate][lubridate::lubridate-package] team that
#' represents "human units", ignoring possible timeline irregularities. That is
#' to say that 1 day as `Period` can have different time spans, when looking to
#' a timeline after a irregularity event.
#'
#' Since the time span of a `Period` object can fluctuate, `round_time()` don't
#' accept this kind of object. You can transform it to a `Duration` object and
#' still use the function, but beware that this can produce errors.
#'
#' Learn more about `Period` objects in the [Dates and
#' times](https://r4ds.had.co.nz/dates-and-times.html#periods) chapter of
#' Wickham & Grolemund (n.d.).
#'
#' @param x An object belonging to one of the following classes: `Duration`,
#'   `difftime`, `hms`, `POSIXct`, or `POSIXlt`.
#'
#' @return An object of the same class of `x` rounded at the level of seconds.
#'
#' @seealso Other date-time rounding functions: [hms::round_hms()]
#'   [hms::trunc_hms()] [lubridate::round_date()].
#'
#' @family utility functions
#' @template references_g
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' lubridate::dmilliseconds(123456789)
#' #> [1] "123456.789s (~1.43 days)" # Expected
#' round_time(lubridate::dmilliseconds(123456789))
#' #> [1] "123457s (~1.43 days)" # Expected
#'
#' as.difftime(12345.6789, units = "secs")
#' #> Time difference of 12345.68 secs # Expected
#' round_time(as.difftime(12345.6789, units = "secs"))
#' #> Time difference of 12346 secs # Expected
#'
#' hms::as_hms(12345.6789)
#' #> 03:25:45.6789 # Expected
#' round_time(hms::as_hms(12345.6789))
#' #> 03:25:46 # Expected
#'
#' lubridate::as_datetime(12345.6789, tz = "EST")
#' #> [1] "1969-12-31 22:25:45 EST" # Expected
#' as.numeric(lubridate::as_datetime(12345.6789, tz = "EST"))
#' #> [1] 12345.68 # Expected
#' round_time(lubridate::as_datetime(12345.6789, tz = "EST"))
#' #> [1] "1969-12-31 22:25:46 EST" # Expected
#' as.numeric(round_time(lubridate::as_datetime(12345.6789, tz = "EST")))
#' #> [1] 12346 # Expected
#'
#' ## Vector example
#'
#' x <- c(lubridate::dhours(5.6987), lubridate::dhours(2.6875154))
#' x
#' #> [1] "20515.32s (~5.7 hours)"    "9675.05544s (~2.69 hours)" # Expected
#' round_time(x)
#' #> [1] "20515s (~5.7 hours)" "9675s (~2.69 hours)" # Expected
round_time <- function(x) {
    classes <- c("Duration", "difftime", "hms", "POSIXct", "POSIXlt")
    checkmate::assert_multi_class(x, classes)

    UseMethod("round_time")
}

#' @rdname round_time
#' @export
round_time.Duration <- function(x) {
    x %>% as.numeric() %>%
        round() %>%
        lubridate::dseconds()
}

#' @rdname round_time
#' @export
round_time.difftime <- function(x) {
    out <- x
    units(out) <- "secs"

    out <- out %>% as.numeric() %>%
        round() %>%
        as.difftime(units = "secs")

    units(out) <- units(x)

    out
}

#' @rdname round_time
#' @export
round_time.hms <- function(x) {
    x %>% as.numeric() %>%
        round() %>%
        hms::as_hms()
}

#' @rdname round_time
#' @export
round_time.POSIXct <- function(x) {
    out <- x %>% as.numeric() %>%
        round()

    attributes(out) <- attributes(x)

    out
}

#' @rdname round_time
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
