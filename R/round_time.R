#' Round time objects
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' This function will be removed on the next `mctq` version. You can still find
#' it in the [`lubritime`](https://github.com/giperbio/lubritime) package.
#'
#' `round_time()` takes a [`Duration`][lubridate::duration()],
#' [`difftime`][base::as.difftime()], [`hms`][hms::hms()],
#' [`POSIXct`][base::as.POSIXct()], or [`POSIXlt`][base::as.POSIXlt()] object
#' and round it at the seconds level.
#'
#' @details
#'
#' ## Round standard
#'
#' `round_time()` uses [base::round()] for rounding. That is to say that
#' `round_time()` uses the same IEC 60559 standard (_"go to the even digit"_)
#' for rounding off a 5. Therefore, `round(0.5)` is equal to 0 and `round(-1.5)`
#' is equal to -2. See [`?round`][base::round()] to learn more.
#'
#' ## `Period` objects
#'
#' [`Period`][lubridate::period()] objects are special type of objects
#' developed by the [lubridate][lubridate::lubridate-package] team that
#' represents "human units", ignoring possible timeline irregularities. That is
#' to say that 1 day as `Period` can have different time spans, when looking to
#' a timeline after a irregularity event.
#'
#' Since the time span of a [`Period`][lubridate::period()] object can
#' fluctuate, `round_time()` don't accept this kind of object. You can transform
#' it to a [`Duration`][lubridate::duration()] object and still use the
#' function, but beware that this can produce errors.
#'
#' Learn more about [`Period`][lubridate::period()] objects in the [Dates and
#' times](https://r4ds.had.co.nz/dates-and-times.html#periods) chapter of
#' Wickham & Grolemund book (n.d.).
#'
#' @param x An object belonging to one of the following classes:
#'   [`Duration`][lubridate::duration()], [`difftime`][base::as.difftime()],
#'   [`hms`][hms::hms()], [`POSIXct`][base::as.POSIXct()], or
#'   [`POSIXlt`][base::as.POSIXlt()].
#'
#' @return An object of the same class of `x` rounded at the seconds level.
#'
#' @seealso Other date-time rounding functions:
#'   [`round_hms()`][hms::round_hms()] [`trunc_hms()`][hms::trunc_hms()]
#'   [`round_date()`][lubridate::round_date()].
#'
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
#' c(lubridate::dhours(5.6987), lubridate::dhours(2.6875154))
#' #> [1] "20515.32s (~5.7 hours)"    "9675.05544s (~2.69 hours)" # Expected
#' round_time(c(lubridate::dhours(5.6987), lubridate::dhours(2.6875154)))
#' #> [1] "20515s (~5.7 hours)" "9675s (~2.69 hours)" # Expected
round_time <- function(x) {
    lifecycle::deprecate_soft(when = "0.3.2", what = "round_time()")
    
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
