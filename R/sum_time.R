#' Sum time objects
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `sum_time()` returns the sum of the time from different kinds of date/time
#' objects.
#'
#' `vct_sum_time()` returns the vectorized sum of the time from different kinds
#' of date/time objects.
#'
#' Both functions can be set to work with a circular time frame (see Details to
#' learn more).
#'
#' @details
#'
#' ## `sum_time()` versus `vct_sum_time()`
#'
#' `sum_time()` behaves similar to [base::sum()], in the sense that it
#' aggregates the time lengths of values in `...` into a single data point. For
#' example, `sum_time(c(x, y), z)` will have the same output as `sum_time(x, y,
#' z)`.
#'
#' `vct_sum_time()` performs a different type of sum (a vectorized one). Instead
#' of aggregating the time lengths, the function perform a paired sum between
#' elements. For example, `sum_time(c(x, y), c(w, z))` will return a vector like
#' `c(sum_time(x, w), sum_time(y, z))`. Because of that, `vct_sum_time()`
#' requires that all objects in `...` have the same length.
#'
#' ## Linear versus circular sum
#'
#' Time can have different "shapes".
#'
#' If the objective is to measure the duration (time span) of an event, time is
#' usually measured considering a linear frame, with a fixed point of
#' [origin](https://en.wikipedia.org/wiki/Origin_(mathematics)). In this
#' context, the time value distance itself to infinity in relation to the
#' origin.
#'
#' ```
#'                                    B
#'                              |----------|
#'                                         A
#'                              |---------------------|
#'  - inf                                                inf +
#' <----------------------------|----------|----------|------->
#'  s                           0          5          10     s
#'                            origin
#'
#' A + B = 10 + 5 = 15s
#' ```
#'
#' But that's not the only possible "shape" of time, as it can also be measured
#' in other contexts.
#'
#' In a "time of day" context, time will be linked to the rotation of the
#' earth, "resetting" when a new rotation cycle starts. That brings a different
#' kind of shape to time: a circular shape. With this shape the time value
#' encounters the origin at the end of each cycle.
#'
#' ```
#'                - <--- h ---> +
#'                     origin
#'                 . . . 0 . . .
#'              .                 .
#'             .                   .
#'            .                     .
#'           .                       .
#'          .                         .
#'          18                        6
#'          .                         .
#'           .                       .
#'            .                     .
#'             .                   .
#'              .                 .
#'                 . . . 12 . . .
#'
#' 18 + 6 = 0h
#' ```
#'
#' If we transpose this circular time frame to a linear one, it would look like
#' this:
#'
#'
#' ```
#' <----|---------------|---------------|---------------|----->
#'     0h              12h              0h             12h
#'   origin                           origin
#' ```
#'
#' Note that now the origin is not fix, but cyclical.
#'
#' `sum_time()` and `vct_sum_time()` can both operate in either a linear or a
#' circular fashion. If `cycle = NULL` (default), the function will use a
#' linear approach. Else, the function will use a circular approach relative to
#' the cycle length (e.g, `cycle = 86400` (1 day)).
#'
#' ## Fractional time
#'
#' `sum_time()` uses the `%%` operator to cycle values. Hence, it can be subject
#' to catastrophic loss of accuracy if values in `...` are fractional and much
#' larger than `cycle`. A warning is given if this is detected.
#'
#' `%%` is a `builtin` R function that operates like this:
#'
#' ```
#' function(a, b) {
#'     a - floor(a / b) * b
#' }
#' ````
#'
#' ## Negative time cycling
#'
#' If the sum of the time is negative, with a `cycle` assigned and
#' `reverse = FALSE`, `sum_time()` and `vtc_sum_time()` will perform the cycle
#' considering the absolute value of the sum and return the result with a
#' negative signal.
#'
#' However, If the sum of the time have a negative value, with a `cycle`
#' assigned and `reverse = TRUE`, `sum_time()` and `vtc_sum_time()` will perform
#' the cycle in reverse, relative to its origin.
#'
#' Example: If the sum of the time have a -30h time span in a reversed cycle of
#' 24h, the result will be 18h. By removing the full cycles of -30h you will
#' get -6h (-30 + 24), and -6h relative to the origin will be 18h.
#'
#' __\deqn{- (|-30| \mod 24) + 24 = 18}__
#'
#' ```
#'                - <--- h ---> +
#'                     origin
#'                 . . . 0 . . .
#'               .                 .
#'             .                   .
#'            .                     .
#'           .                       .
#'          .                         .
#'     (-6) 18                        6 (-18)
#'          .                         .
#'           .                       .
#'            .                     .
#'             .                   .
#'              .                 .
#'                 . . . 12 . . .
#'                     (-12)
#' ```
#'
#' ## `POSIXt` objects
#'
#' [`POSIXt`][base::as.POSIXct()] objects in `...` will be stripped of their
#' dates. Only the time will be considered.
#'
#' Both `POSIXct` and `POSIXlt` are objects that inherits the class `POSIXt`.
#' Learn more about it in [base::DateTimeClasses].
#'
#' ## `Period` objects
#'
#' [`Period`][lubridate::period()] objects are a special type of object
#' developed by the [lubridate][lubridate::lubridate-package] team that
#' represents "human units", ignoring possible timeline irregularities. That is
#' to say that 1 day as `Period` can have different time spans, when looking to
#' a timeline after a irregularity event. `sum_time()` and `vct_sum_time()`
#' ignores that property of `Period` objects, treating them like objects of
#' class [`Duration`][lubridate::duration()].
#'
#' Learn more about `Period` objects in the [Dates and
#' times](https://r4ds.had.co.nz/dates-and-times.html#periods) chapter of
#' Wickham & Grolemund (n.d.).
#'
#' ## `Interval` objects
#'
#' By using [`Interval`][lubridate::interval()] objects in `...`, `sum_time()`
#' and `vct_sum_time()` will consider only their time lengths. That is, the
#' amount of seconds of the intervals.
#'
#' Learn more about `Interval` objects in the [Dates and
#' times](https://r4ds.had.co.nz/dates-and-times.html#periods) chapter of
#' Wickham & Grolemund (n.d.).
#'
#' ## Timeline irregularities
#'
#' This function does not take into account timeline irregularities (e.g.,
#' leap years, DST, leap seconds). This may not be an issue for most people, but
#' it must be considered when doing time arithmetic.
#'
#' @param ... Objects belonging to one of the following classes: `Duration`,
#'   `Period`, `difftime`, `hms`, `POSIXct`, `POSIXlt`, or `Interval`.
#' @param cycle (optional) A `numeric` or `Duration` object of length 1, equal
#'   or greater than 0, indicating the cycle length in seconds. If `NULL` the
#'   function will perform a linear sum (see Details to learn more) (default:
#'   `NULL`).
#' @param reverse (optional) A `logical` value indicating if the function must
#'   use a reverse cycle for negative sums (see Details to learn more) (default:
#'   `TRUE`).
#' @param na_rm (optional) a `logical` value indicating if the function must
#'   remove `NA` values while performing the sum (default: `FALSE`).
#'
#' @return
#'
#' * If `cycle = NULL`, a `Duration` object with a linear sum of the time from
#' objects in `...`.
#' * If `cycle != NULL`, a `Duration` object with a circular sum of the time
#' from objects in `...`.
#'
#' @family utility functions
#' @template references_g
#' @export
#'
#' @examples
#' ## Non-vectorized sum in an linear time frame
#'
#' x <- c(as.POSIXct("2020-01-01 15:00:00"), as.POSIXct("1999-05-04 17:30:00"))
#' y <- lubridate::as.interval(lubridate::dhours(7), as.Date("1970-05-08"))
#' sum_time(x, y)
#' #> [1] "142200s (~1.65 days)" # 39:30:00 # Expected
#'
#' ## Non-vectorized sum in a circular time frame of 24 hours
#'
#' x <- c(lubridate::hours(25), lubridate::dhours(5), lubridate::minutes(50))
#' sum_time(x, cycle = lubridate::ddays())
#' #> [1] "24600s (~6.83 hours)" # 06:50:00 # Expected
#'
#' x <- c(hms::parse_hm("00:15"), hms::parse_hm("02:30"), hms::as_hms(NA))
#' sum_time(x, cycle = lubridate::ddays())
#' #> NA # Expected
#' sum_time(x, cycle = lubridate::ddays(), na_rm = TRUE)
#' #> [1] "9900s (~2.75 hours)" # 02:45:00 # Expected
#'
#' x <- c(lubridate::hours(-12), lubridate::dhours(-13))
#' sum_time(x, cycle = lubridate::ddays(), reverse = FALSE)
#' #> [1] "-3600s (~-1 hours)" # -01:00:00 # Expected
#'
#' x <- c(lubridate::hours(-12), lubridate::dhours(-13))
#' sum_time(x, cycle = lubridate::ddays(), reverse = TRUE)
#' #> [1] "82800s (~23 hours)" # 23:00:00 # Expected
#'
#' ## Vectorized sum in an linear time frame
#'
#' x <- c(lubridate::dhours(6), NA)
#' y <- c(hms::parse_hm("23:00"), hms::parse_hm("10:00"))
#' vct_sum_time(x, y)
#' #> [1] "104400s (~1.21 days)" NA # 29:00:00 NA # Expected
#' vct_sum_time(x, y, na_rm = TRUE)
#' #> [1] "104400s (~1.21 days)" "36000s (~10 hours)" # Expected
#'
#' ## Vectorized sum in a circular time frame of 24 hours
#'
#' x <- c(lubridate::dhours(6), NA)
#' y <- c(hms::parse_hm("23:00"), hms::parse_hm("10:00"))
#' vct_sum_time(x, y, cycle = lubridate::ddays())
#' #> [1] "18000s (~5 hours)" NA  # Expected
#' vct_sum_time(x, y, cycle = lubridate::ddays(), na_rm = TRUE)
#' #> [1] "18000s (~5 hours)"  "36000s (~10 hours)" # Expected
#'
#' x <- c(lubridate::hours(-49), lubridate::hours(-24))
#' y <- c(hms::parse_hm("24:00"), - hms::parse_hm("06:00"))
#' vct_sum_time(x, y, cycle = lubridate::ddays(), reverse = FALSE)
#' #> "-3600s (~-1 hours)"  "-21600s (~-6 hours)" # Expected
#'
#' x <- c(lubridate::hours(-49), lubridate::hours(-24))
#' y <- c(hms::parse_hm("24:00"), - hms::parse_hm("06:00"))
#' vct_sum_time(x, y, cycle = lubridate::ddays(), reverse = TRUE)
#' #> "82800s (~23 hours)" "64800s (~18 hours)" # Expected
sum_time <- function(..., cycle = NULL, reverse = TRUE, na_rm = FALSE) {
    sum_time_build(..., vectorize = FALSE, cycle = cycle, reverse = reverse,
                   na_rm = na_rm)
}

#' @rdname sum_time
#' @export
vct_sum_time <- function(..., cycle = NULL, reverse = TRUE, na_rm = FALSE) {
    sum_time_build(..., vectorize = TRUE, cycle = cycle, reverse = reverse,
                   na_rm = na_rm)
}

sum_time_build <- function(..., vectorize = FALSE, cycle = NULL,
                           reverse = TRUE, na_rm = FALSE) {
    out <- list(...)

    assert_custom <- function(x) {
        classes <- c("Duration", "Period", "difftime", "hms", "POSIXct",
                     "POSIXlt", "Interval")

        checkmate::assert_multi_class(x, classes)
    }

    checkmate::assert_flag(vectorize)
    checkmate::assert_multi_class(cycle, c("numeric", "Duration"),
                                  null.ok = TRUE)
    checkmate::assert_number(cycle, lower = 0, null.ok = TRUE)
    checkmate::assert_flag(na_rm)
    lapply(out, assert_custom)

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
