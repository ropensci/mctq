#' Sum time objects
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `sum_time()` returns the sum of the time from different kinds of date/time
#' objects.
#'
#' `sum_times()` returns the vectorized sum of the time from different kinds of
#' date/time objects.
#'
#' Both functions can be set to work with a circular time frame (see Details to
#' learn more).
#'
#' @details
#'
#' ## `sum_time()` versus `sum_times()`
#'
#' `sum_time()` behaves similar to `sum()`, in the sense that it aggregates the
#' time lengths of values in `...` into a single data point. For example,
#' `sum_time(c(x, y), z)` will have the same output as `sum_time(x, y, z)`.
#'
#' `sum_times()` performs a different type of sum (a vectorized one). Instead of
#' aggregate the time lengths, the function perform a paired sum between
#' elements. For example, `sum_time(c(x, y), c(w, z))` will return a vector like
#' `c(sum_time(x, w), sum_time(y, z))`. Because of that, `sum_times()` requires
#' that all objects in `...` have the same length.
#'
#' ## Linear versus circular sum
#'
#' Time can have different "shapes".
#'
#' If the objective is to measure the duration of an event, time is usually
#' measured considering a linear frame, with a fixed point of
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
#' encounters the origin at the end of the cycle (every 24 hours or 86400
#' seconds).
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
#' -----|---------------|---------------|---------------|----->
#'     0h              12h              0h             12h
#'   origin                           origin
#' ```
#'
#' Note that now the origin is not fixed, but cyclical.
#'
#' `sum_time()` and `sum_times()` can both operate in either a linear or a
#' circular fashion. If `cycle = NULL` (default), the function will use a
#' linear approach. Else, the function will use a circular approach relative to
#' the cycle length (e.g, `cycle = 86400` (1 day)).
#'
#' ## `POSIXt` objects
#'
#' `POSIXt` values in `...` will be stripped of their dates. Only the time will
#' be considered.
#'
#' Both `POSIXct` and `POSIXlt` are objects that inherits the class `POSIXt`.
#' Learn more about it in [base::DateTimeClasses].
#'
#' ## `Period` objects
#'
#' `Period` objects are a special type of object developed by the
#' [lubridate][lubridate::lubridate-package] team that represents "human units",
#' ignoring possible time irregularities. That is to say that 1 day as `Period`
#' will always represent 1 day in the timeline. `sum_time()` and `sum_times()`
#' ignores that property of `Period` objects, treating them like objects of
#' class `Duration`.
#'
#' ## `Interval` objects
#'
#' By using `Interval` objects in `...`, `sum_time()` and `sum_times()` will
#' consider only their time lengths. That is, the amount of seconds of the
#' intervals.
#'
#' ## Timeline irregularities
#'
#' This function does not take into account timeline irregularities (e.g.,
#' leap years, DST, leap seconds). This may not be an issue for most people, but
#' it must be considered when doing time arithmetic.
#'
#' @param ... Objects belonging to one of the following classes: `Duration`,
#'   `Period`, `difftime`, `hms`, `POSIXct`, `POSIXlt`, or `Interval`.
#' @param cycle (optional) a number indicating the cycle length in seconds. If
#'   `NULL` the function will perform a linear sum (see Details to learn
#'   more) (default: `NULL`).
#' @param na_rm (optional) a `logical` value indicating if the function must
#'   remove `NA` values while performing the sum (default: `FALSE`).
#'
#' @return
#'
#' * If `cycle = NULL`, an `hms` object with a linear sum of the time from
#' objects in `...`.
#' * If `cycle != NULL`, an `hms` object with a circular sum of the time
#' from objects in `...`.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' ## Non-vectorized sum in an linear time frame
#'
#' x <- c(as.POSIXct("2020-01-01 15:00:00"), as.POSIXct("1999-05-04 17:30:00"))
#' y <- lubridate::as.interval(lubridate::dhours(7), as.Date("1970-05-08"))
#' sum_time(x, y)
#' #> 39:30:00 # Expected
#'
#' ## Non-vectorized sum in a circular time frame of 24 hours
#'
#' x <- c(lubridate::hours(25), lubridate::dhours(5), lubridate::minutes(50))
#' sum_time(x, cycle = lubridate::ddays())
#' #> 06:50:00 # Expected
#'
#' x <- c(hms::parse_hm("00:15"), hms::parse_hm("02:30"), hms::as_hms(NA))
#' sum_time(x, cycle = lubridate::ddays())
#' #> NA # Expected
#' sum_time(x, cycle = lubridate::ddays(), na_rm = TRUE)
#' #> 02:45:00 # Expected
#'
#' ## Vectorized sum in an linear time frame
#'
#' x <- c(lubridate::dhours(6), NA)
#' y <- c(hms::parse_hm("23:00"), hms::parse_hm("10:00"))
#' sum_times(x, y)
#' #> 29:00:00 # Expected
#' #>       NA # Expected
#' sum_times(x, y, na_rm = TRUE)
#' #> 29:00:00 # Expected
#' #> 10:00:00 # Expected
#'
#' ## Vectorized sum in a circular time frame of 24 hours
#'
#' x <- c(lubridate::dhours(6), NA)
#' y <- c(hms::parse_hm("23:00"), hms::parse_hm("10:00"))
#' sum_times(x, y, cycle = lubridate::ddays())
#' #> 05:00:00 # Expected
#' #>       NA # Expected
#' sum_times(x, y, cycle = lubridate::ddays(), na_rm = TRUE)
#' #> 05:00:00 # Expected
#' #> 10:00:00 # Expected
sum_time <- function(..., cycle = NULL, na_rm = FALSE) {
    build_sum(..., vectorize = FALSE, cycle = cycle, na_rm = na_rm)
}

#' @rdname sum_time
#' @export
sum_times <- function(..., cycle = NULL, na_rm = FALSE) {
    build_sum(..., vectorize = TRUE, cycle = cycle, na_rm = na_rm)
}

build_sum <- function(..., vectorize = FALSE, cycle = NULL, na_rm = FALSE) {
    out <- list(...)

    assert_custom <- function(x) {
        classes <- c("Duration", "Period", "difftime", "hms", "POSIXct",
                     "POSIXlt", "Interval")

        checkmate::assert_multi_class(x, classes)
    }

    checkmate::assert_flag(vectorize)
    checkmate::assert_number(cycle, lower = 0, null.ok = TRUE)
    checkmate::assert_flag(na_rm)
    lapply(out, assert_custom)

    if (isTRUE(vectorize) &&
        !(length(unique(vapply(out, length, integer(1)))) == 1)) {
        cli::cli_abort("All values in '...' must have the same length.")
    }

    normalize <- function(x) {
        if (lubridate::is.POSIXt(x) || lubridate::is.difftime(x)) {
            as.numeric(hms::as_hms(x))
        } else {
            as.numeric(x)
        }
    }

    out <- lapply(out, normalize)

    if (isTRUE(na_rm)) {
        out <- lapply(out, function(x) dplyr::if_else(is.na(x), 0, x))
    }

    if (isTRUE(vectorize)) {
        out <- Reduce("+", out)
    } else {
        out <- do.call("c", out)
        out <- sum(out, na.rm = na_rm)
    }

    if (!is.null(cycle))  out <- out %% cycle

    hms::hms(out)
}
