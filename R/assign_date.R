#' Assign dates to two sequential hours
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `assign_date()` assign dates to two sequential hours. It can facilitate
#' time arithmetic by locating time values without a date reference on a
#' timeline.
#'
#' @details
#'
#' ## Class requirements
#'
#' The `mctq` package works with a set of object classes specially created to
#' hold time values. These classes can be found in the
#' [lubridate][lubridate::lubridate-package] and [hms][hms::hms-package]
#' packages. Please refer to those package documentations to learn more about
#' them.
#'
#' ## `ambiguity` argument
#'
#' In cases when `start` is equal to `end`, there are two possibilities of
#' intervals between the two hours (ambiguity). That's because `start` and `end`
#' can be at the same point in time or they can distance themselves by one day,
#' considering a two-day timeline.
#'
#' ```
#'  start,end       start,end       start,end       start,end
#'    start            end            start            end
#'    10:00           10:00           10:00           10:00
#' -----|---------------|---------------|---------------|----->
#'     0h              0h              0h              0h
#'             24h             24h             24h
#' ```
#'
#' You must instruct `assign_date()` on how to deal with this problem if it
#' occurs. There are three options to choose.
#'
#' * `ambiguity = 0`: to consider the interval between `start` and `end` as 0
#' hours, i.e., `start` and `end` are located at the same point in time
#' (default).
#' * `ambiguity = 24`: to consider the interval between `start` and `end` as 24
#' hours, i.e., `start` and `end` distance themselves by one day.
#' * `ambiguity = NA`: to disregard these cases, assigning `NA` as value.
#'
#' ## Base date and timezone
#'
#' `assign_date()` uses the
#' [Unix epoch](https://en.wikipedia.org/wiki/Unix_time) (1970-01-01) date as
#' the start date for creating intervals.
#'
#' The output will always have `"UTC"` set as timezone. Learn more about
#' time zones in [`?timezone`][base::timezone].
#'
#' ## `POSIXt` objects
#'
#' [`POSIXt`][base::as.POSIXct()] objects passed as argument to `start` or `end`
#' will be stripped of their dates. Only the time will be considered.
#'
#' Both [`POSIXct`][base::as.POSIXct()] and [`POSIXlt`][base::as.POSIXlt()] are
#' objects that inherits the class `POSIXt`. Learn more about it in
#' [`?DateTimeClasses`][base::DateTimeClasses].
#'
#' ## `NA` values
#'
#' `assign_date()` will return an [`Interval`][lubridate::interval()] `NA`-`NA`
#' if `start` or `end` are `NA`.
#'
#' @param start,end An [`hms`][hms::hms()] or [`POSIXt`][base::as.POSIXct()]
#'   object indicating the start or end hour.
#' @param ambiguity (optional) a [`numeric`][numeric()] or `NA` value to
#'   instruct `assign_date()` on how to deal with ambiguities. See the Details
#'   section to learn more (default: `0`).
#'
#' @return A `start`--`end` [`Interval`][lubridate::interval()] object.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' ## Scalar example
#'
#' start <- hms::parse_hms("23:11:00")
#' end <- hms::parse_hms("05:30:00")
#' assign_date(start, end)
#' #> [1] 1970-01-01 23:11:00 UTC--1970-01-02 05:30:00 UTC # Expected
#'
#' start <- hms::parse_hms("10:15:00")
#' end <- hms::parse_hms("13:25:00")
#' assign_date(start, end)
#' #> [1] 1970-01-01 10:15:00 UTC--1970-01-01 13:25:00 UTC # Expected
#'
#' start <- hms::parse_hms("05:42:00")
#' end <- hms::as_hms(NA)
#' assign_date(start, end)
#' #> [1] NA--NA # Expected
#'
#' ## Vector example
#'
#' start <- c(hms::parse_hm("09:45"), hms::parse_hm("20:30"))
#' end <- c(hms::parse_hm("21:15"), hms::parse_hm("04:30"))
#' assign_date(start, end)
#' #> [1] 1970-01-01 09:45:00 UTC--1970-01-01 21:15:00 UTC # Expected
#' #> [2] 1970-01-01 20:30:00 UTC--1970-01-02 04:30:00 UTC # Expected
#'
#' ## To assign a 24 hours interval to ambiguities
#'
#' start <- lubridate::as_datetime("1985-01-15 12:00:00")
#' end <- lubridate::as_datetime("2020-09-10 12:00:00")
#' assign_date(start, end, ambiguity = 24)
#' #> [1] 1970-01-01 12:00:00 UTC--1970-01-02 12:00:00 UTC # Expected
assign_date <- function(start, end, ambiguity = 0) {
    checkmate::assert_multi_class(start, c("hms", "POSIXt"))
    checkmate::assert_multi_class(end, c("hms", "POSIXt"))
    assert_identical(start, end, type = "length")
    checkmate::assert_numeric(as.numeric(hms::as_hms(start)),
                              lower = 0, upper = 86400)
    checkmate::assert_numeric(as.numeric(hms::as_hms(end)),
                              lower = 0, upper = 86400)
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
