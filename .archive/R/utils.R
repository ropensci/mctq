#' @family utility functions
#' @noRd
check_that_ <- function(data, ...) {

    checkmate::assert_data_frame(data)

    confront <- validate::check_that(data, ...)
    validate::summary(confront)

}

#' Change dates by time of day
#'
#' @description
#'
#' `midday_change()` changes the dates of `POSIXt` objects accordingly to the
#' time of day registered in the object values. The function do this by flatting
#' the date to `1970-01-01` and them adding a day if the hour is lower than 12.
#'
#' `hms` objects are also allowed. In this case `midday_change()` will convert
#' all values to `POSIXct` and do the same operations described above.
#'
#' This method can help with time charting, allowing a continuity after the
#' midnight hour.
#'
#' `mdc()` is just a shorter version of `midday_change()` for convenience.
#'
#' @param x A `hms` or `POSIXt` object.
#'
#' @return A `POSIXct`object when `x` is `hms` or `POSIXct`, or a `POSIXlt`
#'   object when `x` is `POSIXlt`.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' x <- lubridate::ymd_hms("2021-01-15 20:02:01") # hour > 12h
#' midday_change(x)
#' #> [1] "1970-01-01 20:02:01 UTC" # Expected
#'
#' x <- lubridate::ymd_hms("1987-12-24 07:45:32") # hour < 12h
#' midday_change(x)
#' #> [1] "1970-01-02 07:45:32 UTC" # Expected
midday_change = function(x) {
    checkmate::assert_multi_class(x, c("hms", "POSIXct", "POSIXlt"))

    x <- flat_posixt(convert(x, "posixct"))

    x <- dplyr::case_when(
        lubridate::hour(x) < 12 ~ change_day(x, 2),
        TRUE ~ x
    )

    x
}

#' @rdname midday_change
#' @noRd
mdc <- function(x) midday_change(x)

#' Flat dates of `POSIXt` objects
#'
#' @description
#'
#' `flat_posixt()` changes the dates of `POSIXt` objects to a base reference.
#' This can be use to standardizing a point of origin to time values.
#'
#' Please note that, accordingly to `?base::POSIXct`, not all OSes can
#' correctly convert times before 1902 or after 2037. It's best to maintain the
#' base date as the [UNIX epoch](https://en.wikipedia.org/wiki/Unix_time),
#' _i.e_ `"1970-01-01"` (default).
#'
#' @param x A `POSIXt` object.
#' @param force_utc (optional) a `logical` value indicating if the time zone of
#'   `x` must be forced to `"UTC"` (default: `TRUE`).
#' @param base (optional) a string indicating the base date for flatting `x`
#'   in `%Y-%m-%d` format (default: `"1970-01-01"`).
#'
#' @return An object of the same `POSIXt` class type as `x` with dates and time
#'   zone transformed.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' x <- lubridate::ymd_hms("2001-09-15 11:15:05", tz = "EST")
#' flat_posixt(x)
#' #> [1] "1970-01-01 11:15:05 UTC" # Expected
#' flat_posixt(x, force_utc = FALSE)
#' #> [1] "1970-01-01 11:15:05 EST" # Expected
#' flat_posixt(x, base = "2020-01-01")
#' #> [1] "2020-01-01 11:15:05 UTC"
flat_posixt = function(x, force_utc = TRUE, base = "1970-01-01") {
    assert_posixt(x, null.ok = FALSE)
    checkmate::assert_flag(force_utc)
    checkmate::assert_string(base)

    lubridate::date(x) <- base

    if (isTRUE(force_utc)) {
        x <- lubridate::force_tz(x, "UTC")
    }

    x
}

#' Change date of `Date` or `POSIXt` objects
#'
#' @description
#'
#' `change_date()` help you change dates of `Date` or `POSIXt` objects without
#' the need for a direct assignment.
#'
#' @param x A `Date` or `POSIXt` object.
#' @param date A `Date` or `character` object of length 1 indicating the date
#'   for `x`.
#'
#' @return An object of the same class as `x` with the indicated date in `date`.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' change_date(lubridate::as_date("1888-02-07"), "2020-01-01")
#' #> [1] "2020-01-01" # Expected
#' change_date(lubridate::ymd_hms("1987-12-24 07:45:32"), as.Date("1999-02-25"))
#' #> [1] "1999-02-25 07:45:32 UTC" # Expected
change_date <- function(x, date) {
    classes <- c("Date", "POSIXct", "POSIXlt")
    checkmate::assert_multi_class(x, classes, null.ok = FALSE)

    classes <- c("character", "Date")
    checkmate::assert_multi_class(date, classes, null.ok = FALSE)
    assert_length_one(date)

    lubridate::date(x) <- date

    x
}

#' Change day of `Date` or `POSIXt` objects
#'
#' @description
#'
#' `change_day()` help you change days of `Date` or `POSIXt` objects without the
#' need for a direct assignment.
#'
#' @param x A `Date` or `POSIXt` object.
#' @param day A number between 1-31 indicating the new day for `x`.
#'
#' @return An object of the same class as `x` with the indicated day in `day`.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' change_day(lubridate::as_date("1888-02-07"), 20)
#' #> [1] "1888-02-20" # Expected
#' change_day(lubridate::ymd_hms("1987-12-24 07:45:32"), 15)
#' #> [1] "1987-12-15 07:45:32 UTC" # Expected
change_day <- function(x, day) {
    classes <- c("Date", "POSIXct", "POSIXlt")

    checkmate::assert_multi_class(x, classes, null.ok = FALSE)
    checkmate::assert_number(day, lower = 1, upper = 31)

    if (any(lubridate::month(x) %in% c(4, 6, 9, 11)) && day > 30) {
        rlang::abort(paste0(
            "You can't assign more than 30 days to April, June, ",
            "September, or November."
        ))
    }

    if (any(lubridate::month(x) == 2 & !lubridate::leap_year(x)) && day > 28) {
        rlang::abort(paste0(
            "You can't assign more than 28 days to February in ",
            "non-leap years."
        ))
    }

    if (any(lubridate::month(x) == 2 & lubridate::leap_year(x)) && day > 29) {
        rlang::abort(paste0(
            "You can't assign more than 29 days to February in a leap year"
        ))
    }

    lubridate::day(x) <- day

    x
}

#' Test if an object inherits a class from a set of date/time classes
#'
#' @description
#'
#' `is_time()` returns a boolean flag testing for objects of class `Duration`,
#' `Period`, `difftime`, `hms`, `Date`, `POSIXct`, `POSIXlt`, or `Interval`.
#'
#' @param x Any kind of R object.
#' @param rm (optional) a character vector indicating names of object classes to
#'   remove from the test (case sensitive) (default: `NULL`).
#'
#' @return If `rm` is `NULL`, a boolean flag checking if `x` inherits a
#'   `Duration`, `Period`, `difftime`, `hms`, `Date`, `POSIXct`, `POSIXlt`, or
#'   `Interval` class. Else, the same as previous, but without the classes
#'   indicated in `rm`.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' is_time(as.Date("2020-01-01"))
#' #> [1] TRUE # Expected
#' is_time(as.Date("2020-01-01"), rm = "Date")
#' #> [1] FALSE # Expected
#' is_time(datasets::iris)
#' #> [1] FALSE # Expected
is_time <- function(x, rm = NULL) {
    checkmate::assert_character(rm, any.missing = FALSE, null.ok = TRUE)

    classes <- c("Duration", "Period", "difftime", "hms", "Date", "POSIXct",
                 "POSIXlt", "Interval")

    if (!is.null(rm)) {
        rm <- paste0("^", rm, "$", collapse = "|")
        classes <- stringr::str_subset(classes, rm, negate = TRUE)
    }

    # if (circular::is.circular(x) && !("circular" %in% rm)) {
    #     return(TRUE)
    # }

    checkmate::test_multi_class(x, classes)
}

#' Collapse class names
#'
#' @description
#'
#' `class_collapse()` is a utility function to help build return messages with
#' [glue::glue()]. It collapses the value of `class(x)` with a `/` and single
#' quote it.
#'
#' @param x Any kind of R object.
#'
#' @return A string with `class(x)` value collapsed with `/` encapsulated with
#'   single quotes.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' class_collapse("")
#' #> [1] "'character'" # Expected
#' class_collapse(1)
#' #> [1] "'numeric'" # Expected
#' class_collapse(hms::parse_hm("00:00"))
#' #> [1] "'hms/difftime'" # Expected
class_collapse <- function(x) {
    glue::single_quote(glue::glue_collapse(class(x), sep = '/'))
}
