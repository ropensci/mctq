#' Find the shortest interval between two hours
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `shortest_interval()` finds and return the shortest interval between two
#' `hms` or `POSIXt` objects. This is useful for time arithmetic, because
#' there's always two possible intervals between two hour values with no date
#' reference.
#'
#' @details
#'
#' ## The two intervals problem
#'
#' Given two hours, `x` and `y`, without date references, there will be always
#' two possible intervals between them, as illustrated below.
#'
#' To figure out what interval is the  shortest or longer, `shortest_interval()`
#' checks two scenarios: 1. When `x` comes before `y`; and 2. when `x` comes
#' after `y`. This only works if `x` value is smaller than `y`, therefore, the
#' function will make sure to swap `x` and `y` values if the latter assumption
#' is not true.
#'
#' Because `shortest_interval()` objective is to find the shortest interval, if
#' `x` and `y` are equal, the shortest interval will have a length of 0 hours,
#' resulting in a interval from `x` to `x`. But, if `inverse = TRUE` or
#' `longer_interval()` is use instead, the latter condition will return a
#' interval with 24 hours of length (from `x` to `x` + 1 day).
#'
#' ```
#'              day 1                        day 2
#'      x                  y         x                  y
#'    06:00              22:00     06:00              22:00
#' -----|------------------|---------|------------------|----->
#'               16h           8h             16h
#'           longer int.   shorter int.    longer int.
#'
#'               day 1                      day 2
#'      y                   x       y                   x
#'    13:00               08:00   13:00               08:00
#' -----|-------------------|-------|-------------------|----->
#'               19h           5h            19h
#'           longer int.   shorter int.   longer int.
#'
#'     x,y             x,y             x,y             x,y
#'      x               y               x               y
#'    10:00           10:00           10:00           10:00
#' -----|---------------|---------------|---------------|----->
#'     0h              0h              0h              0h
#'             24h             24h             24h
#' ```
#'
#' ## `class` argument
#'
#' `shortest_interval()` is integrated with [mctq::convert_to()]. That way you
#' can choose what class of object will prefer for output.
#'
#' Valid `class` values are: `"Duration"`, `"Period"`, `"hms"`, `"POSIXct"`,
#' `"POSIXlt"`, and `"Interval"` (case insensitive).
#'
#' ## `POSIXt` objects
#'
#' `POSIXt` values passed as argument to `x` or `y` will be strip of their
#' dates. Only the hours will be considered.
#'
#' ## `NA` values
#'
#' `shortest_interval()` will return `NA` if `x` or `y` are `NA`.
#'
#' ## `longer_interval()` function
#'
#' `longer_interval()` do the inverse of `shortest_interval()`, _i.e_
#' finds the longer interval between two hours. It's just a wrapper for
#' `shortest_interval(x, y, class, inverse = TRUE)`.
#'
#' @param x,y A `hms` or `POSIXt` vector.
#' @param class (optional) a string indicating the object class of the output.
#' @param inverse (optional) a logical value indicating if the function must
#'   return a inverse output, _i.e_ the longer interval between `x` and `y`.
#'
#' @return A `hms` object, or a type of object indicated on `class`, with the
#'   shortest interval between `x` and `y`.
#' @family utility functions
#' @export
#'
#' @examples
#' ## __ Finding the shortest interval between two hour values __
#' x <- hms::parse_hms("23:00:00")
#' y <- hms::parse_hms("01:00:00")
#' shortest_interval(x, y)
#' #> 02:00:00 # Expected
#'
#' x <- lubridate::as_datetime("1985-01-15 12:00:00")
#' y <- lubridate::as_datetime("2020-09-10 12:00:00")
#' shortest_interval(x, y)
#' #> 00:00:00 # Expected
#'
#' ## __ Finding the longer interval between two hour values __
#' x <- lubridate::parse_date_time("01:10:00", "HMS")
#' y <- lubridate::parse_date_time("11:45:00", "HMS")
#' longer_interval(x, y)
#' #> 13:25:00 # Expected
#'
#' x <- lubridate::as_datetime("1915-02-14 05:00:00")
#' y <- lubridate::as_datetime("1970-07-01 05:00:00")
#' longer_interval(x, y)
#' #> 24:00:00 # Expected
#'
#' ## __ Changing the output object class __
#' x <- as.POSIXct("1988-10-05 02:00:00")
#' y <- as.POSIXlt("2100-05-07 13:30:00")
#' shortest_interval(x, y, "Interval")
#' #> [1] 1970-01-01 02:00:00 UTC--1970-01-01 13:30:00 UTC # Expected
#'
#' longer_interval(x, y, "Duration")
#' #> [1] "45000s (~12.5 hours)" # Expected
#' shortest_interval(x, y, "Period")
#' #> [1] "11H 30M 0S" # Expected
#' longer_interval(x, y, "POSIXct")
#' #> [1] "1970-01-01 12:30:00 UTC" # Expected
shortest_interval <- function(x, y, class = "hms", inverse = FALSE) {

    # Check arguments -----

    choices <- c("Duration", "Period", "hms", "POSIXct", "POSIXlt", "Interval")

    checkmate::assert_multi_class(x, c("hms", "POSIXct", "POSIXlt"))
    checkmate::assert_multi_class(y, c("hms", "POSIXct", "POSIXlt"))
    assert_identical(x, y, type = "length")
    checkmate::assert_numeric(lubridate::hour(x), lower = 0, upper = 23)
    checkmate::assert_numeric(lubridate::hour(y), lower = 0, upper = 23)
    checkmate::assert_choice(tolower(class), tolower(choices))
    checkmate::assert_flag(inverse)

    # Set values -----

    class <- tolower(class)

    # Convert `x` and `y` -----

    x <- flat_posixt(convert_to(x, "posixct"))
    y <- flat_posixt(convert_to(y, "posixct"))

    list2env(swap_if(x, y, "x > y"), envir = environment())

    # Create intervals -----

    x1_y1_interval <- lubridate::interval(x, y)
    y1_x2_interval <- lubridate::interval(y, x + lubridate::days())

    # Find shortest interval -----

    if (isFALSE(inverse)) {
        out <- dplyr::case_when(
            is.na(x) | is.na(y) ~ lubridate::as.interval(NA),
            x == y ~ lubridate::as.interval(lubridate::hours(0), x),
            x1_y1_interval < y1_x2_interval ~ x1_y1_interval,
            x1_y1_interval > y1_x2_interval ~ y1_x2_interval,
        )
    } else {
        out <- dplyr::case_when(
            is.na(x) | is.na(y) ~ lubridate::as.interval(NA),
            x == y ~ lubridate::as.interval(lubridate::hours(24), x),
            x1_y1_interval > y1_x2_interval ~ x1_y1_interval,
            x1_y1_interval < y1_x2_interval ~ y1_x2_interval,
        )
    }

    # Return output -----

    if (class == "interval") {
        out
    } else {
        convert_to(out, class)
    }

}

#' @rdname shortest_interval
#' @export
longer_interval <- function(x, y, class = "hms") {

    shortest_interval(x, y, class, inverse = TRUE)

}
