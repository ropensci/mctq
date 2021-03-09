#' Find the shortest interval between two hours
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `shortest_interval()` finds and return the shortest interval between two
#' `hms` or `POSIXt` objects hours.
#'
#' `longer_interval()` do the inverse of `shortest_interval()`, _i.e_
#' finds the longer interval between two hours. It's just a wrapper for
#' `shortest_interval(x, y, class, inverse = TRUE)`.
#'
#' @details
#'
#' ## The two intervals problem
#'
#' Given two hours, `x` and `y`, in a two-day timeline, without date references,
#' there will be always two possible intervals between them, as illustrated
#' below.
#'
#' To figure out what interval is the  shortest or the longer,
#' `shortest_interval()` checks two scenarios: 1. When `x` comes before `y`; and
#' 2. when `x` comes after `y`. This only works if `x` value is smaller than
#' `y`, therefore, the function will make sure to swap `x` and `y` values if the
#' latter assumption is not true.
#'
#' Because `shortest_interval()` objective is to find the shortest interval, if
#' `x` and `y` are equal, the shortest interval will have a length of 0 hours,
#' resulting in a interval from `x` to `x`. But, if `inverse = TRUE` or
#' `longer_interval()` is used instead, the latter condition will return a
#' interval with 24 hours of length (from `x` to `x` + 1 day).
#'
#' In cases when `x` and `y` distance themselves by 12 hours, there will be no
#' shortest or longer interval (they are equal). In this cases,
#' `shortest_interval()` and `longer_interval()` will return the same value
#' (a interval of 12 hours).
#'
#' ```
#'              day 1                        day 2
#'      x                  y         x                  y
#'    06:00              22:00     06:00              22:00
#' -----|------------------|---------|------------------|----->
#'               16h           8h             16h
#'           longer int.  shortest int.   longer int.
#'
#'               day 1                      day 2
#'      y                   x       y                   x
#'    13:00               08:00   13:00               08:00
#' -----|-------------------|-------|-------------------|----->
#'               19h           5h            19h
#'           longer int.  shortest int.  longer int.
#'
#'     x,y             x,y             x,y             x,y
#'      x               y               x               y
#'    10:00           10:00           10:00           10:00
#' -----|---------------|---------------|---------------|----->
#'     0h              0h              0h              0h
#'             24h             24h             24h
#'
#'               day 1                      day 2
#'      y               x               y               x
#'    12:00           00:00           12:00           00:00
#' -----|---------------|---------------|---------------|----->
#'             12h             12h             12h
#' ```
#'
#' ## Class requirements
#'
#' The `mctq` package works with a set of object classes specially created to
#' hold time values. These classes can be found in the [hms][hms::hms-package]
#' and [lubridate][lubridate::lubridate-package] packages. If your data do not
#' conform to the object classes required, you can use [mctq::convert()] to
#' convert it.
#'
#' ## `class` argument
#'
#' `shortest_interval()` is integrated with [mctq::convert()]. That way you
#' can choose what class of object your prefer as output.
#'
#' Valid `class` values are: `"Duration"`, `"Period"`, `"difftime"`, `"hms"`,
#' and `"Interval"` (case insensitive).
#'
#' ## `POSIXt` objects
#'
#' `POSIXt` objects passed as argument to `x` or `y` will be strip of their
#' dates. Only the hours will be considered.
#'
#' @param x,y A `hms` or `POSIXt` object.
#' @param class (optional) a string indicating the object class of the output
#'   (default: `"hms"`).
#' @param inverse (optional) a `logical` value indicating if the function must
#'   return a inverse output, _i.e_ the longer interval between `x` and `y`.
#' @param quiet (optional) a `logical` value indicating if warnings or messages
#'   must be suppressed (default: `FALSE`).
#'
#' @return An object, of class indicated on `class`, with the shortest or
#'   longer interval (if `inverse = TRUE` or `longer_interval()` is used)
#'   between `x` and `y`.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' ## __ Scalar example __
#' x <- hms::parse_hm("23:00")
#' y <- hms::parse_hm("01:00")
#' shortest_interval(x, y)
#' #> 02:00:00 # Expected
#'
#' x <- lubridate::as_datetime("1985-01-15 12:00:00")
#' y <- lubridate::as_datetime("2020-09-10 12:00:00")
#' shortest_interval(x, y)
#' #> 00:00:00 # Expected
#'
#' ## __ Vector example __
#' x <- c(hms::parse_hm("15:30"), hms::parse_hm("21:30"))
#' y <- c(hms::parse_hm("19:30"), hms::parse_hm("04:00"))
#' shortest_interval(x, y)
#' #> 04:00:00 # Expected
#' #> 06:30:00 # Expected
#'
#' ## __ Finding the longer interval between two hours __
#' x <- lubridate::parse_date_time("01:10:00", "HMS")
#' y <- lubridate::parse_date_time("11:45:00", "HMS")
#' shortest_interval(x, y, inverse = TRUE)
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
#' longer_interval(x, y, "Duration")
#' #> [1] "45000s (~12.5 hours)" # Expected
#' shortest_interval(x, y, "Period")
#' #> [1] "11H 30M 0S" # Expected
#' longer_interval(x, y, "hms")
#' #> 12:30:00" # Expected
shortest_interval <- function(x, y, class = "hms", inverse = FALSE,
                              quiet = FALSE) {
    choices <- c("Duration", "Period", "difftime", "hms", "Interval")

    checkmate::assert_multi_class(x, c("hms", "POSIXct", "POSIXlt"))
    checkmate::assert_multi_class(y, c("hms", "POSIXct", "POSIXlt"))
    assert_identical(x, y, type = "length")
    checkmate::assert_numeric(as.numeric(hms::as_hms(x)),
                              lower = 0, upper = 86400)
    checkmate::assert_numeric(as.numeric(hms::as_hms(y)),
                              lower = 0, upper = 86400)
    checkmate::assert_choice(tolower(class), tolower(choices))
    checkmate::assert_flag(inverse)

    class <- tolower(class)

    x <- flat_posixt(convert(x, "posixct", quiet = TRUE))
    y <- flat_posixt(convert(y, "posixct", quiet = TRUE))

    list2env(swap_if(x, y, "x > y"), envir = environment())

    x1_y1_interval <- lubridate::interval(x, y)
    y1_x2_interval <- lubridate::interval(y, x + lubridate::days())

    if (isFALSE(inverse)) {
        out <- dplyr::case_when(
            is.na(x) | is.na(y) ~ lubridate::as.interval(NA),
            x == y ~ lubridate::as.interval(lubridate::hours(0), x),
            x1_y1_interval <= y1_x2_interval ~ x1_y1_interval,
            x1_y1_interval > y1_x2_interval ~ y1_x2_interval,
        )
    } else {
        out <- dplyr::case_when(
            is.na(x) | is.na(y) ~ lubridate::as.interval(NA),
            x == y ~ lubridate::as.interval(lubridate::hours(24), x),
            x1_y1_interval >= y1_x2_interval ~ x1_y1_interval,
            x1_y1_interval < y1_x2_interval ~ y1_x2_interval,
        )
    }

    if (class == "interval") {
        if (any(x1_y1_interval == y1_x2_interval, na.rm = TRUE)) {
            flags <- which(x1_y1_interval == y1_x2_interval)
            shush(warning(
                "Element(s) ", inline_collapse(flags), " of 'x' ",
                "and 'y' have intervals equal to 12 hours, i.e. ",
                "there's no shortest or longer interval ",
                "between the two hours (they are equal). Only one ",
                "possible interval was returned.",
                call. = FALSE), quiet = quiet)
        }

        out
    } else {
        convert(out, class, quiet = TRUE)
    }
}

#' @rdname shortest_interval
#' @export
longer_interval <- function(x, y, class = "hms", quiet = FALSE) {
    shortest_interval(x, y, class, inverse = TRUE, quiet = quiet)
}
