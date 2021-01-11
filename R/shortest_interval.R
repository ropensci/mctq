#' Find the shortest interval between two hours
#'
#' @description
#'
#' `shortest_interval()` finds the shortest interval between two `hms` or
#' `POSIXt` objects. This is useful for linear time arithmetic, because there
#' is always two possible intervals between two hour values with no date
#' reference.
#'
#' @details
#'
#' ## Two intervals problem
#'
#' Given two hours, `x` and `y`, without date references, there always will be
#' two possible intervals between them, as illustrated below.
#'
#' To figure out what interval is the  shortest or longer, `shortest_interval()`
#' checks two scenarios: 1. When `x` comes before `y`; and 2. when `x`
#' comes after `y`. This only works if `x` value is smaller than `y`. This
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
#'            2nd int.      1st int.        2nd int.
#'
#'               day 1                      day 2
#'      y                   x       y                   x
#'    13:00               08:00   13:00               08:00
#' -----|-------------------|-------|-------------------|----->
#'               19h           5h            19h
#'             2nd int.     1st int.       2nd int.
#'
#'     x,y            x,y            x,y            x,y
#'    10:00          10:00          10:00          10:00
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
#' ## `longer_interval()` function
#'
#' `longer_interval()` do the inverse of `shortest_interval()`, _i.e_
#' finds the longer interval between two hours. It's just a wrapper for
#' `shortest_interval(x, y, class, inverse = TRUE)`.
#'
#' @param x,y A `hms` or `POSIXt` vector.
#' @param class A string indicating the object class of the output.
#' @param inverse A logical value indicating if the function must return the
#' inverse output, _i.e_ the longer interval between `x` and `y`.
#'
#' @family Time arithmetic
#' @export
#'
#' @examples
#' x <- hms::parse_hms("23:00:00")
#' y <- hms::parse_hms("01:00:00")
#' shortest_interval(x, y)
#' #> 02:00:00 # Expected
#' longer_interval(x, y)
#' #> 22:00:00 # Expected
#' shortest_interval(x, y, "Interval")
#' #> [1] 0000-01-01 23:00:00 UTC--0000-01-02 01:00:00 UTC # Expected
#' longer_interval(x, y, "Interval")
#' #> [1] 0000-01-01 01:00:00 UTC--0000-01-01 23:00:00 UTC # Expected
shortest_interval <- function(x, y, class = "hms", inverse = FALSE) {

    choices <- c("Duration", "Period", "hms", "POSIXct", "POSIXlt", "Interval")

    checkmate::check_multi_class(x, c("hms", "POSIXct", "POSIXlt"))
    checkmate::check_multi_class(y, c("hms", "POSIXct", "POSIXlt"))
    check_identical(x, y, "length")
    checkmate::assert_numeric(lubridate::hours(x), lower = 0, max.len = 23)
    checkmate::assert_numeric(lubridate::hours(y), lower = 0, max.len = 23)
    checkmate::assert_choice(tolower(class), tolower(choices))
    checkmate::assert_flag(inverse)

    class <- tolower(class)
    x <- flat_posixt(convert_to(x, "posixct"))
    y <- flat_posixt(convert_to(y, "posixct"))

    list2env(swap_if(x, y, "x > y"), envir = environment())

    x1_y1_interval <- lubridate::interval(x, y)
    y1_x2_interval <- lubridate::interval(y, x + lubridate::days())

    if (isFALSE(inverse)) {
        out <- dplyr::case_when(
            x1_y1_interval < y1_x2_interval ~ x1_y1_interval,
            y1_x2_interval < x1_y1_interval ~ y1_x2_interval,
            TRUE ~ lubridate::as.interval(lubridate::days(0), x)
        )
    } else {
        out <- dplyr::case_when(
            x1_y1_interval > y1_x2_interval ~ x1_y1_interval,
            y1_x2_interval > x1_y1_interval ~ y1_x2_interval,
            TRUE ~ lubridate::as.interval(lubridate::days(24), x)
        )
    }

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
