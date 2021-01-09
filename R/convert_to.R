#' Converts a R object to another
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `convert_to` converts a R object to another object of a predefined class. It
#' have the mission to facilitate conversions between any kind of R object in a
#' simple and fast way.
#'
#' This function also supports date/time parsing and value transformations.
#' Check details section to learn more.
#'
#' @details
#'
#' `convert_to` was designed to be a simple to use function with powerful
#' applications. However, please note that it may not work for all cases.
#' Test it first before applying it to large datasets.
#'
#' To learn more about how to handle date and time objects, check chapter "Dates
#' and Times" from Wickham and Grolemund ([n.d.](https://r4ds.had.co.nz)) and
#' chapter "Technical Representation of Data" from Loo and Jonge
#' ([2018](https://bit.ly/3pVuUdt)).
#'
#' ## Wrappers
#'
#' `convert_to` has two wrappers functions for convenience.
#'
#' `convert_to_tu` was made to help make transformations from date/time objects
#' to units more easily.
#'
#' `convert_to_uu` was made to help make transformations from unit to unit
#' more easily.
#'
#' ## `class` argument
#'
#' The `class` argument indicates the desired class of the output object . At
#' the moment, it accepts the following values: `"character"`, `"integer"`,
#' `"double"`, `"numeric"`, `"Duration"`, `"Period"`, `"difftime"`, `"hms"`,
#' `"Date"`, `"POSIXct"`, and `"POSIXlt"` (case insensitive).
#'
#' ## `orders` argument
#'
#' `orders` is an optional argument to indicate date/time formats for
#' parsing `character` and `numeric` objects. This kind of parsing is mainly
#' based on [lubridate::parse_date_time()]. Please check it documentation
#' to set the right formats for your input (default: `c("HMS", "HM", "H")`).
#'
#' ## `input_unit` and `output_unit` arguments
#'
#' To be able to transform date/time objects and numeric values to decimal time,
#' radians, and degrees, `convert_to` need to know the `input_unit`
#' (default: `NULL`) of `x` and the `output_unit` (default: `NULL`).
#' Transformations of Date/time objects only need the latter.
#'
#' At the moment, valid values for this two variables are:
#'
#' * `"S"`: for decimal seconds.
#' * `"M"`: for decimal minutes.
#' * `"H"`: for decimal hours.
#' * `"d"`: for decimal days.
#' * `"W"`: for decimal weeks.
#' * `"m"`: for decimal months.
#' * `"y"`: for decimal years.
#' * `"date_decimal"`: for decimal dates
#' * `"rad"`: for radians.
#' * `"deg"`: for degrees.
#'
#' ## `tz` argument
#'
#' Time zones (tz) are used only if `class` is set to `"Date"`, `"POSIXct"`, or
#' `"POSIXlt"`. See [base::timezones] to know more about time zone values
#' (default: `"UTC"`).
#'
#' ## Parsing limitations
#'
#' `convert_to` uses [lubridate::parse_date_time()] to convert `character` and
#' `numeric` objects to date/time. Since parse_date_time() outputs a `POSIXt`
#' object, `character` and `numeric` inputs cannot have time values equal or
#' greater than 24 hours.
#'
#' That limits the set of `convert_to` applications (_e.g_ when you want to
#' parse a `character` to a `duration` object of 35 minutes and 30 seconds). To
#' get around this, some exceptions were made to orders __equal__ to `"H"`,
#' `"M"`, `"S"`, `HM`, or `HMS` (_e.g_ `convert_to(c(10, 45, 100), "duration",
#' "M")`). For `HM` and `HMS` exceptions, minutes and seconds are limited to
#' `[0-59]`, and, when hours exceeds 2 digits, a `:` must be allocated between
#' hours and minutes.
#'
#' ## Different outputs
#'
#' Your output can change according to your settings. Here are some examples.
#'
#' * When `class = "character"`
#'
#' `convert_to` will return a [base::as.character()] output if `class` is set to
#' `"character"`. When `x` is parsed for date/time, and there's no
#' indication of year, `convert_to` will return only a character string with a
#' `hms` time.
#'
#' You can also parse a `character` value and transform it direct to an unit.
#' See examples section to learn more.
#'
#' * When `class = "numeric"`
#'
#' `convert_to` will return a [base::as.numeric()] output if `class`
#' is set to `"numeric"`. For dates or date-time objects, the output will be the
#' total of seconds from the UNIX origin date (`1970-01-01 00:00:00 UTC`) (See
#' [Unix time](https://en.wikipedia.org/wiki/Unix_time) to know more). For time
#' values, the output will be the total of seconds.
#'
#' The output for `class = "numeric"` can also be different if `input_unit` and
#' `output_unit` are assigned.
#'
#' * `Interval` objects
#'
#' `Interval` objects are treated like `difftime` objects. That is,
#' `Interval` objects will be converted to the time difference of the interval.
#'
#' ## Round-off errors
#'
#' This function is not optimized to deal with
#' [round-off errors](https://en.wikipedia.org/wiki/Round-off_error).
#'
#' ## Year and month lengths
#'
#' The length of months and years can vary. For example, March have 31 days,
#' while April have have 30. Due to leap years, the same can be said to year
#' lengths.
#'
#' To address this problem, `convert_to` use as default the mean of
#' possible values for months and years, used to calculate month and year
#' durations in the lubridate package (see: [lubridate::dmonths()] and
#' [lubridate::dyears()]). If you like, you can reset this assigning other
#' values to the arguments `month_length` and `year_length`.
#'
#' `month_length` and `year_length` values must be assigned with the number of
#' seconds equivalent to unit duration. You can also assign a lubridate duration
#' object (see [lubridate::duration()]) if you like.
#'
#' ## Decimal dates
#'
#' `convert_to` can be used as a wrapper to [lubridate::decimal_date()]
#' by creating decimal dates that can be used on [lubridate::date_decimal()].
#'
#' Decimal dates are a kind of decimal time where the year corresponds to the
#' integer part of the value. The months, days, hours, minutes, and seconds
#' elements are picked so the date-time will represent the fraction of the year
#' expressed by decimal.
#'
#' Beware that decimal dates are not very precise! It cannot be used as a way
#' to store time data.
#'
#' @param x Any R object, provided that it has an assigned method.
#' @param class A string indicating the class of the output.
#' @param orders (optional) A character vector of date/time formats to parse a
#'   `x` value of class `character` or `numeric` (default:
#'   `c("HMS", "HM", "H")`).
#' @param tz (optional) A string indicating the time zone with which to
#'   convert/parse `x`.
#' @param input_unit (optional) A string indicating the unit of `x`.
#' @param output_unit (optional) A string indicating the desire output unit.
#' @param month_length (optional) A `duration` object or a non negative numeric
#'   value with the number of seconds equivalent to the month length. See topic
#'   "Year and month lengths" in the details section to learn more (default:
#'   `lubridate::dmonths()`, which is equivalent to 30.4375 days).
#' @param year_length (optional) A `duration` object or a non negative numeric
#'   value with the number of seconds equivalent to the year length. Ssee topic
#'   "Year and month lengths" in the details section to know more (default:
#'   `lubridate::dyears()`, which is equivalent to 365.25 days).
#' @param ignore_date (optional) A logical value indicating if dates must be
#' ignored from `Date` or `POSIXt` objects when converting they to `numeric`.
#' @param quiet (optional) A logical value indicating if warnings or messages
#'   are allowed with the output (default: `FALSE`).
#'
#' @return A R object of the indicated class.
#' @aliases convert_to_ convert_to__
#' @export
#'
#' @references
#'
#' Van der Loo, M., & De Jonge, E. (2018).
#' _Statistical data cleaning with applications in R_. Hooboken, NJ: John
#' Wiley & Sons. doi:
#' [10.1002/9781118897126](http://dx.doi.org/10.1002/9781118897126).
#'
#' Wickham, H, & Grolemund. (n.d.). _R for data science_. Sebastopol, CA:
#' O'Reilly Media. Retrieved from <https://r4ds.had.co.nz>.
#'
#' @examples
#' ## ** conversion of character or numeric values to date/time objects **
#' convert_to("10:00 PM", "hms", "IMp")
#' #> [1] 22:00:00 # Expected
#' convert_to("21:00 AM", "Period")
#' #> [1] "21H 0M 0S" # Expected
#' convert_to("2020-01-01 10:00:00", "Date", "ymd HMS")
#' #> [1] "2020-01-01" # Expected
#' convert_to(13, "POSIXct", "H")
#' #> [1] "0000-01-01 13:00:00 UTC" # Expected
#' convert_to("2020-01-01 12:31:05", "POSIXct", "ymd HMS", tz = "EST")
#' #> [1] "2020-01-01 12:31:05 EST" # Expected
#' convert_to("03/07/1982 13:00", "POSIXlt", "dmy HM")
#' #> [1] "1982-07-03 13:00:00 UTC" # Expected
#'
#' ## ** conversion of date/time objects to decimal time, radians or degrees **
#' convert_to("1015", "numeric", output_unit = "rad")
#' #> [1] 2.683444
#' convert_to(1.308997, "duration", input_unit = "rad")
#' #> [1] "18000.0432s (~5 hours)"
#'
#' ## ** conversion between date/time objects **
#' convert_to(lubridate::duration(120), "hms")
#' #> 00:02:00 # Expected
#' convert_to(hms::as_hms("13:45:05"), "POSIXct")
#' #> [1] "0000-01-01 13:45:05 UTC" # Expected
#' convert_to(lubridate::period(60), "POSIXct")
#' #> [1] "0000-01-01 00:01:00 UTC" # Expected
#' convert_to(as.POSIXct("2020-01-01 12:31:05", tz = "EST"),
#'                      "POSIXct")
#' #> [1] "2020-01-01 12:31:05 UTC" # Expected
#'
#' ## ** transformation between numeric units **
#' convert_to(1.308997, "numeric", input_unit = "rad", output_unit = "H")
#' #> [1] 5 # Expected
convert_to <- function(x, class, ..., quiet = FALSE) {

    # Check arguments -----

    choices <- stringr::str_to_lower(
        c("character", "integer", "double", "numeric", "Duration",
          "Period", "difftime", "hms", "Date", "POSIXct", "POSIXlt"))

    checkmate::assert_string(class)
    checkmate::assert_flag(quiet)
    checkmate::assert_choice(stringr::str_to_lower(class), choices)

    # Set method -----

    ## ah, the little hacks we have to do...

    if (any(is.na(x)) && length(x) == 1) {
        x <- as.character(NA)
        return(convert_to.character(x, class, ... = ..., quiet = quiet))
    } else if (lubridate::is.difftime(x)) {
        return(convert_to.difftime(x, class, ... = ..., quiet = quiet))
    } else if (hms::is_hms(x)) {
        return(convert_to.hms(x, class, ... = ..., quiet = quiet))
    } else if (lubridate::is.Date(x)) {
        return(convert_to.Date(x, class, ... = ..., quiet = quiet))
    } else if (lubridate::is.POSIXt(x)) {
        return(convert_to.POSIXt(x, class, ... = ..., quiet = quiet))
    } else {
        UseMethod("convert_to")
    }

}

#' @rdname convert_to
#' @export
convert_to.character <- function(x, class, orders = c("HMS", "HM", "H"),
                                 ..., tz = "UTC", input_unit = NULL,
                                 output_unit = NULL,
                                 month_length = lubridate::dmonths(),
                                 year_length = lubridate::dyears(),
                                 ignore_date = TRUE, quiet = FALSE) {

    # Check arguments -----

    checkmate::assert_character(orders, min.chars = 1, any.missing = FALSE,
                                all.missing = FALSE, min.len = 1, unique = TRUE,
                                null.ok = TRUE)
    checkmate::assert_string(tz)

    if (tolower(class) == "circular" &&
        (is.null(output_unit) || !(output_unit %in% c("H", "rad", "deg")))) {
        rlang::abort(glue::glue(
            'When `class = {class}`, `output_unit` must be `"H"`, `"rad"`, or ',
            '`"deg"`'))
    }

    # Set values -----

    class <- tolower(class)

    # Transform values -----

    if (!is.null(input_unit) && !is.null(output_unit) &&
        !any((is.na(shush(as.numeric(x)))))) {
        convert_to.numeric(as.numeric(x), class, orders,
                           tz = tz, input_unit = input_unit,
                           output_unit = output_unit,
                           month_length = month_length,
                           year_length = year_length,
                           ignore_date = ignore_date,
                           quiet = quiet)
    }

    # Parse values -----

    if (!is.null(orders)) {
        x <- parse_to_date_time(x, orders, tz, quiet)
    }

    # Fix values -----

    if (class %in% c("character", "integer", "double", "numeric") &&
        !is.null(orders) && all(lubridate::year(x) %in% c(0, 1970))) {
        x <- hms::as_hms(x)
    }

    if (class %in% c("posixct", "posixct") &&
        hms::is_hms(x)) {
        x <- flat_posixt(as.POSIXct(x))
    }

    # Check inconsistencies -----

    if (is.null(orders) &&
        !(class %in% c("character", "integer", "double", "numeric"))) {
        if (isFALSE(quiet)) {
            rlang::warn(glue::glue(
                "Non-parsed character vectors cannot be converted to {class}. ",
                "Did you forget to assign values to `orders`?"))
        }

        return(as.character(NA))
    }

    # Convert to class and return output -----

    if (class == "character") {
        as.character(x)
    } else if (class == "integer") {
        shush(as.integer(x), quiet = quiet)
    } else if (class %in% c("double", "numeric")) {
        if (is_time(x) && !is.null(output_unit) && !all(is.na(x))) {
            convert_to_unit(x, input_unit = NULL, output_unit, month_length,
                            year_length, ignore_date, quiet)
        } else {
            shush(as.numeric(x), quiet = quiet)
        }
    } else if (class == "duration") {
        lubridate::as.duration(hms::as_hms(x))
    } else if (class == "period") {
        lubridate::as.period(hms::as_hms(x))
    } else if (class == "difftime") {
        lubridate::as.difftime(lubridate::as.duration(hms::as_hms(x)))
    } else if (class == "hms") {
        hms::as_hms(x)
    } else if (class == "date") {
        if (all(is.na(x))) {
            as.Date(rep(NA, length(x)))
        } else if (!lubridate::is.POSIXt(x) ||
                   (lubridate::is.POSIXt(x) && all(lubridate::year(x) == 0))) {
            if (isFALSE(quiet)) rlang::warn("There's no date to parse in `x`.")
            as.Date(rep(NA, length(x)))
        } else {
            lubridate::as_date(lubridate::force_tz(x, tz))
        }
    } else if (class == "posixct") {
        lubridate::force_tz(as.POSIXct(x), tz = tz)
    } else if (class == "posixlt") {
        lubridate::force_tz(as.POSIXlt(x), tz = tz)
    } else {
        rlang::abort("Critical error")
    }

}

#' @rdname convert_to
#' @export
convert_to.numeric <- function(x, class, orders = c("HMS", "HM", "H"),
                               ..., tz = "UTC", input_unit = NULL,
                               output_unit = NULL,
                               month_length = lubridate::dmonths(),
                               year_length = lubridate::dyears(),
                               ignore_date = TRUE, quiet = FALSE) {

   # __UNDER DEVELOPMENT__

}

#' @rdname convert_to
#' @export
convert_to.Duration <- function(x, class, ..., tz = "UTC",
                                output_unit = NULL,
                                month_length = lubridate::dmonths(),
                                year_length = lubridate::dyears(),
                                quiet = FALSE) {

    checkmate::assert_string(tz)

    class <- tolower(class)

    if (class == "character") {
        as.character(x)
    } else if (class == "numeric") {
        as.numeric(x)
    } else if (class == "difftime") {
        lubridate::as.difftime(x)
    } else if (class == "duration") {
        lubridate::as.duration(x)
    } else if (class == "hms") {
        hms::as_hms(as.numeric(x))
    } else if (class == "period") {
        lubridate::as.period(x)
    } else if (class == "date") {
        if (isFALSE(quiet)) {
            rlang::warn("There's no date to parse")
        }
        lubridate::as_date(NA)
    } else if (class == "posixct") {
        out <- as.POSIXct(hms::as_hms(as.numeric(x)))
        lubridate::year(out) <- 0
        lubridate::force_tz(as.POSIXct(out), tz = tz)
    } else if (class == "posixlt") {
        out <- as.POSIXlt(hms::as_hms(as.numeric(x)))
        lubridate::year(out) <- 0
        lubridate::force_tz(as.POSIXlt(out), tz = tz)
    } else {
        rlang::abort("Critical error")
    }

}

#' @rdname convert_to
#' @export
convert_to.Period <- function(x, class, ..., tz = "UTC",
                              output_unit = NULL,
                              month_length = lubridate::dmonths(),
                              year_length = lubridate::dyears(),
                              quiet = FALSE) {

    convert_to.Duration(x, class, ..., tz = "UTC", output_unit = NULL,
                        month_length = lubridate::dmonths(),
                        year_length = lubridate::dyears(),
                        quiet = FALSE)

}

#' @rdname convert_to
#' @export
convert_to.difftime <- function(x, class, ..., tz = "UTC",
                                output_unit = NULL,
                                month_length = lubridate::dmonths(),
                                year_length = lubridate::dyears(),
                                quiet = FALSE) {

    convert_to.hms(x, class, ..., tz = "UTC", output_unit = NULL,
                   month_length = lubridate::dmonths(),
                   year_length = lubridate::dyears(),
                   quiet = FALSE)

}

#' @rdname convert_to
#' @export
convert_to.hms <- function(x, class, ..., tz = "UTC",
                           output_unit = NULL,
                           month_length = lubridate::dmonths(),
                           year_length = lubridate::dyears(),
                           quiet = FALSE) {

    checkmate::assert_string(tz)

    class <- tolower(class)

    if (class == "character") {
        as.character(x)
    } else if (class == "numeric") {
        as.numeric(x)
    } else if (class == "difftime") {
        lubridate::as.difftime(lubridate::as.duration(x))
    } else if (class == "duration") {
        lubridate::as.duration(x)
    } else if (class == "hms") {
        hms::as_hms(x)
    } else if (class == "period") {
        lubridate::as.period(x)
    } else if (class == "date") {
        if (isFALSE(quiet)) {
            rlang::warn("There's no date to parse")
        }
        lubridate::as_date(NA)
    } else if (class == "posixct") {
        out <- as.POSIXct(hms::as_hms(x))
        lubridate::year(out) <- 0
        lubridate::force_tz(as.POSIXct(out), tz = tz)
    } else if (class == "posixlt") {
        out <- as.POSIXlt(hms::as_hms(x))
        lubridate::year(out) <- 0
        lubridate::force_tz(as.POSIXlt(out), tz = tz)
    } else {
        rlang::abort("Critical error")
    }

}

#' @rdname convert_to
#' @export
convert_to.Date <- function(x, class, ..., tz = "UTC",
                            output_unit = NULL,
                            month_length = lubridate::dmonths(),
                            year_length = lubridate::dyears(),
                            ignore_date = TRUE, quiet = FALSE) {

    checkmate::assert_string(tz)

    class <- tolower(class)

    if (class == "character") {
        as.character(x)
    } else if (class == "numeric") {
        as.numeric(x)
    } else if (class == "difftime") {
        if (isFALSE(quiet)) {
            rlang::warn("There's no time to parse")
        }
        as.numeric(NA)
    } else if (class == "duration") {
        if (isFALSE(quiet)) {
            rlang::warn("There's no time to parse")
        }
        lubridate::as.duration(NA)
    } else if (class == "hms") {
        if (isFALSE(quiet)) {
            rlang::warn("There's no time to parse")
        }
        hms::as_hms(NA)
    } else if (class == "period") {
        if (isFALSE(quiet)) {
            rlang::warn("There's no time to parse")
        }
        lubridate::as.period(NA)
    } else if (class == "date") {
        lubridate::force_tz(x, tz)
    } else if (class == "posixct") {
        out <- as.POSIXct(lubridate::force_tz(x, tz))
        lubridate::force_tz(as.POSIXct(out), tz = tz)
    } else if (class == "posixlt") {
        out <- as.POSIXlt(lubridate::force_tz(x, tz))
        lubridate::force_tz(as.POSIXlt(out), tz = tz)
    } else {
        rlang::abort("Critical error")
    }

}

#' @rdname convert_to
#' @export
convert_to.POSIXt <- function(x, class, ..., tz = "UTC",
                              output_unit = NULL,
                              month_length = lubridate::dmonths(),
                              year_length = lubridate::dyears(),
                              ignore_date = TRUE, quiet = FALSE) {

    checkmate::assert_string(tz)

    class <- tolower(class)

    if (class == "character") {
        as.character(x)
    } else if (class == "numeric") {
        as.numeric(x)
    } else if (class == "difftime") {
        lubridate::as.difftime(lubridate::as.duration(hms::as_hms(x)))
    } else if (class == "duration") {
        lubridate::as.duration(hms::as_hms(x))
    } else if (class == "hms") {
        hms::as_hms(x)
    } else if (class == "period") {
        lubridate::as.period(hms::as_hms(x))
    } else if (class == "date") {
        lubridate::as_date(lubridate::force_tz(x, tz))
    } else if (class == "posixct") {
        lubridate::force_tz(as.POSIXct(x), tz = tz)
    } else if (class == "posixlt") {
        lubridate::force_tz(as.POSIXlt(x), tz = tz)
    } else {
        rlang::abort("Critical error")
    }

}

#' @rdname convert_to
#' @export
convert_to.Interval <- function(x, class, ..., tz = "UTC",
                                output_unit = NULL,
                                month_length = lubridate::dmonths(),
                                year_length = lubridate::dyears(),
                                quiet = FALSE) {

    checkmate::assert_string(tz)

    class <- tolower(class)

    if (class == "logical") {
        as.logical(NA)
    } else if (class == "character") {
        as.character(x)
    } else if (class == "numeric") {
        as.numeric(x)
    } else if (class == "difftime") {
        lubridate::as.difftime(x)
    } else if (class == "duration") {
        lubridate::as.duration(x)
    } else if (class == "hms") {
        hms::hms(x)
    } else if (class == "period") {
        lubridate::as.period(x)
    } else if (class == "date") {
        rlang::warn("There's no date to parse")
        lubridate::as_date(NA)
    } else if (class == "posixct") {
        out <- as.POSIXct(hms::hms(x))
        lubridate::year(out) <- 0
        lubridate::force_tz(as.POSIXct(out), tz = tz)
    } else if (class == "posixlt") {
        out <- as.POSIXlt(hms::hms(x))
        lubridate::year(out) <- 0
        lubridate::force_tz(as.POSIXlt(out), tz = tz)
    } else {
        rlang::abort("Critical error")
    }

}


# WRAPPERS =====

#' @rdname convert_to
#' @export
convert_to_tu <- function(x, output_unit, ...) {

    assert_time(x)

    convert_to(x, class = "numeric", output_unit = output_unit, ... = ...)

}

#' @rdname convert_to
#' @export
convert_to_uu <- function(x, input_unit, output_unit, ...) {

    checkmate::assert_numeric(x)

    convert_to(x, class = "numeric", input_unit = input_unit,
               output_unit = output_unit, ... = ...)

}


# HELPERS =====

#' @noRd
parse_to_date_time <- function(x, orders = c("HMS", "HM", "H"), tz = "UTC",
                               quiet = FALSE) {

    # Check arguments -----

    checkmate::assert_multi_class(x, c("character", "numeric"))
    checkmate::assert_character(orders, min.chars = 1, any.missing = FALSE,
                                all.missing = FALSE, min.len = 1, unique = TRUE)
    checkmate::assert_string(tz)
    checkmate::assert_flag(quiet)

    # Set values -----

    out <- x

    # Remove whitespaces and assign NA -----

    if (is.character(out)) {
        out <- stringr::str_squish(out)
        for (i in c("", "NA")) {
            out <- dplyr::na_if(out, i)
        }
    }

    # Parse to date/time -----

    if ((any(is.na(out)) || any(is.null(out))) && length(out) == 1) {
        as.POSIXct(NA)
    } else if (length(orders) == 1 &&
               stringr::str_detect(orders[1], "^(H)?(M)?(S)?$")) {
        pattern_1 <- "^([-+])?\\d+(.\\d+)?$"
        pattern_2 <- paste0("^([-+])?\\d{1,2}(:)?[0-5]\\d$", "|",
                            "^([-+])?\\d{3,}:[0-5]\\d$")
        pattern_3 <- paste0("^([-+])?\\d{1,2}(:)?[0-5]\\d(:)?[0-5]\\d$",
                            "|", "^([-+])?\\d{3,}:[0-5]\\d(:)?[0-5]\\d$")

        assign_signal <- function(out, x) {
            out <- dplyr::case_when(
                stringr::str_detect(x, "^-") ~ - out,
                TRUE ~ out
            )
        }

        check_nas <- function(out, x, quiet = FALSE) {

            if (isTRUE(quiet)) return()
            na_diff <- length(which(is.na(out))) - length(which(is.na(x)))

            if (all(is.na(out))) {
                rlang::warn("All formats failed to parse. No formats found.")
            } else if (na_diff > 0) {
                rlang::warn(glue::glue("{na_diff} failed to parse."))
            }
        }

        if (orders[1] %in% c("H", "M", "S")) {
            parse_1 <- function(out, x, orders){
                out <- paste(out, orders)
                out <- hms::as_hms(as.integer(lubridate::duration(out)))
                out <- assign_signal(out, x)
            }

            out <- dplyr::case_when(
                stringr::str_detect(out, pattern_1) | is.na(out) ~
                    parse_1(out, x, orders)
            )

            check_nas(out, x, quiet = quiet)
        } else if (orders[1] %in% c("HM")) {
            parse_2 <- function(out, x) {
                pattern_h <- "\\d+(?=(:)?[0-5]\\d)"
                hours <- as.integer(stringr::str_extract(out, pattern_h))
                pattern_m <- "[0-5]\\d$"
                minutes <- as.integer(stringr::str_extract(out, pattern_m))

                out <- lubridate::dhours(hours) + lubridate::dminutes(minutes)
                out <- hms::as_hms(as.integer(out))
                out <- assign_signal(out, x)
            }

            out <- dplyr::case_when(
                stringr::str_detect(out, pattern_2) | is.na(out) ~
                    parse_2(out, x)
            )

            check_nas(out, x, quiet = quiet)
        } else if (orders[1] %in% c("HMS")) {
            parse_3 <- function(out, x) {
                pattern_h <- paste0("\\d{3,}(?=:[0-5]\\d(:)?[0-5]\\d)", "|",
                                    "(?<=([-+])?)\\d{1,2}",
                                    "(?=(:)?[0-5]\\d(:)?[0-5]\\d)")
                hours <- as.integer(stringr::str_extract(out, pattern_h))
                pattern_m <- paste0("(?<=:)[0-5]\\d(?=(:)?[0-5]\\d)", "|",
                                    "(?<=([-+])?\\d{1,2})[0-5]\\d",
                                    "(?=(:)?[0-5]\\d)")
                minutes <- as.integer(stringr::str_extract(out, pattern_m))
                pattern_s <- "[0-5]\\d$"
                seconds <- as.integer(stringr::str_extract(out, pattern_s))

                out <- lubridate::dhours(hours) + lubridate::dminutes(minutes) +
                    lubridate::dseconds(seconds)
                out <- hms::as_hms(as.integer(out))
                out <- assign_signal(out, x)
            }

            out <- dplyr::case_when(
                stringr::str_detect(out, pattern_3) | is.na(out) ~
                    parse_3(out, x)
            )

            check_nas(out, x, quiet = quiet)
        } else {
            out <- lubridate::parse_date_time(out, orders, tz, quiet = quiet)
        }
    } else {
        out <- lubridate::parse_date_time(out, orders, tz, quiet = quiet)
    }

    out

}

#' @noRd
convert_to_seconds <- function(x, input_unit = NULL,
                               month_length = lubridate::dmonths(),
                               year_length = lubridate::dyears(),
                               ignore_date = TRUE,
                               quiet = FALSE) {

    # Check arguments -----

    classes <- c("integer", "double", "numeric", "Duration",  "Period",
                 "difftime", "hms", "Date", "POSIXct", "POSIXlt", "Interval",
                 "Circular")

    choices <- c("S", "M", "H", "d", "W", "m", "y", "date_decimal",
                 "rad", "deg")

    checkmate::assert_multi_class(x, classes, null.ok = FALSE)
    checkmate::assert_choice(input_unit, choices, null.ok = TRUE)
    checkmate::assert_numeric(month_length, lower = 0, len = 1)
    checkmate::assert_numeric(year_length, lower = 0, len = 1)
    checkmate::assert_flag(ignore_date)
    checkmate::assert_flag(quiet)

    if (!is_time(x) && is.null(input_unit)) {
        rlang::abort("When `x` is numeric, `input_unit` cannot be `NULL`.")
    }

    # Set values -----

    ## rad_second <- (2 * pi) / 24 / 60 / 60
    rad_second <- pi / 43200 # rad equivalent to 1 second
    ## deg_second <- 15 / (60 * 60)
    deg_second <- 15 / 3600 # degree equivalent to 1 second

    # Compute output -----

    if (any(class(x) %in% c("integer", "double", "numeric")) &&
        !circular::is.circular(x)) {
        if (input_unit == "S") {
            x
        } else if (input_unit == "M") {
            as.numeric(lubridate::dminutes(x))
        } else if (input_unit == "H") {
            as.numeric(lubridate::dhours(x))
        } else if (input_unit == "d") {
            as.numeric(lubridate::ddays(x))
        } else if (input_unit == "W") {
            as.numeric(lubridate::dweeks(x))
        } else if (input_unit == "m") {
            x * as.numeric(month_length)
        } else if (input_unit == "y") {
            x * as.numeric(year_length)
        } else if (input_unit == "date_decimal") {
            as.numeric(hms::as_hms(lubridate::date_decimal(x)))
        } else if (input_unit == "rad") {
            x / rad_second
        } else if (input_unit == "deg") {
            x / deg_second
        }
    } else if (is_time(x)) {
        if (lubridate::is.duration(x)) {
            as.numeric(x)
        } else if (lubridate::is.period(x)) {
            as.numeric(x)
        } else if (lubridate::is.difftime(x)) {
            as.numeric(x)
        } else if (hms::is_hms(x)) {
            as.numeric(x)
        } else if (lubridate::is.Date(x)) {
            if (isTRUE(ignore_date)) {
                as.numeric(0)
            } else {
                as.numeric(x) * as.numeric(lubridate::ddays())
            }
        } else if (lubridate::is.POSIXt(x)) {
            if (isTRUE(ignore_date)) {
                as.numeric(hms::as_hms(x))
            } else {
                as.numeric(x)
            }
        } else if (lubridate::is.interval(x)) {
            as.numeric(x)
        } else if (circular::is.circular(x)) {
            circular_units <- attr(x, "circularp")$units

            if (circular_units == "hours") {
                lubridate::dhours(as.numeric(x))
            } else if (circular_units == "radians") {
                as.numeric(x) / rad_second
            } else if (circular_units == "degrees") {
                as.numeric(x) / deg_second
            }
        }
    } else {
        if (isFALSE(quiet)) rlang::warn("NAs introduced by coercion")
        as.numeric(rep(NA, length(x)))
    }

}

#' @noRd
convert_to_unit <- function(x, input_unit = NULL, output_unit = "H",
                            month_length = lubridate::dmonths(),
                            year_length = lubridate:: dyears(),
                            ignore_date = TRUE,
                            quiet = FALSE) {

    # Check arguments -----

    choices <- c("S", "M", "H", "d", "W", "m", "y", "date_decimal",
                 "rad", "deg")

    checkmate::assert_choice(output_unit, choices, null.ok = TRUE)

    # Set values -----

    ## rad_second <- (2 * pi) / 24 / 60 / 60
    rad_second <- pi / 43200 # rad equivalent to 1 second
    ## deg_second <- 15 / (60 * 60)
    deg_second <- 15 / 3600 # degree equivalent to 1 second

    # Compute output -----

    if (output_unit == "date_decimal") {
        if (lubridate::is.Date(x) || lubridate::is.POSIXt(x)) {
            return(lubridate::decimal_date(x))
        } else {
            rlang::abort(
                glue::glue('When `output_unit` is equal to "date_decimal" ',
                           '`x` must be class `Date` or `POSIXt`.'))
        }
    }

    x <- convert_to_seconds(x, input_unit, month_length, year_length,
                            ignore_date, quiet)

    if (output_unit == "S") {
        x
    } else if (output_unit == "M") {
        x / as.numeric(lubridate::dminutes())
    } else if (output_unit == "H") {
        x / as.numeric(lubridate::dhours())
    } else if (output_unit == "d") {
        x / as.numeric(lubridate::ddays())
    } else if (output_unit == "W") {
        x / as.numeric(lubridate::dweeks())
    } else if (output_unit == "m") {
        x / as.numeric(month_length)
    } else if (output_unit == "y") {
        x / as.numeric(year_length)
    } else if (output_unit == "rad") {
        x * rad_second
    } else if (output_unit == "deg") {
        x * deg_second
    } else {
        rlang::abort("Critical error")
    }

}
