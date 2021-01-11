#' Converts a R object to another
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `convert_to` converts a R object to another object of a predefined class. Its
#' mission is to facilitate conversions between any kind of R object in a
#' simple and fast way.
#'
#' This function also supports date/time parsing and value transformations.
#' Check Details section to learn more.
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
#' `convert_to` have some wrappers functions for convenience.
#'
#' `convert_to_tu` help make conversions from date/time objects to units.
#'
#' `convert_to_ut` help make conversions from units to date/time objects.
#'
#' `convert_to_tt` help make conversions from date/time objects to other
#' date/time objects.
#'
#' `convert_to_uu` help make conversions from unit to other units.
#'
#' `convert_to_pt` help make conversions from `character` or `numeric` objects
#' to date/time objects.
#'
#' `convert_to_pu` help make conversions from `character` or `numeric` objects
#' to units.
#'
#' ## `class` argument
#'
#' The `class` argument indicates the desired class of the output object . It
#' accepts the following values: `"character"`, `"integer"`, `"double"`,
#' `"numeric"`, `"Duration"`, `"Period"`, `"difftime"`, `"hms"`, `"Date"`,
#' `"POSIXct"`, and `"POSIXlt"` (case insensitive).
#'
#' ## `orders` argument
#'
#' `orders` is an optional argument to indicate date/time formats for
#' parsing `character` or `numeric` objects. This parsing is mainly
#' based on [lubridate::parse_date_time()], please check it documentation
#' to set the right formats for your input.
#'
#' ## `input_unit` and `output_unit` arguments
#'
#' To be able to convert date/time objects and `numeric` objects to decimal
#' time, radians, or degrees, `convert_to` need to know the unit of `x` and the
#' output unit. Conversions of date/time objects only need the latter.
#'
#' Valid values for this two arguments are:
#'
#' * `"S"`: for decimal seconds;
#' * `"M"`: for decimal minutes;
#' * `"H"`: for decimal hours;
#' * `"d"`: for decimal days;
#' * `"W"`: for decimal weeks;
#' * `"m"`: for decimal months;
#' * `"y"`: for decimal years;
#' * `"date_decimal"`: for decimal dates;
#' * `"rad"`: for radians;
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
#' `convert_to` uses [lubridate::parse_date_time()] to parse `character` and
#' `numeric` objects to date/time. Since parse_date_time() outputs a `POSIXt`
#' object, `character` and `numeric` inputs cannot have time values equal or
#' greater than 24 hours.
#'
#' That limits the set of `convert_to` applications (_e.g_ when you want to
#' parse a `character` to a `duration` object of 35 minutes and 30 seconds). To
#' get around this, some exceptions were made to orders __equal__ to `"H"`,
#' `"M"`, `"S"`, `HM`, or `HMS`. For `HM` and `HMS` exceptions, minutes and
#' seconds are limited to `[0-59]`, and, when hours exceeds 2 digits, a `:` must
#' be allocated between hours and minutes.
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
#' You can also parse a `character` object and transform it direct to an unit.
#' See Examples section to know how.
#'
#' * When `class = "numeric"`
#'
#' `convert_to` will return a [base::as.numeric()] output if `class` is set to
#' `"numeric"`. For dates or date-time objects, the output will be the total of
#' seconds from the UNIX origin (`1970-01-01 00:00:00 UTC`) (See [Unix
#' time](https://en.wikipedia.org/wiki/Unix_time) to know more). For time
#' values, the output will be the total of seconds.
#'
#' The output `class = "numeric"` can also be different if `input_unit` and
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
#' values to `month_length` and `year_length`.
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
#' @param ... Additional arguments to be passed to or from methods.
#' @param orders (optional) A character vector of date/time formats to parse a
#'   `x` value of class `character` or `numeric`.
#' @param tz (optional) A string indicating the time zone with which to
#'   convert/parse `x` (defaul: `"UTC"`).
#' @param input_unit (optional) A string indicating the unit of `x`.
#' @param output_unit (optional) A string indicating the desire output unit.
#' @param month_length (optional) A `duration` object or a non negative numeric
#'   value with the number of seconds equivalent to the month length (default:
#'   `lubridate::dmonths()`, which is equivalent to 30.4375 days).
#' @param year_length (optional) A `duration` object or a non negative numeric
#'   value with the number of seconds equivalent to the year length (default:
#'   `lubridate::dyears()`, which is equivalent to 365.25 days).
#' @param ignore_date (optional) A logical value indicating if dates must be
#' ignored from `Date` or `POSIXt` objects when converting they to `numeric`.
#' @param close_round (optional) A logical value indicating if numbers with
#' decimals starting with five leading 0s or 9s must be rounded
#' (_e.g._ 1.99999) (default: `TRUE`).
#' @param quiet (optional) A logical value indicating if warnings or messages
#'   are allowed with the output (default: `FALSE`).
#'
#' @return A R object of the indicated class.
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
#' ## ** conversion from date/time objects to units **
#' convert_to(lubridate::dhours(), "numeric", output_unit = "M")
#' #> [1] 60 # Expected
#' convert_to(lubridate::days(), "numeric", output_unit = "rad")
#' #> [1] 6.283185 # Expected
#' x <- lubridate::as_datetime("1985-10-20 12:00:00")
#' convert_to(x, "numeric", output_unit = "d")
#' #> [1] 0.5 # Expected
#' x <- lubridate::as_datetime("1985-10-20 12:00:00")
#' convert_to(x, "numeric", output_unit = "d", ignore_date = FALSE)
#' #> [1] 5771.5 # Expected (days since UNIX origin)
#' convert_to(hms::parse_hm("15:45:00"), "numeric", output_unit = "H")
#' #> [1] 15.75 # Expected
#' convert_to_tu(hms::parse_hm("15:45:00"), "H") # Wrapper function
#' #> [1] 15.75 # Expected
#'
#' ## ** conversion from units to date/time objects **
#' convert_to(360, "period", input_unit = "deg")
#' #> [1] "1d 0H 0M 0S" # Expected
#' convert_to(6.5, "posixct", input_unit = "H")
#' #> [1] "0000-01-01 06:30:00 UTC" # Expected
#' convert_to(365.25, "hms", input_unit = "d")
#' #> 8766:00:00 # Expected
#' convert_to(1, "posixlt", input_unit = "W")
#' #> [1] "0000-01-08 UTC" # Expected
#' convert_to(1.308997, "duration", input_unit = "rad")
#' #> [1] "18000s (~5 hours)" # Expected
#' convert_to_ut(1.308997, "duration", "rad") # Wrapper function
#' #> [1] "18000s (~5 hours)" # Expected
#'
#' ## ** conversion between date/time objects **
#' convert_to(lubridate::duration(120), "hms")
#' #> 00:02:00 # Expected
#' convert_to(hms::as_hms("13:45:05"), "POSIXct")
#' #> [1] "0000-01-01 13:45:05 UTC" # Expected
#' convert_to(lubridate::period(60), "POSIXct")
#' #> [1] "0000-01-01 00:01:00 UTC" # Expected
#' convert_to(lubridate::as_date("1765-10-05"), "POSIXct")
#' #> [1] "1765-10-05 UTC" # Expected
#' x <- lubridate::ymd_hms("2020-01-01 12:31:05", tz = "EST")
#' convert_to(x, "POSIXct")
#' #> [1] "2020-01-01 12:31:05 UTC" # Expected
#' convert_to_tt(x, "POSIXct") # Wrapper function
#' #> [1] "2020-01-01 12:31:05 UTC" # Expected
#'
#' ## ** conversion between units **
#' convert_to(1.308997, "numeric", input_unit = "rad", output_unit = "H")
#' #> [1] 5 # Expected
#' convert_to(60, "numeric", input_unit = "deg", output_unit = "rad")
#' #> [1] 1.047198 # Expected
#' convert_to(1, "numeric", input_unit = "m", output_unit = "y")
#' #> [1] 0.08333333 # Expected
#' convert_to(0.2617994, "numeric", input_unit = "rad", output_unit = "H")
#' #> [1] 1 # Expected
#' convert_to(40, "numeric", input_unit = "d", output_unit = "deg")
#' #> [1] 14400 # Expected
#' convert_to_uu(40, "d", "deg") # Wrapper function
#' #> [1] 14400 # Expected
#'
#' ## ** conversion from character or numeric objects to date/time objects **
#' convert_to("19:55:17", "duration", orders = "HMS")
#' #> [1] "71717s (~19.92 hours)" # Expected
#' convert_to("21:00", "Period", orders = "HM")
#' #> [1] "21H 0M 0S" # Expected
#' convert_to(1, "difftime", orders = "H")
#' #> Time difference of 3600 secs # Expected
#' convert_to("10:00 PM", "hms", orders = "IMp")
#' #> 22:00:00 # Expected
#' convert_to("2020-01-01 10:00:00", "Date", orders = "ymd HMS")
#' #> [1] "2020-01-01" # Expected
#' convert_to(13, "POSIXct", orders = "H")
#' #> [1] "0000-01-01 13:00:00 UTC" # Expected
#' convert_to("2020-01-01 12:31:05", "POSIXct", orders = "ymd HMS", tz = "EST")
#' #> [1] "2020-01-01 12:31:05 EST" # Expected
#' convert_to("03/07/1982 13:00", "POSIXlt", orders = "dmy HM")
#' #> [1] "1982-07-03 13:00:00 UTC" # Expected
#' convert_to_pt("03/07/1982 13:00", "POSIXlt", "dmy HM") # Wrapper function
#' #> [1] "1982-07-03 13:00:00 UTC" # Expected
#'
#' ## ** conversion from character or numeric objects to units **
#' convert_to("0145", "numeric", orders = "HM", output_unit = "M")
#' #> [1] 105 # Expected
#' convert_to(45, "numeric", orders = "M", output_unit = "H")
#' #> [1] 0.75 # Expected
#' convert_to(4500, "numeric", orders = "HM", output_unit = "d")
#' #> [1] 1.875 # Expected
#' convert_to("2020-03-15 02", "numeric", orders = "ymd H", output_unit = "H")
#' #> [1] 2 # Expected
#' convert_to("01:00", "numeric", orders = "HM", output_unit = "rad")
#' #> [1] 0.261799 # Expected
#' convert_to_pu("01:00", "HM", "rad") # Wrapper function
#' #> [1] 0.2617994 # Expected
convert_to <- function(x, class, ..., quiet = FALSE) {

    # Check arguments -----

    choices <- stringr::str_to_lower(
        c("character", "integer", "double", "numeric", "Duration",
          "Period", "difftime", "hms", "Date", "POSIXct", "POSIXlt"))

    checkmate::assert_choice(stringr::str_to_lower(class), choices)
    checkmate::assert_flag(quiet)

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
convert_to.character <- function(x, class, ..., orders = NULL, tz = "UTC",
                                 input_unit = NULL, output_unit = NULL,
                                 month_length = lubridate::dmonths(),
                                 year_length = lubridate::dyears(),
                                 ignore_date = TRUE, close_round = TRUE,
                                 quiet = FALSE) {

    # Check arguments -----

    checkmate::assert_character(orders, min.chars = 1, any.missing = FALSE,
                                all.missing = FALSE, min.len = 1, unique = TRUE,
                                null.ok = TRUE)
    checkmate::assert_string(tz)

    # Set values -----

    class <- tolower(class)

    # Remove extra whitespaces and assign NA -----

    if (is.character(x)) {
        x <- stringr::str_squish(x)
        for (i in c("", "NA")) {
            x <- dplyr::na_if(x, i)
        }
    }

    # Parse and/or transform values -----

    check <- identical(count_na(x), count_na(shush(as.numeric(x))))

    if (is.null(orders)) {
        if (!is.null(input_unit) && !is.null(output_unit) && check &&
            class %in% c("integer", "double", "numeric")) {
            x <- convert_to_unit(shush(as.numeric(x)),
                                 input_unit = input_unit,
                                 output_unit = output_unit,
                                 month_length = month_length,
                                 year_length = year_length,
                                 ignore_date = ignore_date,
                                 close_round = close_round,
                                 quiet = quiet)

            if (class %in% "integer") return(as.integer(x))
            if (class %in% c("double", "numeric")) return(x)
        } else if (!is.null(input_unit) && check &&
                   !(class %in% c("integer", "double", "numeric"))) {
            return(convert_to_date_time(shush(as.numeric(x)),
                                        class = class,
                                        input_unit = input_unit,
                                        month_length = month_length,
                                        year_length = year_length,
                                        close_round = close_round,
                                        tz = tz, quiet = quiet))
        }
    } else {
        x <- parse_to_date_time(x, orders, tz, quiet)

        if (class %in% c("integer", "double", "numeric") && is_time(x) &&
            !is.null(output_unit) && !all(is.na(x))) {
            x <- convert_to_unit(x, input_unit = NULL,
                                 output_unit = output_unit,
                                 month_length = month_length,
                                 year_length = year_length,
                                 ignore_date = ignore_date,
                                 close_round = close_round,
                                 quiet = quiet)

            if (class %in% "integer") return(as.integer(x))
            if (class %in% c("double", "numeric")) return(x)
        }
    }

    # Fix values -----

    if (class %in% c("character", "integer", "double", "numeric") &&
        !is.null(orders) && all(lubridate::year(x) %in% c(0))) {
        x <- hms::as_hms(x)
    }

    if (class %in% c("posixct", "posixct") &&
        hms::is_hms(x)) {
        x <- flat_posixt(as.POSIXct(x))
    }

    # Check inconsistencies -----

    if (is.null(orders) && !all(is.na(x)) &&
        !(class %in% c("character", "integer", "double", "numeric"))) {
        shush(rlang::abort(glue::glue(
            "Non-parsed character or numeric vectors cannot be converted to ",
            "{class}. Did you forget to assign values to `orders`?")),
            quiet)
    }

    # Convert to class and return output -----

    if (class == "character") {
        as.character(x)
    } else if (class == "integer") {
        shush(as.integer(x), quiet)
    } else if (class %in% c("double", "numeric")) {
        shush(as.numeric(x), quiet)
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
            lubridate::as_date(rep(NA, length(x)))
        } else if (!lubridate::is.POSIXt(x) ||
                   (lubridate::is.POSIXt(x) && all(lubridate::year(x) == 0))) {
            shush(rlang::warn("There is no date to parse."), quiet)
            lubridate::as_date(rep(NA, length(x)))
        } else {
            lubridate::as_date(lubridate::force_tz(x, tz))
        }
    } else if (class == "posixct") {
        lubridate::force_tz(as.POSIXct(x), tz = tz)
    } else if (class == "posixlt") {
        if (lubridate::is.POSIXct(x)) {
            lubridate::force_tz(as.POSIXlt(x), tz = tz)
        } else {
            x <- as.POSIXlt(x)
            lubridate::year(x) <- 0
            lubridate::force_tz(x, tz = tz)
        }
    } else {
        rlang::abort("Critical error.")
    }

}

#' @rdname convert_to
#' @export
convert_to.numeric <- function(x, class, ..., orders = NULL, tz = "UTC",
                               input_unit = NULL, output_unit = NULL,
                               month_length = lubridate::dmonths(),
                               year_length = lubridate::dyears(),
                               ignore_date = TRUE, close_round = TRUE,
                               quiet = FALSE) {

   convert_to.character(x, class, orders = orders, tz = tz,
                        input_unit = input_unit, output_unit = output_unit,
                        month_length = month_length, year_length = year_length,
                        ignore_date = ignore_date,
                        close_round = close_round, quiet = quiet)

}

#' @rdname convert_to
#' @export
convert_to.Duration <- function(x, class, ..., tz = "UTC", output_unit = NULL,
                                month_length = lubridate::dmonths(),
                                year_length = lubridate::dyears(),
                                close_round = TRUE, quiet = FALSE) {

    # Check arguments -----

    checkmate::assert_string(tz)

    # Set values -----

    class <- tolower(class)

    # Transform values -----

    if (class %in% c("integer", "double", "numeric") &&
        !is.null(output_unit) && !all(is.na(x))) {
        x <- convert_to_unit(x, input_unit = NULL, output_unit = output_unit,
                             month_length = month_length,
                             year_length = year_length, ignore_date = TRUE,
                             close_round = close_round, quiet = quiet)

        if (class %in% "integer") return(as.integer(x))
        if (class %in% c("double", "numeric")) return(x)
    }

    # Convert to class and return output -----

    if (class == "character") {
        as.character(hms::as_hms(as.numeric(x)))
    } else if (class == "integer") {
        shush(as.integer(x), quiet)
    } else if (class %in% c("double", "numeric")) {
        shush(as.numeric(x), quiet)
    } else if (class == "duration") {
        lubridate::as.duration(x)
    } else if (class == "period") {
        lubridate::as.period(x)
    } else if (class == "difftime") {
        lubridate::as.difftime(x)
    } else if (class == "hms") {
        hms::as_hms(as.numeric(x))
    } else if (class == "date") {
        shush(rlang::warn("There is no date to parse."), quiet)
        lubridate::as_date(NA)
    } else if (class == "posixct") {
        x <- as.POSIXct(hms::as_hms(as.numeric(x)))
        lubridate::year(x) <- 0
        lubridate::force_tz(x, tz = tz)
    } else if (class == "posixlt") {
        x <- as.POSIXlt(hms::as_hms(as.numeric(x)))
        lubridate::year(x) <- 0
        lubridate::force_tz(x, tz = tz)
    } else {
        rlang::abort("Critical error.")
    }

}

#' @rdname convert_to
#' @export
convert_to.Period <- function(x, class, ..., tz = "UTC", output_unit = NULL,
                              month_length = lubridate::dmonths(),
                              year_length = lubridate::dyears(),
                              close_round = TRUE, quiet = FALSE) {

    convert_to.Duration(x, class, tz = tz, output_unit = output_unit,
                        month_length = month_length, year_length = year_length,
                        close_round = close_round, quiet = quiet)

}

#' @rdname convert_to
#' @export
convert_to.difftime <- function(x, class, ..., tz = "UTC", output_unit = NULL,
                                month_length = lubridate::dmonths(),
                                year_length = lubridate::dyears(),
                                close_round = TRUE, quiet = FALSE) {

    convert_to.hms(x, class, tz = tz, output_unit = output_unit,
                   month_length = month_length, year_length = year_length,
                   close_round = close_round, quiet = quiet)

}

#' @rdname convert_to
#' @export
convert_to.hms <- function(x, class, ..., tz = "UTC", output_unit = NULL,
                           month_length = lubridate::dmonths(),
                           year_length = lubridate::dyears(),
                           close_round = TRUE, quiet = FALSE) {

    # Check arguments -----

    checkmate::assert_string(tz)

    # Set values -----

    class <- tolower(class)

    # Transform values -----

    if (class %in% c("integer", "double", "numeric") &&
        !is.null(output_unit) && !all(is.na(x))) {
        x <- convert_to_unit(x, input_unit = NULL, output_unit = output_unit,
                             month_length = month_length,
                             year_length = year_length, ignore_date = TRUE,
                             close_round = close_round, quiet = quiet)

        if (class %in% "integer") return(as.integer(x))
        if (class %in% c("double", "numeric")) return(x)
    }

    # Convert to class and return output -----

    if (class == "character") {
        as.character(x)
    } else if (class == "integer") {
        shush(as.integer(x), quiet)
    } else if (class %in% c("double", "numeric")) {
        shush(as.numeric(x), quiet)
    } else if (class == "duration") {
        lubridate::as.duration(x)
    } else if (class == "period") {
        lubridate::as.period(x)
    } else if (class == "difftime") {
        lubridate::as.difftime(lubridate::as.duration(x))
    } else if (class == "hms") {
        hms::as_hms(x)
    } else if (class == "date") {
        shush(rlang::warn("There is no date to parse"), quiet)
        lubridate::as_date(NA)
    } else if (class == "posixct") {
        x <- as.POSIXct(hms::as_hms(x))
        lubridate::year(x) <- 0
        lubridate::force_tz(x, tz = tz)
    } else if (class == "posixlt") {
        x <- as.POSIXlt(hms::as_hms(x))
        lubridate::year(x) <- 0
        lubridate::force_tz(x, tz = tz)
    } else {
        rlang::abort("Critical error.")
    }

}

#' @rdname convert_to
#' @export
convert_to.Date <- function(x, class, ..., tz = "UTC", output_unit = NULL,
                            month_length = lubridate::dmonths(),
                            year_length = lubridate::dyears(),
                            close_round = TRUE, quiet = FALSE) {

    # Check arguments -----

    checkmate::assert_string(tz)

    # Set values -----

    class <- tolower(class)

    # Transform values -----

    if (class %in% c("integer", "double", "numeric") &&
        !is.null(output_unit) && !all(is.na(x))) {
        x <- convert_to_unit(x, input_unit = NULL, output_unit = output_unit,
                             month_length = month_length,
                             year_length = year_length, ignore_date = FALSE,
                             close_round = close_round, quiet = quiet)

        if (class %in% "integer") return(as.integer(x))
        if (class %in% c("double", "numeric")) return(x)
    }

    # Convert to class and return output -----

    if (class == "character") {
        as.character(x)
    } else if (class == "integer") {
        shush(as.integer(x), quiet)
    } else if (class %in% c("double", "numeric")) {
        shush(as.numeric(x), quiet)
    } else if (class == "duration") {
        shush(rlang::warn("There is no time to parse."), quiet)
        lubridate::as.duration(NA)
    } else if (class == "period") {
        shush(rlang::warn("There is no time to parse."), quiet)
        lubridate::as.period(NA)
    } else if (class == "difftime") {
        shush(rlang::warn("There is no time to parse."), quiet)
        as.numeric(NA)
    } else if (class == "hms") {
        shush(rlang::warn("There is no time to parse."), quiet)
        hms::as_hms(NA)
    } else if (class == "date") {
        lubridate::force_tz(x, tz)
    } else if (class == "posixct") {
        lubridate::force_tz(lubridate::as_datetime(x), tz = tz)
    } else if (class == "posixlt") {
        as.POSIXlt(lubridate::force_tz(lubridate::as_datetime(x), tz = tz))
    } else {
        rlang::abort("Critical error.")
    }

}

#' @rdname convert_to
#' @export
convert_to.POSIXt <- function(x, class, ..., tz = "UTC", output_unit = NULL,
                              month_length = lubridate::dmonths(),
                              year_length = lubridate::dyears(),
                              ignore_date = TRUE, close_round = TRUE,
                              quiet = FALSE) {

    # Check arguments -----

    checkmate::assert_string(tz)

    # Set values -----

    class <- tolower(class)

    # Transform values -----

    if (class %in% c("integer", "double", "numeric") &&
        !is.null(output_unit) && !all(is.na(x))) {
        x <- convert_to_unit(x, input_unit = NULL, output_unit = output_unit,
                             month_length = month_length,
                             year_length = year_length,
                             ignore_date = ignore_date,
                             close_round = close_round, quiet = quiet)

        if (class %in% "integer") return(as.integer(x))
        if (class %in% c("double", "numeric")) return(x)
    }

    # Convert to class and return output -----

    if (class == "character") {
        as.character(x)
    } else if (class == "integer") {
        shush(as.integer(x), quiet)
    } else if (class %in% c("double", "numeric")) {
        shush(as.numeric(x), quiet)
    } else if (class == "duration") {
        lubridate::as.duration(hms::as_hms(x))
    } else if (class == "period") {
        lubridate::as.period(hms::as_hms(x))
    } else if (class == "difftime") {
        lubridate::as.difftime(lubridate::as.duration(hms::as_hms(x)))
    } else if (class == "hms") {
        hms::as_hms(x)
    } else if (class == "date") {
        lubridate::as_date(lubridate::force_tz(x, tz))
    } else if (class == "posixct") {
        lubridate::force_tz(as.POSIXct(x), tz = tz)
    } else if (class == "posixlt") {
        lubridate::force_tz(as.POSIXlt(x), tz = tz)
    } else {
        rlang::abort("Critical error.")
    }

}

#' @rdname convert_to
#' @export
convert_to.Interval <- function(x, class, ..., tz = "UTC", output_unit = NULL,
                                month_length = lubridate::dmonths(),
                                year_length = lubridate::dyears(),
                                close_round = TRUE, quiet = FALSE) {

    # Check arguments -----

    checkmate::assert_string(tz)

    # Set values -----

    class <- tolower(class)

    # Transform values -----

    if (class %in% c("integer", "double", "numeric") &&
        !is.null(output_unit) && !all(is.na(x))) {
        x <- convert_to_unit(x, input_unit = NULL, output_unit = output_unit,
                             month_length = month_length,
                             year_length = year_length, ignore_date = NULL,
                             close_round = close_round, quiet = quiet)

        if (class %in% "integer") return(as.integer(x))
        if (class %in% c("double", "numeric")) return(x)
    }

    # Convert to class and return output -----

    if (class == "character") {
        as.character(x)
    } else if (class == "integer") {
        shush(as.integer(x), quiet)
    } else if (class %in% c("double", "numeric")) {
        shush(as.numeric(x), quiet)
    } else if (class == "duration") {
        lubridate::as.duration(x)
    } else if (class == "period") {
        lubridate::as.period(x)
    } else if (class == "difftime") {
        lubridate::as.difftime(x)
    } else if (class == "hms") {
        hms::hms(x)
    } else if (class == "date") {
        shush(rlang::warn("There is no date to parse."), quiet)
        lubridate::as_date(NA)
    } else if (class == "posixct") {
        x <- as.POSIXct(hms::as_hms(as.numeric(x)))
        lubridate::year(x) <- 0
        lubridate::force_tz(x, tz = tz)
    } else if (class == "posixlt") {
        x <- as.POSIXlt(hms::as_hms(as.numeric(x)))
        lubridate::year(x) <- 0
        lubridate::force_tz(x, tz = tz)
    } else {
        rlang::abort("Critical error.")
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
convert_to_ut <- function(x, class, input_unit, ...) {

    checkmate::assert_numeric(x)

    convert_to(x, class, input_unit = input_unit, ... = ...)

}

#' @rdname convert_to
#' @export
convert_to_tt <- function(x, class, ...) {

    assert_time(x)

    convert_to(x, class, ... = ...)

}

#' @rdname convert_to
#' @export
convert_to_uu <- function(x, input_unit, output_unit, ...) {

    checkmate::assert_numeric(x)

    convert_to(x, class = "numeric", input_unit = input_unit,
               output_unit = output_unit, ... = ...)

}

#' @rdname convert_to
#' @export
convert_to_pt <- function(x, class, orders, ...) {

    checkmate::assert_multi_class(x, c("character", "numeric"))

    convert_to(x, class, orders = orders, ... = ...)

}

#' @rdname convert_to
#' @export
convert_to_pu <- function(x, orders, output_unit, ...) {

    checkmate::assert_multi_class(x, c("character", "numeric"))

    convert_to(x, class = "numeric", orders = orders,
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

    # Remove extra whitespaces and assign NA -----

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
                               ignore_date = TRUE, quiet = FALSE) {

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
        shush(rlang::warn("NAs introduced by coercion."), quiet)
        as.numeric(rep(NA, length(x)))
    }

}

#' @noRd
convert_to_unit <- function(x, input_unit = NULL, output_unit = "H",
                            month_length = lubridate::dmonths(),
                            year_length = lubridate:: dyears(),
                            ignore_date = TRUE, close_round = TRUE,
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
        x <- x
    } else if (output_unit == "M") {
        x <- x / as.numeric(lubridate::dminutes())
    } else if (output_unit == "H") {
        x <- x / as.numeric(lubridate::dhours())
    } else if (output_unit == "d") {
        x <- x / as.numeric(lubridate::ddays())
    } else if (output_unit == "W") {
        x <- x / as.numeric(lubridate::dweeks())
    } else if (output_unit == "m") {
        x <- x / as.numeric(month_length)
    } else if (output_unit == "y") {
        x <- x / as.numeric(year_length)
    } else if (output_unit == "rad") {
        x <- x * rad_second
    } else if (output_unit == "deg") {
        x <- x * deg_second
    } else {
        rlang::abort("Critical error.")
    }

    if (isTRUE(close_round)) {
        close_round(x, 5)
    } else {
        x
    }

}

#' @noRd
convert_to_date_time <- function(x, class, input_unit = NULL,
                                 month_length = lubridate::dmonths(),
                                 year_length = lubridate:: dyears(),
                                 close_round = TRUE, tz = "UTC",
                                 quiet = FALSE) {

    checkmate::assert_numeric(x)

    x <- convert_to_unit(x, input_unit = input_unit, output_unit = "H",
                         month_length = month_length,
                         year_length = year_length, quiet = quiet,
                         close_round = TRUE)

    convert_to(lubridate::dhours(x), class, tz = tz)

}
