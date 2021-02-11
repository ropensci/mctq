#' Convert a R object to another
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `convert()` converts a R object to another object of a predefined class. Its
#' mission is to facilitate conversions between any kind of R object in a
#' simple and fast way.
#'
#' This function also supports date/time parsing and value transformations.
#' Check Details section to learn more.
#'
#' For a complete picture about how to convert your MCTQ data, see
#' `vignette("converting-data", package = "mctq")`.
#'
#' @details
#'
#' `convert()` was designed to be a simple to use function with powerful
#' applications. However, please note that it may not work for all cases.
#' Test it first before applying it to large datasets.
#'
#' To learn more about how to handle date and time objects, see chapter "Dates
#' and Times" from Wickham and Grolemund (n.d.) and chapter "Technical
#' Representation of Data" from Loo and Jonge (2018).
#'
#' ## Wrappers
#'
#' `convert()` have some wrappers functions for convenience.
#'
#' * `convert_tu()` help make conversions from date/time objects to units.
#'
#' * `convert_ut()` help make conversions from units to date/time objects.
#'
#' * `convert_tt()` help make conversions from date/time objects to other
#' date/time objects.
#'
#' * `convert_uu()` help make conversions from unit to other units.
#'
#' * `convert_pt()` help make conversions from `character` or `numeric` objects
#' to date/time objects.
#'
#' * `convert_pu()` help make conversions from `character` or `numeric` objects
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
#' time, radians, or degrees, `convert()` need to know the unit of `x` and the
#' output unit. Conversions of date/time objects only need the latter.
#'
#' Valid values for this two arguments are:
#'
#' * `"S"`: for decimal seconds.
#' * `"M"`: for decimal minutes.
#' * `"H"`: for decimal hours.
#' * `"d"`: for decimal days.
#' * `"W"`: for decimal weeks.
#' * `"m"`: for decimal months.
#' * `"y"`: for decimal years.
#' * `"date_decimal"`: for decimal dates.
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
#' `convert()` uses [lubridate::parse_date_time()] to parse `character` and
#' `numeric` objects to date/time. Since `parse_date_time()` outputs a `POSIXt`
#' object, `character` and `numeric` inputs cannot have time values equal or
#' greater than 24 hours.
#'
#' That limits the set of `convert()` applications (_e.g_ when you want to
#' parse a `character` to a `Duration` object of 35 minutes and 30 seconds). To
#' get around this, some exceptions were made to orders __equal__ to `"H"`,
#' `"M"`, `"S"`, `HM`, or `HMS`. For `HM` and `HMS` exceptions, minutes and
#' seconds are limited to `[0-59]`, and, when hours exceeds 2 digits, a `:` must
#' be allocated between hours and minutes.
#'
#' ## Converting columns of a data frame
#'
#' `convert()` also allow conversions of data frame variables. This is made
#' with the help of [dplyr::mutate()].
#'
#' Operations with data frames are only column-wise and can be made by
#' selecting individual columns (using the `col` argument) or group of columns
#' (by applying a flag function (_e.g_ `is.numeric`) in the `where` argument).
#'
#' ## Different outputs
#'
#' Your output can change according to your settings. Here are some examples.
#'
#' * When `class = "character"`
#'
#' `convert()` will return a [base::as.character()] output if `class` is set to
#' `"character"`. When `x` is parsed for date/time, and there's no
#' indication of year, `convert()` will return a `character` vector with a
#' `hms` time.
#'
#' You can also parse a `character` object and transform it direct to an unit.
#' See Examples section to know how.
#'
#' * When `class = "numeric"`
#'
#' `convert()` will return a [base::as.numeric()] output if `class` is set to
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
#' ## Year and month lengths
#'
#' The length of months and years can vary. For example, March have 31 days,
#' while April have have 30. Due to leap years, the same can be said to year
#' lengths.
#'
#' To address this problem, `convert()` use as default the mean of
#' possible values for months and years, used to calculate month and year
#' durations in the lubridate package (see: [lubridate::dmonths()] and
#' [lubridate::dyears()]). If you like, you can reset this by assigning other
#' values to `month_length` and `year_length` arguments.
#'
#' `month_length` and `year_length` values must be assigned with the number of
#' seconds equivalent to the unit duration. You can also assign a lubridate
#' duration object if you like (see [lubridate::duration()]).
#'
#' ## Decimal dates
#'
#' `convert()` can be used as a wrapper to [lubridate::decimal_date()]
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
#' @param orders (optional) a `character` object of date/time formats to parse a
#'   `x` value of class `character` or `numeric`.
#' @param tz (optional) a string indicating the time zone with which to
#'   convert/parse `x` (default: `"UTC"`).
#' @param input_unit (optional) a string indicating the unit of `x`.
#' @param output_unit (optional) a string indicating the desire output unit.
#' @param month_length (optional) a `Duration` value __or__ a non negative
#'   `numeric` value corresponding to the number of seconds equivalent to the
#'   month length (default: `lubridate::dmonths()`, which is equivalent to
#'   30.4375 days).
#' @param year_length (optional) a `Duration` value __or__ a non negative
#'   `numeric` value corresponding to the number of seconds equivalent to the
#'   year length (default: `lubridate::dyears()`, which is equivalent to 365.25
#'   days).
#' @param ignore_date (optional) a `logical` value indicating if dates must be
#' ignored from `Date` or `POSIXt` objects when converting they to `numeric`
#' (default: `TRUE`).
#' @param close_round (optional) a `logical` value indicating if numbers with
#' decimals starting with five leading 0s or 9s must be rounded
#' (_e.g._ 1.99999) (default: `TRUE`).
#' @param cols (optional) a `character` object indicating the column names in
#'   `x` to transform (default: `NULL`).
#' @param where (optional) a function to apply in a [tidyselect::where()] call
#'   (default: `NULL`).
#' @param quiet (optional) a `logical` value indicating if warnings or messages
#'   must be suppressed (default: `FALSE`).
#'
#' @return A R object of the indicated class.
#'
#' @family utility functions
#' @export
#'
#' @references
#'
#' Van der Loo, M., & De Jonge, E. (2018).
#' _Statistical data cleaning with applications in R_. Hooboken, NJ: John
#' Wiley & Sons. \doi{10.1002/9781118897126}.
#'
#' Wickham, H, & Grolemund. (n.d.). _R for data science_. Sebastopol, CA:
#' O'Reilly Media. Retrieved from <https://r4ds.had.co.nz>.
#'
#' @examples
#' ## __ Converting from date/time objects to units __
#' convert(lubridate::dhours(), "numeric", output_unit = "M")
#' #> [1] 60 # Expected
#' convert(lubridate::days(), "numeric", output_unit = "rad")
#' #> [1] 6.283185 # Expected
#' x <- lubridate::as_datetime("1985-10-20 12:00:00")
#' convert(x, "numeric", output_unit = "d")
#' #> [1] 0.5 # Expected
#' x <- lubridate::as_datetime("1985-10-20 12:00:00")
#' convert(x, "numeric", output_unit = "d", ignore_date = FALSE)
#' #> [1] 5771.5 # Expected (days since UNIX origin)
#' convert(hms::parse_hm("15:45:00"), "numeric", output_unit = "H")
#' #> [1] 15.75 # Expected
#' convert_tu(hms::parse_hm("15:45:00"), "H") # Wrapper function
#' #> [1] 15.75 # Expected
#'
#' ## __ Converting from units to date/time objects __
#' convert(360, "Period", input_unit = "deg")
#' #> [1] "1d 0H 0M 0S" # Expected
#' convert(6.5, "Posixct", input_unit = "H")
#' #> [1] "1970-01-01 06:30:00 UTC" # Expected
#' convert(365.25, "hms", input_unit = "d")
#' #> 8766:00:00 # Expected
#' convert(1, "Posixlt", input_unit = "W")
#' #> [1] "1970-01-08 UTC" # Expected
#' convert(1.308997, "Duration", input_unit = "rad")
#' #> [1] "18000s (~5 hours)" # Expected
#' convert_ut(1.308997, "Duration", "rad") # Wrapper function
#' #> [1] "18000s (~5 hours)" # Expected
#'
#' ## __ Converting between date/time objects __
#' convert(lubridate::dseconds(120), "hms")
#' #> 00:02:00 # Expected
#' convert(hms::as_hms("13:45:05"), "POSIXct")
#' #> [1] "1970-01-01 13:45:05 UTC" # Expected
#' convert(lubridate::seconds(60), "POSIXct")
#' #> [1] "1970-01-01 00:01:00 UTC" # Expected
#' convert(lubridate::as_date("1765-10-05"), "POSIXct")
#' #> [1] "1765-10-05 UTC" # Expected
#' x <- lubridate::ymd_hms("2020-01-01 12:31:05", tz = "EST")
#' convert(x, "POSIXct", tz = "UTC")
#' #> [1] "2020-01-01 12:31:05 UTC" # Expected
#' convert_tt(x, "Duration") # Wrapper function
#' #> [1] "45065s (~12.52 hours)" # Expected
#'
#' ## __ Converting between units __
#' convert(1.308997, "numeric", input_unit = "rad", output_unit = "H")
#' #> [1] 5 # Expected
#' convert(60, "numeric", input_unit = "deg", output_unit = "rad")
#' #> [1] 1.047198 # Expected
#' convert(1, "numeric", input_unit = "m", output_unit = "y")
#' #> [1] 0.08333333 # Expected
#' convert(0.2617994, "numeric", input_unit = "rad", output_unit = "H")
#' #> [1] 1 # Expected
#' convert(40, "numeric", input_unit = "d", output_unit = "deg")
#' #> [1] 14400 # Expected
#' convert_uu(40, "d", "deg") # Wrapper function
#' #> [1] 14400 # Expected
#'
#' ## __ Converting from character or numeric objects to date/time objects __
#' convert("19:55:17", "Duration", orders = "HMS")
#' #> [1] "71717s (~19.92 hours)" # Expected
#' convert("21:00", "Period", orders = "HM")
#' #> [1] "21H 0M 0S" # Expected
#' convert(1, "difftime", orders = "H")
#' #> Time difference of 3600 secs # Expected
#' convert("10:00 PM", "hms", orders = "IMp")
#' #> 22:00:00 # Expected
#' convert("2020-01-01 10:00:00", "Date", orders = "ymd HMS")
#' #> [1] "2020-01-01" # Expected
#' convert(13, "POSIXct", orders = "H")
#' #> [1] "1970-01-01 13:00:00 UTC" # Expected
#' convert("2020-01-01 12:31:05", "POSIXct", orders = "ymd HMS", tz = "EST")
#' #> [1] "2020-01-01 12:31:05 EST" # Expected
#' convert("03/07/1982 13:00", "POSIXlt", orders = "dmy HM")
#' #> [1] "1982-07-03 13:00:00 UTC" # Expected
#' convert_pt("03/07/1982 13:00", "POSIXlt", "dmy HM") # Wrapper function
#' #> [1] "1982-07-03 13:00:00 UTC" # Expected
#'
#' ## __ Converting from character or numeric objects to units __
#' convert("0145", "numeric", orders = "HM", output_unit = "M")
#' #> [1] 105 # Expected
#' convert(45, "numeric", orders = "M", output_unit = "H")
#' #> [1] 0.75 # Expected
#' convert(4500, "numeric", orders = "HM", output_unit = "d")
#' #> [1] 1.875 # Expected
#' convert("2020-03-15 02", "numeric", orders = "ymd H", output_unit = "H")
#' #> [1] 2 # Expected
#' convert("01:00", "numeric", orders = "HM", output_unit = "rad")
#' #> [1] 0.261799 # Expected
#' convert_pu("01:00", "HM", "rad") # Wrapper function
#' #> [1] 0.2617994 # Expected
#'
#' ## __ Converting columns of a data frame __
#' \dontrun{
#' out <- convert(datasets::mtcars, "posixct", cols = c("cyl", "carb"),
#'                   orders = "H")
#' head(out)
#'
#' out <- convert(datasets::iris, "duration", where = is.numeric,
#'                   input_unit = "H")
#' head(out)
#' }
convert <- function(x, class, ..., quiet = FALSE) {

    choices <- tolower(
        c("character", "integer", "double", "numeric", "Duration",
          "Period", "difftime", "hms", "Date", "POSIXct", "POSIXlt"))

    checkmate::assert_choice(tolower(class), choices)
    checkmate::assert_flag(quiet)

    if (any(is.na(x)) && length(x) == 1) {
        x <- as.character(NA)
        return(convert.character(x, class, ... = ..., quiet = quiet))
    } else {
        UseMethod("convert")
    }

}

#' @rdname convert
#' @export
convert.character <- function(x, class, ..., orders = NULL, tz = "UTC",
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
    if (is.character(x)) fix_character(x)

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
        (is.numeric(x) || is.character(x))  &&
        !(class %in% c("character", "integer", "double", "numeric",
                       "duration", "period", "difftime", "hms"))) {
        shush(stop(
            "Non-parsed ", class(x)[1], " vectors cannot be converted to ",
            class, ". Did you forget to assign values to `orders`?",
            call. = FALSE), quiet)
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
            shush(warning("There's no date to parse.", call. = FALSE), quiet)
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
            lubridate::force_tz(x, tz = tz)
        }
    }

}

#' @rdname convert
#' @export
convert.numeric <- function(x, class, ..., orders = NULL, tz = "UTC",
                            input_unit = NULL, output_unit = NULL,
                            month_length = lubridate::dmonths(),
                            year_length = lubridate::dyears(),
                            ignore_date = TRUE, close_round = TRUE,
                            quiet = FALSE) {

    convert.character(x, class, orders = orders, tz = tz,
                      input_unit = input_unit, output_unit = output_unit,
                      month_length = month_length, year_length = year_length,
                      ignore_date = ignore_date,
                      close_round = close_round, quiet = quiet)

}

#' @rdname convert
#' @export
convert.Duration <- function(x, class, ..., tz = "UTC", output_unit = NULL,
                             month_length = lubridate::dmonths(),
                             year_length = lubridate::dyears(),
                             close_round = TRUE, quiet = FALSE) {

    checkmate::assert_string(tz)

    class <- tolower(class)

    if (class %in% c("integer", "double", "numeric") &&
        !is.null(output_unit) && !all(is.na(x))) {
        x <- convert_to_unit(x, input_unit = NULL, output_unit = output_unit,
                             month_length = month_length,
                             year_length = year_length, ignore_date = TRUE,
                             close_round = close_round, quiet = quiet)

        if (class %in% "integer") return(as.integer(x))
        if (class %in% c("double", "numeric")) return(x)
    }

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
        shush(warning("There's no date to parse.", call. = FALSE), quiet)
        lubridate::as_date(NA)
    } else if (class == "posixct") {
        x <- as.POSIXct(hms::as_hms(as.numeric(x)))
        lubridate::force_tz(x, tz = tz)
    } else if (class == "posixlt") {
        x <- as.POSIXlt(hms::as_hms(as.numeric(x)))
        lubridate::force_tz(x, tz = tz)
    }

}

#' @rdname convert
#' @export
convert.Period <- function(x, class, ..., tz = "UTC", output_unit = NULL,
                           month_length = lubridate::dmonths(),
                           year_length = lubridate::dyears(),
                           close_round = TRUE, quiet = FALSE) {

    convert.Duration(x, class, tz = tz, output_unit = output_unit,
                     month_length = month_length, year_length = year_length,
                     close_round = close_round, quiet = quiet)

}

#' @rdname convert
#' @export
convert.difftime <- function(x, class, ..., tz = "UTC", output_unit = NULL,
                             month_length = lubridate::dmonths(),
                             year_length = lubridate::dyears(),
                             close_round = TRUE, quiet = FALSE) {

    convert.hms(x, class, tz = tz, output_unit = output_unit,
                month_length = month_length, year_length = year_length,
                close_round = close_round, quiet = quiet)

}

#' @rdname convert
#' @export
convert.hms <- function(x, class, ..., tz = "UTC", output_unit = NULL,
                        month_length = lubridate::dmonths(),
                        year_length = lubridate::dyears(),
                        close_round = TRUE, quiet = FALSE) {

    checkmate::assert_string(tz)

    class <- tolower(class)

    if (class %in% c("integer", "double", "numeric") &&
        !is.null(output_unit) && !all(is.na(x))) {
        x <- convert_to_unit(x, input_unit = NULL, output_unit = output_unit,
                             month_length = month_length,
                             year_length = year_length, ignore_date = TRUE,
                             close_round = close_round, quiet = quiet)

        if (class %in% "integer") return(as.integer(x))
        if (class %in% c("double", "numeric")) return(x)
    }

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
        shush(warning("There's no date to parse", call. = FALSE), quiet)
        lubridate::as_date(NA)
    } else if (class == "posixct") {
        x <- as.POSIXct(hms::as_hms(x))
        lubridate::force_tz(x, tz = tz)
    } else if (class == "posixlt") {
        x <- as.POSIXlt(hms::as_hms(x))
        lubridate::force_tz(x, tz = tz)
    }

}

#' @rdname convert
#' @export
convert.Date <- function(x, class, ..., tz = "UTC", output_unit = NULL,
                         month_length = lubridate::dmonths(),
                         year_length = lubridate::dyears(),
                         close_round = TRUE, quiet = FALSE) {

    checkmate::assert_string(tz)

    class <- tolower(class)

    if (class %in% c("integer", "double", "numeric") &&
        !is.null(output_unit) && !all(is.na(x))) {
        x <- convert_to_unit(x, input_unit = NULL, output_unit = output_unit,
                             month_length = month_length,
                             year_length = year_length, ignore_date = FALSE,
                             close_round = close_round, quiet = quiet)

        if (class %in% "integer") return(as.integer(x))
        if (class %in% c("double", "numeric")) return(x)
    }

    if (class %in% c("duration", "period", "difftime", "hms")) {
        shush(warning("There's no time to parse", call. = FALSE), quiet)
    }

    if (class == "character") {
        as.character(x)
    } else if (class == "integer") {
        shush(as.integer(x), quiet)
    } else if (class %in% c("double", "numeric")) {
        shush(as.numeric(x), quiet)
    } else if (class == "duration") {
        lubridate::as.duration(NA)
    } else if (class == "period") {
        lubridate::as.period(NA)
    } else if (class == "difftime") {
        as.numeric(NA)
    } else if (class == "hms") {
        hms::as_hms(NA)
    } else if (class == "date") {
        lubridate::force_tz(x, tz)
    } else if (class == "posixct") {
        lubridate::force_tz(lubridate::as_datetime(x), tz = tz)
    } else if (class == "posixlt") {
        as.POSIXlt(lubridate::force_tz(lubridate::as_datetime(x), tz = tz))
    }

}

#' @rdname convert
#' @export
convert.POSIXt <- function(x, class, ..., tz = "UTC", output_unit = NULL,
                           month_length = lubridate::dmonths(),
                           year_length = lubridate::dyears(),
                           ignore_date = TRUE, close_round = TRUE,
                           quiet = FALSE) {

    checkmate::assert_string(tz)

    class <- tolower(class)

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
    }

}

#' @rdname convert
#' @export
convert.Interval <- function(x, class, ..., tz = "UTC", output_unit = NULL,
                             month_length = lubridate::dmonths(),
                             year_length = lubridate::dyears(),
                             close_round = TRUE, quiet = FALSE) {

    checkmate::assert_string(tz)

    class <- tolower(class)

    if (class %in% c("integer", "double", "numeric") &&
        !is.null(output_unit) && !all(is.na(x))) {
        x <- convert_to_unit(x, input_unit = NULL, output_unit = output_unit,
                             month_length = month_length,
                             year_length = year_length, ignore_date = NULL,
                             close_round = close_round, quiet = quiet)

        if (class %in% "integer") return(as.integer(x))
        if (class %in% c("double", "numeric")) return(x)
    }

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
        shush(warning("There's no date to parse", call. = FALSE), quiet)
        lubridate::as_date(NA)
    } else if (class == "posixct") {
        x <- as.POSIXct(hms::as_hms(as.numeric(x)))
        lubridate::force_tz(x, tz = tz)
    } else if (class == "posixlt") {
        x <- as.POSIXlt(hms::as_hms(as.numeric(x)))
        lubridate::force_tz(x, tz = tz)
    }

}

#' @rdname convert
#' @export
convert.data.frame <- function(x, class, ..., cols = NULL, where = NULL,
                               orders = NULL, tz = "UTC",
                               input_unit = NULL, output_unit = NULL,
                               month_length = lubridate::dmonths(),
                               year_length = lubridate::dyears(),
                               close_round = TRUE, quiet = FALSE) {

    checkmate::assert_function(where, null.ok = TRUE)
    checkmate::assert_string(tz)

    if (!is.null(cols)) {
        checkmate::assert_names(cols, "unique", subset.of = names(x))
    }

    if (is.null(cols) & is.null(where)) {
        stop("`cols` and `where` cannot both be `NULL`.", call. = FALSE)
    }

    call <- function(x) {
        convert(x, class, orders = orders, tz = tz, input_unit = input_unit,
                output_unit = output_unit, month_length = month_length,
                year_length = year_length, close_round = close_round,
                quiet = quiet)
    }

    where_function <- function(x) where(x)

    if (!is.null(where)) {
        out <- dplyr::mutate(x, dplyr::across(where(where_function), call))
        invisible(out)
    } else if (!is.null(cols)) {
        out <- dplyr::mutate(x, dplyr::across(cols, call))
        invisible(out)
    }

}


# WRAPPERS =====

#' @rdname convert
#' @export
convert_tu <- function(x, output_unit, ...) {

    assert_time(x)

    convert(x, class = "numeric", output_unit = output_unit, ... = ...)

}

#' @rdname convert
#' @export
convert_ut <- function(x, class, input_unit, ...) {

    checkmate::assert_numeric(x)

    convert(x, class, input_unit = input_unit, ... = ...)

}

#' @rdname convert
#' @export
convert_tt <- function(x, class, ...) {

    assert_time(x)

    convert(x, class, ... = ...)

}

#' @rdname convert
#' @export
convert_uu <- function(x, input_unit, output_unit, ...) {

    checkmate::assert_numeric(x)

    convert(x, class = "numeric", input_unit = input_unit,
            output_unit = output_unit, ... = ...)

}

#' @rdname convert
#' @export
convert_pt <- function(x, class, orders, ...) {

    checkmate::assert_multi_class(x, c("character", "numeric"))

    convert(x, class, orders = orders, ... = ...)

}

#' @rdname convert
#' @export
convert_pu <- function(x, orders, output_unit, ...) {

    checkmate::assert_multi_class(x, c("character", "numeric"))

    convert(x, class = "numeric", orders = orders,
            output_unit = output_unit, ... = ...)

}


# HELPERS =====

#' @family utility functions
#' @noRd
parse_to_date_time <- function(x, orders = c("HMS", "HM", "H"), tz = "UTC",
                               quiet = FALSE) {

    # Check arguments -----

    checkmate::assert_multi_class(x, c("character", "integer", "numeric"))
    checkmate::assert_character(orders, min.chars = 1, any.missing = FALSE,
                                all.missing = FALSE, min.len = 1, unique = TRUE)
    checkmate::assert_string(tz)
    checkmate::assert_flag(quiet)

    # Set values -----

    out <- x
    if (is.character(out)) fix_character(out)

    # Parse to date/time -----

    if ((any(is.na(out)) || any(is.null(out))) && length(out) == 1) {
        as.POSIXct(NA)
    } else if (length(orders) == 1 && grepl("^(H)?(M)?(S)?$", orders[1])) {
        pattern_1 <- "^([-+])?\\d+(.\\d+)?$"
        pattern_2 <- paste0("^([-+])?\\d{1,2}(:)?[0-5]\\d$", "|",
                            "^([-+])?\\d{3,}:[0-5]\\d$")
        pattern_3 <- paste0("^([-+])?\\d{1,2}(:)?[0-5]\\d(:)?[0-5]\\d$",
                            "|", "^([-+])?\\d{3,}:[0-5]\\d(:)?[0-5]\\d$")

        assign_signal <- function(out, x) {
            out <- dplyr::case_when(
                grepl("^-", x) ~ - out,
                TRUE ~ out
            )
        }

        check_nas <- function(out, x, quiet = FALSE) {

            if (isTRUE(quiet)) return()
            na_diff <- length(which(is.na(out))) - length(which(is.na(x)))

            if (all(is.na(out))) {
                shush(warning("All formats failed to parse. No formats found.",
                              call. = FALSE), quiet = quiet)
            } else if (na_diff > 0) {
                shush(warning(na_diff, " failed to parse.",
                              call. = FALSE), quiet = quiet)
            }
        }

        if (orders[1] %in% c("H", "M", "S")) {
            parse_1 <- function(out, x, orders){
                out <- paste(out, orders)
                out <- hms::as_hms(as.integer(lubridate::duration(out)))
                out <- assign_signal(out, x)
                out
            }

            out <- dplyr::case_when(
                grepl(pattern_1, out) | is.na(out) ~
                    parse_1(out, x, orders)
            )

            check_nas(out, x, quiet = quiet)
        } else if (orders[1] %in% c("HM")) {
            parse_2 <- function(out, x) {
                pattern_h <- "\\d+(?=(:)?[0-5]\\d)"
                hours <- as.integer(str_extract_(out, pattern_h))
                pattern_m <- "[0-5]\\d$"
                minutes <- as.integer(str_extract_(out, pattern_m))

                out <- lubridate::dhours(hours) + lubridate::dminutes(minutes)
                out <- hms::as_hms(as.integer(out))
                out <- assign_signal(out, x)
                out
            }

            out <- dplyr::case_when(
                grepl(pattern_2, out) | is.na(out) ~
                    parse_2(out, x)
            )

            check_nas(out, x, quiet = quiet)
        } else if (orders[1] %in% c("HMS")) {
            parse_3 <- function(out, x) {
                pattern_h <- paste0("\\d{3,}(?=:[0-5]\\d(:)?[0-5]\\d)", "|",
                                    "([-+])?\\K\\d{1,2}",
                                    "(?=(:)?[0-5]\\d(:)?[0-5]\\d)")
                hours <- as.integer(str_extract_(out, pattern_h))
                pattern_m <- paste0(":\\K[0-5]\\d(?=(:)?[0-5]\\d)", "|",
                                    "([-+])?\\d{1,2}\\K[0-5]\\d",
                                    "(?=(:)?[0-5]\\d)")
                minutes <- as.integer(str_extract_(out, pattern_m))
                pattern_s <- "[0-5]\\d$"
                seconds <- as.integer(str_extract_(out, pattern_s))

                out <- lubridate::dhours(hours) + lubridate::dminutes(minutes) +
                    lubridate::dseconds(seconds)
                out <- hms::as_hms(as.integer(out))
                out <- assign_signal(out, x)
                out
            }

            out <- dplyr::case_when(
                grepl(pattern_3, out) | is.na(out) ~
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

#' @family utility functions
#' @noRd
convert_to_seconds <- function(x, input_unit = NULL,
                               month_length = lubridate::dmonths(),
                               year_length = lubridate::dyears(),
                               ignore_date = TRUE, quiet = FALSE) {

    classes <- c("integer", "double", "numeric", "Duration",  "Period",
                 "difftime", "hms", "Date", "POSIXct", "POSIXlt", "Interval")

    choices <- c("S", "M", "H", "d", "W", "m", "y", "date_decimal",
                 "rad", "deg")

    checkmate::assert_multi_class(x, classes, null.ok = FALSE)
    checkmate::assert_choice(input_unit, choices, null.ok = TRUE)
    checkmate::assert_numeric(month_length, lower = 0, len = 1)
    checkmate::assert_numeric(year_length, lower = 0, len = 1)
    checkmate::assert_flag(ignore_date)
    checkmate::assert_flag(quiet)

    if (!is_time(x) && is.null(input_unit)) {
        stop("When `x` is numeric, `input_unit` cannot be `NULL`.",
             call. = FALSE)
    }

    ## rad_second <- (2 * pi) / 24 / 60 / 60
    rad_second <- pi / 43200 # rad equivalent to 1 second
    ## deg_second <- 15 / (60 * 60)
    deg_second <- 15 / 3600 # degree equivalent to 1 second

    if (any(class(x) %in% c("integer", "double", "numeric"))) {
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
        }
    } else {
        shush(warning("NAs introduced by coercion.", call. = FALSE), quiet)
        as.numeric(rep(NA, length(x)))
    }

}

#' @family utility functions
#' @noRd
convert_to_unit <- function(x, input_unit = NULL, output_unit = "H",
                            month_length = lubridate::dmonths(),
                            year_length = lubridate:: dyears(),
                            ignore_date = TRUE, close_round = TRUE,
                            quiet = FALSE) {

    choices <- c("S", "M", "H", "d", "W", "m", "y", "date_decimal",
                 "rad", "deg")

    checkmate::assert_choice(output_unit, choices, null.ok = TRUE)

    ## rad_second <- (2 * pi) / 24 / 60 / 60
    rad_second <- pi / 43200 # rad equivalent to 1 second
    ## deg_second <- 15 / (60 * 60)
    deg_second <- 15 / 3600 # degree equivalent to 1 second

    if (output_unit == "date_decimal") {
        if (lubridate::is.Date(x) || lubridate::is.POSIXt(x)) {
            return(lubridate::decimal_date(x))
        } else {
            stop('When `output_unit` is equal to "date_decimal" ',
                 '`x` must be class `Date` or `POSIXt`.',
                 call. = FALSE)
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
    }

    if (isTRUE(close_round)) {
        close_round(x, 5)
    } else {
        x
    }

}

#' @family utility functions
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

    convert(lubridate::dhours(x), class, tz = tz)

}
