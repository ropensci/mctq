#' Convert an object to another
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `convert()` converts an object to another object of a predefined class. Its
#' mission is to facilitate conversions between any kind of object in a
#' simple and fast way.
#'
#' This function also supports date/time parsing and value transformations.
#' Check the Details section to learn more.
#'
#' @details
#'
#' `convert()` was designed to be a simple to use function with powerful
#' applications. However, please note that it may not work for all cases.
#' Test it first before applying it to your code.
#'
#' To learn more about how to handle date and time objects, see chapter "Dates
#' and Times" from Wickham and Grolemund (n.d.) and chapter "Technical
#' Representation of Data" from Loo and Jonge (2018).
#'
#' ## Wrappers
#'
#' `convert()` has some wrapper functions for convenience.
#'
#' * `convert_tu()` helps make conversions from date/time objects to units.
#' * `convert_ut()` helps make conversions from units to date/time objects.
#' * `convert_uu()` helps make conversions from unit to other units.
#' * `convert_pt()` helps make conversions from `character` or `numeric` objects
#' to date/time objects.
#' * `convert_pu()` helps make conversions from `character` or `numeric` objects
#' to units.
#'
#' ## `class` argument
#'
#' The `class` argument indicates the desired class of the output object . Most
#' of the methods can accept the following values: `"character"`, `"integer"`,
#' `"double"`, `"numeric"`, `"Duration"`, `"Period"`, `"difftime"`, `"hms"`,
#' `"Date"`, `"POSIXct"`, and `"POSIXlt"` (case insensitive).
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
#' Valid values for those two arguments are:
#'
#' * `"S"`: for decimal seconds.
#' * `"M"`: for decimal minutes.
#' * `"H"`: for decimal hours.
#' * `"d"`: for decimal days.
#' * `"W"`: for decimal weeks.
#' * `"m"`: for decimal months.
#' * `"y"`: for decimal years.
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
#' `numeric` objects to date/time. Since `parse_date_time()` outputs a `POSIXct`
#' object, `character` and `numeric` inputs cannot have time values equal or
#' greater than 24 hours.
#'
#' That limits the set of `convert()` applications (e.g., when you want to
#' parse a `character` to a `Duration` object of 35 hours). To get around this,
#' some exceptions were made to `orders` __equal__ to `"H"`, `"M"`, `"S"`,
#' `"HM"`, or `"HMS"`. For `"HM"` and `"HMS"` exceptions, minutes and seconds
#' are limited to `[0-59]`, and, when hours exceed 2 digits, a `:` must be
#' allocated between hours and minutes.
#'
#' ## Converting columns of a data frame
#'
#' `convert()` also allow direct conversions of data frame columns. This is
#' made with the help of [dplyr::mutate()].
#'
#' Operations with data frames are only column-wise and can be made by selecting
#' specific columns (using the `col` argument) or a group of columns (by
#' applying a flag function (e.g., `is.numeric`) in the `where` argument).
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
#' You can also parse a `character` object and transform it directly into a
#' unit. See the Examples section to know how.
#'
#' * When `class = "numeric"` or `class = "double"`
#'
#' `convert()` will return a [base::as.numeric()] output if `class` is set to
#' `"numeric"` or `"double"`. For `Date` objects, the output will be the total
#' of days since '1970-01-01' (UNIX epoch date). For date-time objects (e.g.,
#' `POSIXt`), the output will be the total of seconds from the UNIX epoch
#' (`1970-01-01 00:00:00 UTC`) (See
#' [Unix time](https://en.wikipedia.org/wiki/Unix_time) to learn more). For time
#' objects, (e.g., `hms`) the output will be the total of seconds.
#'
#' The output `class = "numeric"` can also be different if `input_unit` and
#' `output_unit` are assigned.
#'
#' * `Interval` objects
#'
#' `Interval` objects are treated like `difftime` objects. That is,
#' `Interval` objects will be converted to the interval time span.
#'
#' ## Year and month lengths
#'
#' The length of months and years can vary. For example, March has 31 days,
#' while April has 30. Due to leap years, the same can be said to year lengths.
#'
#' To address this problem, `convert()` use by default the mean of possible
#' values for months and years, used to calculate month and year durations in
#' the lubridate package (see: [lubridate::dmonths()] and
#' [lubridate::dyears()]). You can reset this by assigning other values to
#' `month_length` and `year_length` arguments.
#'
#' `month_length` and `year_length` values must be assigned with the number of
#' seconds equivalent to the unit duration. You can also assign a lubridate
#' `Duration` object (see [lubridate::duration()]).
#'
#' @param x Any R object, provided that convert() has a method to it.
#' @param class A string indicating the class of the output.
#' @param ... (optional) additional arguments to be passed to or from methods.
#' @param orders (optional) a `character` object of date/time formats to parse a
#'   `x` object of class `character` or `numeric`.
#' @param tz (optional) a string indicating the time zone with which to
#'   convert/parse `x` (default: `"UTC"`).
#' @param input_unit (optional) a string indicating the unit of `x`.
#' @param output_unit (optional) a string indicating the desire output unit.
#' @param month_length (optional) a `Duration` value __or__ a non-negative
#'   `numeric` value corresponding to the number of seconds equivalent to the
#'   month length (default: `lubridate::dmonths()`, which is equivalent to
#'   30.4375 days or 2629800 seconds).
#' @param year_length (optional) a `Duration` value __or__ a non-negative
#'   `numeric` value corresponding to the number of seconds equivalent to the
#'   year length (default: `lubridate::dyears()`, which is equivalent to 365.25
#'   days or 31557600 seconds).
#' @param close_round (optional) a `logical` value indicating if numbers with
#' decimals starting with three leading 0s or 9s must be rounded
#' (e.g., 1.999) (default: `TRUE`).
#' @param cols (optional) a `character` object indicating the column names in
#'   `x` to transform (default: `NULL`).
#' @param where (optional) a function to apply in a [tidyselect::where()] call
#'   that flags the column names in `x` to transform them (default: `NULL`).
#' @param quiet (optional) a `logical` value indicating if warnings or messages
#'   must be suppressed (default: `FALSE`).
#'
#' @return An object of the indicated class in `class`.
#'
#' @template references_d
#' @family utility functions
#' @export
#'
#' @examples
#' ## Converting from date/time objects to units
#'
#' convert(lubridate::dhours(), "numeric", output_unit = "M")
#' #> [1] 60 # Expected
#' convert(lubridate::days(), "numeric", output_unit = "rad")
#' #> [1] 6.283185 # Expected
#' x <- lubridate::as_datetime("1985-10-20 12:00:00")
#' convert(x, "numeric", output_unit = "d")
#' #> [1] 0.5 # Expected
#' x <- lubridate::as_datetime("1985-10-20 12:00:00")
#' convert(x, "numeric", output_unit = "d", ignore_date = FALSE)
#' #> [1] 5771.5 # Expected (days since the UNIX epoch)
#' convert(hms::parse_hm("15:45:00"), "numeric", output_unit = "H")
#' #> [1] 15.75 # Expected
#'
#' ## Converting from units to date/time objects
#'
#' convert(360, "Period", input_unit = "deg")
#' #> [1] "1d 0H 0M 0S" # Expected
#' convert(6.5, "POSIXct", input_unit = "H")
#' #> [1] "1970-01-01 06:30:00 UTC" # Expected
#' convert(365.25, "hms", input_unit = "d")
#' #> 8766:00:00 # Expected
#' convert(1, "Posixlt", input_unit = "W")
#' #> [1] "1970-01-08 UTC" # Expected
#' convert(1.308997, "Duration", input_unit = "rad")
#' #> [1] "18000s (~5 hours)" # Expected
#'
#' ## Converting between date/time objects
#'
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
#'
#' ## Converting between units
#'
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
#'
#' ## Converting from 'character' or 'numeric' to date/time objects
#'
#' convert("19:55:17", "Duration", orders = "HMS")
#' #> [1] "71717s (~19.92 hours)" # Expected
#' convert("21:00", "Period", orders = "HM")
#' #> [1] "21H 0M 0S" # Expected
#' convert(1, "difftime", orders = "H", quiet = TRUE)
#' #> Time difference of 3600 secs # Expected
#' convert("10:00 PM", "hms", orders = "IMp", quiet = TRUE)
#' #> 22:00:00 # Expected
#' convert("2020-01-01 10:00:00", "Date", orders = "ymd HMS", quiet = TRUE)
#' #> [1] "2020-01-01" # Expected
#' convert(13, "POSIXct", orders = "H")
#' #> [1] "1970-01-01 13:00:00 UTC" # Expected
#' convert("2020-01-01 12:31:05", "POSIXct", orders = "ymd HMS", tz = "EST")
#' #> [1] "2020-01-01 12:31:05 EST" # Expected
#' convert("03/07/1982 13:00", "POSIXlt", orders = "dmy HM")
#' #> [1] "1982-07-03 13:00:00 UTC" # Expected
#'
#' ## Converting from 'character' or 'numeric' objects to units
#'
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
#'
#' ## Converting columns of a data frame
#'
#' data <- data.frame(a = 1, b = 2)
#' data
#'
#' out <- convert(data, "duration", cols = c("a"), orders = "H")
#' out
#'
#' out <- convert(data, "hms", where = is.numeric, input_unit = "H")
#' out
convert <- function(x, class, ...) {
    if (identical(x, NA)) {
        NA
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
                              close_round = TRUE, quiet = FALSE) {
    choices <- tolower(
        c("logical", "character", "integer", "double", "numeric", "Duration",
          "Period", "difftime", "hms", "Date", "POSIXct", "POSIXlt"))

    checkmate::assert_choice(tolower(class), choices)
    checkmate::assert_character(orders, min.chars = 1, any.missing = FALSE,
                                all.missing = FALSE, min.len = 1, unique = TRUE,
                                null.ok = TRUE)
    checkmate::assert_string(tz)
    checkmate::assert_flag(quiet)

    args <- as.list(match.call()[-1])
    if (is.null(orders) && (!is.null(input_unit) || !is.null(output_unit))) {
        return(do.call("parser_1", args))
    } else if (!is.null(orders)) {
        return(do.call("parser_2", args))
    }

    class <- tolower(class)
    x <- fix_character(x)

    if (!(class %in% c("logical", "character"))) {
        shush(cli::cli_alert_warning(paste0(
            "'x' was converted 'as is'. This can produce ",
            "'NA' values. Did you forgot to assign values to ",
            "'orders' or 'input_unit'/'output_unit'?"
        )), quiet)
    }

    if (class == "logical") {
        shush(cli::cli_alert_warning(paste0(
            "'x' was converted 'as is'."
        )), quiet)
        as.logical(x)
    } else if (class == "character") {
        x
    } else if (class == "integer") {
        shush(as.integer(x))
    } else if (class %in% c("double", "numeric")) {
        shush(as.numeric(x))
    } else if (class == "duration") {
        lubridate::duration(x)
    } else if (class == "period") {
        lubridate::period(x)
    } else if (class == "difftime") {
        shush(cli::cli_alert_warning(paste0(
            "'difftime' units was set to seconds."
        )), quiet)
        lubridate::as.difftime(rep("NA", length(x)), units = "secs")
    } else if (class == "hms") {
        hms::hms(rep(NA, length(x)))
    } else if (class == "date") {
        shush(lubridate::as_date(x))
    } else if (class == "posixct") {
        x <- shush(lubridate::as_datetime(x))
        lubridate::force_tz(x, tz = tz)
    } else if (class == "posixlt") {
        x <- as.POSIXlt(shush(lubridate::as_datetime(x)))
        lubridate::force_tz(x, tz = tz)
    }
}

#' @rdname convert
#' @export
convert.numeric <- function(x, class, ..., orders = NULL, tz = "UTC",
                            input_unit = NULL, output_unit = NULL,
                            month_length = lubridate::dmonths(),
                            year_length = lubridate::dyears(),
                            close_round = TRUE, quiet = FALSE) {
    choices <- tolower(
        c("logical", "character", "integer", "double", "numeric", "Duration",
          "Period", "difftime", "hms", "Date", "POSIXct", "POSIXlt"))

    checkmate::assert_choice(tolower(class), choices)
    checkmate::assert_character(orders, min.chars = 1, any.missing = FALSE,
                                all.missing = FALSE, min.len = 1, unique = TRUE,
                                null.ok = TRUE)
    checkmate::assert_string(tz)
    checkmate::assert_flag(quiet)

    args <- as.list(match.call()[-1])
    if (is.null(orders) && (!is.null(input_unit) || !is.null(output_unit))) {
        return(do.call("parser_1", args))
    } else if (!is.null(orders)) {
        return(do.call("parser_2", args))
    }

    class <- tolower(class)

    if (!(class %in% c("logical", "character", "integer", "double",
                       "numeric"))) {
        shush(cli::cli_alert_warning(paste0(
            "'x' was converted 'as is'. This can produce ",
            "'NA' values. Did you forgot to assign values to ",
            "'orders' or 'input_unit'/'output_unit'?"
        )), quiet)
    }

    if (class == "logical") {
        shush(cli::cli_alert_warning(paste0(
            "'x' was converted 'as is'."
        )), quiet)
        as.logical(x)
    } else if (class == "character") {
        as.character(x)
    } else if (class == "integer") {
        as.integer(x)
    } else if (class %in% c("double", "numeric")) {
        x
    } else if (class == "duration") {
        lubridate::duration(x)
    } else if (class == "period") {
        lubridate::as.period(hms::hms(x))
    } else if (class == "difftime") {
        shush(cli::cli_alert_warning(paste0(
            "'difftime' units was set to seconds."
        )), quiet)
        lubridate::as.difftime(x, units = "secs")
    } else if (class == "hms") {
        hms::hms(x)
    } else if (class == "date") {
        lubridate::as_date(x)
    } else if (class == "posixct") {
        shush(cli::cli_alert_warning(paste0(
            "'POSIXct' origin was set as '1970-01-01 UTC'."
        )), quiet)
        lubridate::as_datetime(x, tz = tz)
    } else if (class == "posixlt") {
        shush(cli::cli_alert_warning(paste0(
            "'POSIXlt' origin was set as '1970-01-01 UTC'."
        )), quiet)
        as.POSIXlt(lubridate::as_datetime(x), tz = tz)
    }
}

#' @rdname convert
#' @export
convert.Duration <- function(x, class, ..., tz = "UTC", output_unit = NULL,
                             month_length = lubridate::dmonths(),
                             year_length = lubridate::dyears(),
                             close_round = TRUE, quiet = FALSE) {
    choices <- tolower(
        c("logical", "character", "integer", "double", "numeric", "Duration",
          "Period", "difftime", "hms", "Date", "POSIXct", "POSIXlt"))

    checkmate::assert_choice(tolower(class), choices)
    checkmate::assert_string(tz)
    checkmate::assert_flag(quiet)

    args <- c(ignore_date = TRUE, as.list(match.call()[-1]))
    if (!is.null(output_unit)) return(do.call("parser_3", args))

    class <- tolower(class)

    if (class == "logical") {
        shush(cli::cli_alert_warning(paste0(
            "'x' cannot be converted to 'logical'."
        )), quiet)
        as.logical(rep(NA, length(x)))
    } else if (class == "character") {
        shush(cli::cli_alert_warning(paste0(
            "'x' was formatted as HMS."
        )), quiet)
        as.character(hms::as_hms(as.numeric(x)))
    } else if (class == "integer") {
        shush(cli::cli_alert_warning(paste0(
            "'x' was converted to total of full seconds."
        )), quiet)
        as.integer(x)
    } else if (class %in% c("double", "numeric")) {
        shush(cli::cli_alert_warning(paste0(
            "'x' was converted to total of seconds."
        )), quiet)
        shush(as.numeric(x), quiet)
    } else if (class == "duration") {
        lubridate::as.duration(x)
    } else if (class == "period") {
        lubridate::as.period(x)
    } else if (class == "difftime") {
        shush(cli::cli_alert_warning(paste0(
            "'difftime' units was set to seconds."
        )), quiet)
        lubridate::as.difftime(x)
    } else if (class == "hms") {
        hms::as_hms(as.numeric(x))
    } else if (class == "date") {
        shush(cli::cli_alert_warning(paste0(
            "There's no date to convert."
        )), quiet)
        lubridate::as_date(rep(NA, length(x)))
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
    choices <- tolower(
        c("logical", "character", "integer", "double", "numeric", "Duration",
          "Period", "difftime", "hms", "Date", "POSIXct", "POSIXlt"))

    checkmate::assert_choice(tolower(class), choices)
    checkmate::assert_string(tz)
    checkmate::assert_flag(quiet)

    args <- c(ignore_date = TRUE, as.list(match.call()[-1]))
    if (!is.null(output_unit)) return(do.call("parser_3", args))

    class <- tolower(class)

    if (class == "logical") {
        shush(cli::cli_alert_warning(paste0(
            "'x' cannot be converted to 'logical'."
        )), quiet)
        as.logical(rep(NA, length(x)))
    } else if (class == "character") {
        shush(cli::cli_alert_warning(paste0(
            "'x' was formatted as HMS."
        )), quiet)
        as.character(hms::as_hms(x))
    } else if (class == "integer") {
        shush(cli::cli_alert_warning(paste0(
            "'x' was converted to total of full seconds."
        )), quiet)
        shush(as.integer(x), quiet)
    } else if (class %in% c("double", "numeric")) {
        shush(cli::cli_alert_warning(paste0(
            "'x' was converted to total of seconds."
        )), quiet)
        shush(as.numeric(x), quiet)
    } else if (class == "duration") {
        lubridate::as.duration(x)
    } else if (class == "period") {
        lubridate::as.period(x)
    } else if (class == "difftime") {
        shush(cli::cli_alert_warning(paste0(
            "'difftime' units was set to seconds."
        )), quiet)
        lubridate::as.difftime(lubridate::as.duration(x))
    } else if (class == "hms") {
        hms::as_hms(x)
    } else if (class == "date") {
        shush(cli::cli_alert_warning(paste0(
            "There's no date to convert."
        )), quiet)
        lubridate::as_date(rep(NA, length(x)))
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
convert.Date <- function(x, class, ..., tz = "UTC", quiet = FALSE) {
    choices <- tolower(
        c("logical", "character", "integer", "double", "numeric", "Duration",
          "Period", "difftime", "hms", "Date", "POSIXct", "POSIXlt"))

    checkmate::assert_choice(tolower(class), choices)
    checkmate::assert_string(tz)
    checkmate::assert_flag(quiet)

    class <- tolower(class)

    if (class %in% c("integer", "double", "numeric")) {
        shush(cli::cli_alert_warning(paste0(
            "'x' was converted to total of days since ",
            "'1970-01-01' (UNIX epoch)."
        )), quiet)
    }

    if (class %in% c("duration", "period", "difftime", "hms")) {
        shush(cli::cli_alert_warning(paste0(
            "There's no time to convert."
        )), quiet)
    }

    if (class == "logical") {
        shush(cli::cli_alert_warning(paste0(
            "'x' cannot be converted to 'logical'."
        )), quiet)
        as.logical(rep(NA, length(x)))
    } else if (class == "character") {
        as.character(x)
    } else if (class == "integer") {
        shush(as.integer(x), quiet)
    } else if (class %in% c("double", "numeric")) {
        shush(as.numeric(x), quiet)
    } else if (class == "duration") {
        lubridate::as.duration(rep(NA, length(x)))
    } else if (class == "period") {
        lubridate::as.period(rep(NA, length(x)))
    } else if (class == "difftime") {
        shush(cli::cli_alert_warning(paste0(
            "'difftime' units was set to seconds."
        )), quiet)
        lubridate::as.difftime(lubridate::as.duration(rep(NA, length(x))))
    } else if (class == "hms") {
        hms::as_hms(rep(NA, length(x)))
    } else if (class == "date") {
        lubridate::as_date(x)
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
                           close_round = TRUE, quiet = FALSE) {
    choices <- tolower(
        c("logical", "character", "integer", "double", "numeric", "Duration",
          "Period", "difftime", "hms", "Date", "POSIXct", "POSIXlt"))

    checkmate::assert_choice(tolower(class), choices)
    checkmate::assert_string(tz)
    checkmate::assert_flag(quiet)

    args <- as.list(match.call()[-1])
    if (!is.null(output_unit)) return(do.call("parser_3", args))

    class <- tolower(class)

    if (class %in% c("duration", "period", "difftime", "hms")) {
        shush(cli::cli_alert_warning(paste0(
            "'x' date was discarded. Only 'x' time was considered."
        )), quiet)
    }

    if (class == "logical") {
        shush(cli::cli_alert_warning(paste0(
            "'x' cannot be converted to 'logical'."
        )), quiet)
        as.logical(rep(NA, length(x)))
    } else if (class == "character") {
        as.character(x)
    } else if (class == "integer") {
        shush(cli::cli_alert_warning(paste0(
            "'x' was converted to total of full seconds since ",
            "'1970-01-01 00:00:00' (UNIX epoch)."
        )), quiet)
        shush(as.integer(x), quiet)
    } else if (class %in% c("double", "numeric")) {
        shush(cli::cli_alert_warning(paste0(
            "'x' was converted to total of seconds since ",
            "'1970-01-01 00:00:00' (UNIX epoch)."
        )), quiet)
        shush(as.numeric(x), quiet)
    } else if (class == "duration") {
        lubridate::as.duration(hms::as_hms(x))
    } else if (class == "period") {
        lubridate::as.period(hms::as_hms(x))
    } else if (class == "difftime") {
        shush(cli::cli_alert_warning(paste0(
            "'difftime' units was set to seconds."
        )), quiet)
        lubridate::as.difftime(as.numeric(hms::as_hms(x)), units = "secs")
    } else if (class == "hms") {
        hms::as_hms(x)
    } else if (class == "date") {
        lubridate::as_date(x)
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
    choices <- tolower(
        c("logical", "character", "integer", "double", "numeric", "Duration",
          "Period", "difftime", "hms", "Date", "POSIXct", "POSIXlt"))

    checkmate::assert_choice(tolower(class), choices)
    checkmate::assert_string(tz)
    checkmate::assert_flag(quiet)

    args <- c(ignore_date = NULL, as.list(match.call()[-1]))
    if (!is.null(output_unit)) return(do.call("parser_3", args))

    class <- tolower(class)

    if (class %in% c("duration", "period", "difftime", "hms")) {
        shush(cli::cli_alert_warning(paste0(
            "'x' was converted to the interval time span."
        )), quiet)
    }

    if (class %in% c("posixct", "posixlt")) {
        shush(cli::cli_alert_warning(paste0(
            "'x' was converted to the interval time span with ",
            "'1970-01-01 as origin (UNIX epoch)."
        )), quiet)
    }

    if (class == "logical") {
        shush(cli::cli_alert_warning(paste0(
            "'x' cannot be converted to 'logical'."
        )), quiet)
        as.logical(rep(NA, length(x)))
    } else if (class == "character") {
        as.character(x)
    } else if (class == "integer") {
        shush(cli::cli_alert_warning(paste0(
            "'x' was converted to total of full seconds of the ",
            "interval time span."
        )), quiet)
        shush(as.integer(x), quiet)
    } else if (class %in% c("double", "numeric")) {
        shush(cli::cli_alert_warning(paste0(
            "'x' was converted to total of seconds of the interval ",
            "time span."
        )), quiet)
        shush(as.numeric(x), quiet)
    } else if (class == "duration") {
        lubridate::as.duration(x)
    } else if (class == "period") {
        lubridate::as.period(x)
    } else if (class == "difftime") {
        shush(cli::cli_alert_warning(paste0(
            "'difftime' units was set to seconds."
        )), quiet)
        lubridate::as.difftime(x)
    } else if (class == "hms") {
        hms::hms(x)
    } else if (class == "date") {
        shush(cli::cli_alert_warning(paste0(
            "There's no sigle date to convert."
        )), quiet)
        lubridate::as_date(rep(NA, length(x)))
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
                               close_round = TRUE, quiet = TRUE) {
    choices <- tolower(
        c("logical", "character", "integer", "double", "numeric", "Duration",
          "Period", "difftime", "hms", "Date", "POSIXct", "POSIXlt"))

    checkmate::assert_choice(tolower(class), choices)
    checkmate::assert_function(where, null.ok = TRUE)
    checkmate::assert_string(tz)
    checkmate::assert_flag(quiet)

    if (!is.null(cols)) {
        checkmate::assert_names(cols, "unique", subset.of = names(x))
    }

    if (is.null(cols) & is.null(where)) {
        cli::cli_abort("'cols' and 'where' cannot both be 'NULL'.")
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
    assert_temporal(x)

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

parser_1 <- function(x, class, ..., orders = NULL, tz = "UTC",
                     input_unit = NULL, output_unit = NULL,
                     month_length = lubridate::dmonths(),
                     year_length = lubridate::dyears(),
                     ignore_date = TRUE, close_round = TRUE, quiet = FALSE) {
    class <- tolower(class)
    if (is.character(x)) x <- fix_character(x)

    if (!identical(count_na(x), count_na(shush(as.numeric(x))))) {
        cli::cli_abort(paste0(
            "To convert 'character' objects to units, all values must be ",
            "able to be coerced to 'numeric'. Try `as.numeric(x)` to check ",
            "for errors."
        ))
    }

    if (!is.null(output_unit) && is.null(input_unit)) {
        cli::cli_abort(paste0(
            "'x' can only be converted to 'output_unit' if 'input_unit' ",
             "is assigned."
        ))
    }

    if (!is.null(output_unit) &&
        !(class %in% c("integer", "double", "numeric"))) {
        cli::cli_abort(paste0(
            "'x' can only be converted to 'output_unit' if 'class' ",
            "is 'integer', 'double', or 'numeric'."
        ))
    }

    if (class %in% c("integer", "double", "numeric") &&
        (is.null(input_unit) || is.null(output_unit))) {
        cli::cli_abort(paste0(
            "input_unit' and 'output_unit' must both be assigned, or be ",
            "'NULL', when 'class' is equal to {single_quote_(class)}."
        ))
    }

    if (!is.null(input_unit) && !is.null(output_unit)) {
        x <- convert_to_unit(shush(as.numeric(x)),
                             input_unit = input_unit,
                             output_unit = output_unit,
                             month_length = month_length,
                             year_length = year_length,
                             ignore_date = ignore_date,
                             close_round = close_round,
                             quiet = quiet)

        convert(x, class, tz = tz, quiet = TRUE)
    } else if (!is.null(input_unit)) {
        convert_to_date_time(shush(as.numeric(x)),
                             class = class, tz = tz,
                             input_unit = input_unit,
                             month_length = month_length,
                             year_length = year_length,
                             close_round = close_round,
                             quiet = quiet)
    }
}

parser_2 <- function(x, class, ..., orders = NULL, tz = "UTC",
                     input_unit = NULL, output_unit = NULL,
                     month_length = lubridate::dmonths(),
                     year_length = lubridate::dyears(),
                     ignore_date = TRUE, close_round = TRUE, quiet = FALSE) {
    class <- tolower(class)
    if (is.character(x)) fix_character(x)
    x <- parse_to_date_time(x, orders, tz, quiet)

    if (class %in% c("integer", "double", "numeric") &&
        !is.null(output_unit) && !all(is.na(x))) {
        x <- convert_to_unit(x, input_unit = NULL,
                             output_unit = output_unit,
                             month_length = month_length,
                             year_length = year_length,
                             ignore_date = ignore_date,
                             close_round = close_round,
                             quiet = quiet)

        if (class == "integer") return(as.integer(x))
        if (class %in% c("double", "numeric")) return(x)
    } else {
        convert(x, class, tz = tz, quiet = TRUE)
    }
}

parser_3 <- function(x, class, ..., output_unit = NULL,
                     month_length = lubridate::dmonths(),
                     year_length = lubridate::dyears(),
                     ignore_date = TRUE, close_round = TRUE, quiet = FALSE) {
    class <- tolower(class)

    if (!(class %in% c("integer", "double", "numeric"))) {
        cli::cli_abort(paste0(
            "'x' can only be converted to 'output_unit' if 'class' ",
            "is 'integer', 'double' or 'numeric'."
        ))
    }

    x <- convert_to_unit(x, input_unit = NULL, output_unit = output_unit,
                         month_length = month_length,
                         year_length = year_length, ignore_date = ignore_date,
                         close_round = close_round, quiet = quiet)

    if (class == "integer") {
        as.integer(x)
    } else {
        x
    }
}

parse_to_date_time <- function(x, orders = c("HMS", "HM", "H"), tz = "UTC",
                               quiet = FALSE) {
    assert_custom_1(x)
    checkmate::assert_character(orders, min.chars = 1, any.missing = FALSE,
                                all.missing = FALSE, min.len = 1, unique = TRUE)
    checkmate::assert_string(tz)
    checkmate::assert_flag(quiet)

    out <- x
    if (is.character(out)) out <- fix_character(out)

    if ((any(is.na(out)) || is.null(out)) && length(out) == 1) {
        out <- lubridate::as_datetime(NA)
    } else if (length(orders) == 1 && grepl("^(H)?(M)?(S)?$", orders[1])) {
        pattern_1 <- "^([-+])?\\d+(.\\d+)?$"
        pattern_2 <- paste0("^([-+])?\\d{1,2}(:)?[0-5]\\d$", "|",
                            "^([-+])?\\d{3,}:[0-5]\\d$")
        pattern_3 <- paste0("^([-+])?\\d{1,2}(:)?[0-5]\\d(:)?[0-5]\\d$",
                            "|", "^([-+])?\\d{3,}:[0-5]\\d(:)?[0-5]\\d$")

        assign_signal <- function(out, x) {
            dplyr::case_when(
                grepl("^-", x) ~ - out,
                TRUE ~ out
            )
        }

        check_nas <- function(out, x, quiet = quiet) {

            if (isTRUE(quiet)) return()
            na_diff <- length(which(is.na(out))) - length(which(is.na(x)))

            if (all(is.na(out))) {
                shush(cli::cli_alert_warning(paste0(
                    "All formats failed to parse. No formats found."
                )), quiet)
            } else if (na_diff > 0) {
                shush(cli::cli_alert_warning(paste0(
                    na_diff, " failed to parse."
                )), quiet)
            }
        }

        if (orders[1] %in% c("H", "M", "S")) {
            parse_1 <- function(out, x, orders){
                out <- paste(out, orders)
                out <- hms::as_hms(as.integer(lubridate::duration(out)))
                assign_signal(out, x)
            }

            out <- dplyr::case_when(
                grepl(pattern_1, out) | is.na(out) ~ parse_1(out, x, orders),
                TRUE ~ hms::as_hms(NA)
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
                assign_signal(out, x)
            }

            out <- dplyr::case_when(
                grepl(pattern_2, out) | is.na(out) ~ parse_2(out, x),
                TRUE ~ hms::as_hms(NA)
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
                assign_signal(out, x)
            }

            out <- dplyr::case_when(
                grepl(pattern_3, out) | is.na(out) ~ parse_3(out, x),
                TRUE ~ hms::as_hms(NA)
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

convert_to_seconds <- function(x, input_unit = NULL,
                               month_length = lubridate::dmonths(),
                               year_length = lubridate::dyears(),
                               ignore_date = TRUE, quiet = FALSE) {
    classes <- c("integer", "double", "numeric", "Duration",  "Period",
                 "difftime", "hms", "Date", "POSIXct", "POSIXlt", "Interval")

    choices <- c("S", "M", "H", "d", "W", "m", "y", "rad", "deg")

    checkmate::assert_multi_class(x, classes, null.ok = FALSE)
    checkmate::assert_choice(input_unit, choices, null.ok = TRUE)
    checkmate::assert_numeric(month_length, lower = 0, len = 1)
    checkmate::assert_numeric(year_length, lower = 0, len = 1)
    checkmate::assert_flag(ignore_date)
    checkmate::assert_flag(quiet)

    if (!test_temporal(x) && is.null(input_unit)) {
        cli::cli_abort(paste0(
            "When 'x' is 'integer' or 'numeric', 'input_unit' cannot be ",
            "'NULL'."
        ))
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
        } else if (input_unit == "rad") {
            x / rad_second
        } else if (input_unit == "deg") {
            x / deg_second
        }
    } else if (test_temporal(x)) {
        if (lubridate::is.duration(x) || lubridate::is.period(x) ||
            hms::is_hms(x) || lubridate::is.interval(x)) {
            as.numeric(x)
        } else if (class(x)[1] == "difftime") {
            as.numeric(hms::as_hms(x))
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
        }
    }
}

convert_to_unit <- function(x, input_unit = NULL, output_unit = "H",
                            month_length = lubridate::dmonths(),
                            year_length = lubridate:: dyears(),
                            ignore_date = TRUE, close_round = TRUE,
                            quiet = FALSE) {
    choices <- c("S", "M", "H", "d", "W", "m", "y", "rad", "deg")

    checkmate::assert_choice(output_unit, choices, null.ok = TRUE)
    checkmate::assert_flag(close_round)

    ## rad_second <- (2 * pi) / 24 / 60 / 60
    rad_second <- pi / 43200 # rad equivalent to 1 second
    ## deg_second <- 15 / (60 * 60)
    deg_second <- 15 / 3600 # degree equivalent to 1 second

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
        close_round(x, 3)
    } else {
        x
    }
}

convert_to_date_time <- function(x, class, input_unit, tz = "UTC",
                                 month_length = lubridate::dmonths(),
                                 year_length = lubridate:: dyears(),
                                 close_round = TRUE, quiet = FALSE) {
    assert_numeric_(x)

    if (!(input_unit == "S")) {
        x <- convert_to_seconds(x, input_unit = input_unit,
                                month_length = month_length,
                                year_length = year_length, quiet = quiet)
    }

    if (isTRUE(close_round)) x <- close_round(x, 3)
    convert(lubridate::dseconds(x), class, tz = tz, quiet = TRUE)
}
