#' Convert values to date/time
#'
#' @description
#'
#' `convert_to_date_time` converts a date/time value to a predefined object
#' class in a fast and easy way. It can be use to convert/parse objects of class
#' `character`, `numeric`, `difftime`, `Duration`, `hms`, `Period`, `Date`,
#' `POSIXct`, `POSIXlt` and `Interval`.
#'
#' Conversions to decimal time and radians are also possible by using other
#' functions of the `convert_to` family. Check the details section to learn how.
#'
#' @details
#'
#' `convert_to_date_time` is heavily based on tidyverse's [lubridate::lubridate]
#' package. Its main objective is to group together several parse methods for
#' the main date/time classes.
#'
#' This function was made to help users and to assist other functions from the
#' MCTQ package. It may not work for all cases. For better results, make sure
#' that your values fit the argument parameters on the function of your choice.
#'
#' ## Orders
#'
#' This function is mainly based on lubridate's [lubridate::parse_date_time()]
#' function. If needed, please check it documentation to set the right orders
#' for your input, .
#'
#' `convert_to_date_time` is set by default to guess the most common used
#' patterns of data collection for the Munich Chronotype Questionnaire (MCTQ),
#' _i.e_ `"HMS"`, `"HM"`, and `"H"`.
#'
#' ## Limitations
#'
#' `convert_to_date_time` uses [lubridate::parse_date_time()] to convert
#' `character` and `numeric` objects. Since parse_date_time() output a `POSIXt`
#' object, `character` and `numeric` inputs cannot have time values equal or
#' greater than 24 hours.
#'
#' That limits the set of `convert_to_date_time` applications (_e.g_ when you
#' want to parse a character to a `duration` object of 35 hours and 30 minutes).
#' A exception was made to `character` and `numeric` objects with `order = "H"`.
#'
#' To go around this limitation, learn about how to use the
#' [lubridate::lubridate] package.
#'
#' ## AM/PM signaling
#'
#' If your value have an AM/PM signal in your value, `convert_to_date_time` will
#' try to fix it for you. You can also use the `"Op"` format on the `orders`
#' variable to match this indicators (check [lubridate::parse_date_time()] to
#' know more about it).
#'
#' ## Converting date/time objects to decimal time or radians (or the reverse)
#'
#' `convert_to_date_time` don't perform conversions for decimal time or radians,
#' but there are easy ways to do it with the `convert_to` family.
#'
#' * Converting a date/time object to decimal time or radian
#'
#' Use [mctq::convert_to_decimal] to convert date/time objects to decimal time,
#' or use [mctq::convert_to_rad] to convert date/time objects to radians.
#'
#' * Converting a decimal time to a date/time object
#'
#' Convert the decimal time to a date/time object using the [duration
#' family][lubridate::duration()] of functions from the lubridate package. Then,
#' use `convert_to_date_time` to convert the value to what you which.
#'
#' __EXAMPLE__: To convert a decimal time of 23.5 hours to an object of class
#' `hms`, do:
#'
#' ```
#' convert_to_date_time(lubridate::dhours(23.5), "hms")
#' #> 23:30:00
#' ```
#'
#' * Converting radians to a date/time object
#'
#' Convert the radian value to a decimal time using [mctq::convert_to_decimal].
#' Them, follow the same procedure showed above.
#'
#' __EXAMPLE__: To convert 1.309 radian to an object of class `duration`, do:
#'
#' ```
#' convert_to_date_time(lubridate::dhours(convert_to_decimal(1.309)),
#' "duration")
#' #> [1] "18000s (~5 hours)"
#' ```
#'
#' ## NAs as output
#'
#' `convert_to_date_time` will return a `NA` value when were not possible to
#' convert the input to the class assigned (sorry!).
#'
#' ## Outputs
#'
#' Your output can change according to the class value indicated on `class`.
#' Here are some examples.
#'
#' * When `class = "character"`
#'
#' `convert_to_date_time` will return a [base::as.character()] output if `class`
#' is set to `"character"`. If there's no indication of year in `x`,
#' `convert_to_date_time` will return only a character string with a `hms`
#' time.
#'
#' * When `class = "numeric"`
#'
#' `convert_to_date_time` will return a [base::as.numeric()] output if `class`
#' is set to `"numeric"`. For dates or date-time objects, the output is the
#' total of seconds from the UNIX origin date (`1970-01-01 00:00:00 UTC`) (See
#' [Unix time](https://en.wikipedia.org/wiki/Unix_time) to know more). For time
#' values, the output is the total of seconds.
#'
#' * `Interval` objects
#'
#' Class `interval` objects are treated like `difftime` objects. That is,
#' `interval` objects will be converted to the time difference of the interval.
#'
#' @section Time zone warning:
#'
#' You must be careful when working with different time zones (tz), because
#' some of them can be affected with Daylight Saving Time (DST).
#'
#' It's unlike that you going to have to worry about time zone while working
#' with the MCTQ. Because of that, `convert_to_date_time` is set to `"UTC"` by
#' default for `Date`, `POSIXct`, and `POSIXlt` outputs.
#'
#' To know more about how to handle date and time objects, check the chapter
#' "Dates and Times" from the book [R for Data
#' Science](https://r4ds.had.co.nz/dates-and-times.html) of Hadley Wickham &
#' Garrett Grolemund.
#'
#' @param x A `character` or `numeric` vector to parse to a date/time object
#'   __or__ a date/time object of one of the following classes: `difftime`,
#'   `Duration`, `hms`, `Period`, `Date`, `POSIXct`, `POSIXlt`, and `Interval`.
#' @param class A character vector indicating the class of the output. Valid
#'   values are: `"character"`, `"numeric"`, `"difftime"`, `"Duration"`,
#'   `"hms"`, `"Period"`, `"Date"`, `"POSIXct"` and `"POSIXlt"` (case
#'   insensitive).
#' @param orders A character vector of date/time formats that correspond to `x`
#'   (only needed If `x` is `character` or `numeric`). You can try to parse `x`
#'   trying one or more formats, in order of importance. Check the lubridate
#'   [lubridate::parse_date_time()] function to see the right format values for
#'   your input (default: `c("HMS", "HM", "H")`).
#' @param tz A character string that specifies the time zone with which to
#'   convert/parse `x`. Time zones are used only if `class` is set to `"Date"`,
#'   `"POSIXct"`, or `"POSIXlt"` (see [base::timezones] to know more) (default:
#'   `"UTC"`).
#'
#' @note
#'
#' To do:
#'
#' * Fix conversion for hours equal or greater to 24 for `character` and
#' `numeric` variables.
#' * Fix time attribution to `Periods` (remove functionality?).
#'
#' @return A date/time object of the indicated class.
#' @family Convert to date/time functions
#' @export
#'
#' @examples
#' ## ** conversion of character or numeric values to date/time objects **
#' convert_to_date_time("10:00 PM", "hms")
#' #> [1] 22:00:00
#' convert_to_date_time("21:00 AM", "Period")
#' #> [1] "21H 0M 0S"
#' convert_to_date_time("2020-01-01 10:00:00", "Date", "ymd HMS")
#' #> [1] "2020-01-01"
#' convert_to_date_time(13, "POSIXct", "H")
#' #> [1] "0000-01-01 13:00:00 UTC"
#' convert_to_date_time("2020-01-01 12:31:05", "POSIXct", "ymd HMS", tz = "EST")
#' #> [1] "2020-01-01 12:31:05 EST"
#' convert_to_date_time("03/07/1982 13:00", "POSIXlt", "dmy HM")
#' #> [1] "1982-07-03 13:00:00 UTC"
#'
#' ## ** conversion between date/time objects **
#' convert_to_date_time(lubridate::duration(120), "hms")
#' #> 00:02:00
#' convert_to_date_time(hms::as_hms("13:45:05"), "POSIXct")
#' #> [1] "0000-01-01 13:45:05 UTC"
#' convert_to_date_time(lubridate::period(60), "POSIXct")
#' #> [1] "0000-01-01 00:01:00 UTC"
#' convert_to_date_time(as.POSIXct("2020-01-01 12:31:05", tz = "EST"),
#'                      "POSIXct")
#' #> [1] "2020-01-01 12:31:05 UTC"
convert_to_date_time <- function(x, class, orders = c("HMS", "HM", "H"),
                       tz = "UTC") {

    # Check arguments --------------------

    assert_custom_1(x)
    checkmate::assert_string(class)
    checkmate::assert_character(orders, any.missing = FALSE)
    checkmate::assert_string(tz)

    choices <- stringr::str_to_lower(
        c("character", "numeric", "Duration", "Period", "difftime", "hms",
          "Date", "POSIXct", "POSIXlt"))
    checkmate::assert_choice(stringr::str_to_lower(class), choices)

    # Set values --------------------

    class <- stringr::str_to_lower(class)

    # Check redundancies --------------------

    if ((class == "difftime" && lubridate::is.difftime(x)) ||
        (class == "duration" && lubridate::is.duration(x)) ||
        (class == "hms" && hms::is_hms(x)) ||
        (class == "period" && lubridate::is.period(x)) ||
        (class == "date" && lubridate::is.Date(x))) {
        return(x)
    } else if ((class == "posixct" && lubridate::is.POSIXct(x)) ||
               (class == "posixlt" && lubridate::is.POSIXlt(x))) {
        return(lubridate::force_tz(x, tz))
    }

    # Set method --------------------

    ## Oh, the little hacks we have to do...

    if (lubridate::is.difftime(x) || hms::is_hms(x)) {
        return(convert_to_date_time.hms(x, class, orders, tz))
    }

    if (lubridate::is.Date(x)) {
        return(convert_to_date_time.Date(x, class, orders, tz))
    }

    if (lubridate::is.POSIXct(x) || lubridate::is.POSIXlt(x)) {
        return(convert_to_date_time.POSIXt(x, class, orders, tz))
    }

    UseMethod("convert_to_date_time")

}

#' @rdname convert_to_date_time
#' @export
convert_to_date_time.character <- function(x, class,
                                           orders = c("HMS", "HM", "H"),
                                           tz = "UTC") {

    class <- stringr::str_to_lower(class)
    x_bkp <- x

    if (!(class %in% c("date", "posixct", "posixlt"))) {
        if (all(stringr::str_detect(x, "^2[4-9]|^[3-9]\\d|\\d{3,}")) &&
            (any(orders == "H") && length(orders) == 1)) {
            x <- hms::as_hms(as.numeric(lubridate::dhours(as.numeric(x))))
        } else {
            x <- lubridate::parse_date_time(x, orders, tz = tz)
        }
    } else {
        x <- lubridate::parse_date_time(x, orders, tz = tz)
    }

    if (all(is.na(x))) return(x)

    for (i in seq_along(x)){
        if (any(stringr::str_detect(orders, "Op"))) {
            next
        }

        if (stringr::str_detect(x_bkp[i], stringr::regex("pm",
                                                     ignore_case = TRUE))) {
            x[i] <- x[i] + lubridate::dhours(12)
        }

    }

    if (class == "character") {
        output <- as.character(NULL)
        for (i in seq_along(x)) {
            if (lubridate::is.POSIXt(x[i]) &&
                lubridate::year(x[i]) == 0) {
                output[i] <- as.character(hms::as_hms(x[i]))
            } else {
                output[i] <- as.character(x[i])
            }
        }

        output
    } else if (class == "numeric") {
        output <- as.numeric(NULL)
        for (i in seq_along(x)) {
            if (lubridate::is.POSIXt(x[i]) &&
                lubridate::year(x[i]) == 0) {
                output[i] <- as.numeric(hms::as_hms(x[i]))
            } else {
                output[i] <- as.numeric(x[i])
            }
        }

        output
    } else if (class == "difftime") {
        lubridate::as.difftime(lubridate::as.duration(hms::as_hms(x)))
    } else if (class == "duration") {
        lubridate::as.duration(hms::as_hms(x))
    } else if (class == "hms") {
        hms::as_hms(x)
    } else if (class == "period") {
        lubridate::as.period(hms::as_hms(x))
    } else if (class == "date") {
        output <- as.Date(NULL)
        for (i in seq_along(x)) {
            if (lubridate::year(x[i]) == 0) {
                rlang::warn(
                    glue::glue("There's no date to parse in {x[i]}"))
                output[i] <- NA
            } else {
                output[i] <- lubridate::as_date(
                    lubridate::force_tz(x[i], tz))
            }
        }

        output
    } else if (class == "posixct") {
        as.POSIXct(x)
    } else if (class == "posixlt") {
        as.POSIXlt(x)
    } else {
        rlang::abort("Critical error")
    }

}

#' @rdname convert_to_date_time
#' @export
convert_to_date_time.numeric <- function(x, class, orders = c("HMS", "HM", "H"),
                                         tz = "UTC") {

    convert_to_date_time.character(x, class, orders, tz)

}

#' @rdname convert_to_date_time
#' @export
convert_to_date_time.difftime <- function(x, class,
                                          orders = c("HMS", "HM", "H"),
                                          tz = "UTC") {

    convert_to_date_time.hms(x, class, orders, tz)

}

#' @rdname convert_to_date_time
#' @export
convert_to_date_time.Duration <- function(x, class,
                                          orders = c("HMS", "HM", "H"),
                                          tz = "UTC") {

    class <- stringr::str_to_lower(class)

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
        rlang::warn("There's no date to parse")
        lubridate::as_date(NA)
    } else if (class == "posixct") {
        output <- as.POSIXct(hms::as_hms(as.numeric(x)))
        lubridate::year(output) <- 0
        lubridate::force_tz(as.POSIXct(output), tz = tz)
    } else if (class == "posixlt") {
        output <- as.POSIXlt(hms::as_hms(as.numeric(x)))
        lubridate::year(output) <- 0
        lubridate::force_tz(as.POSIXlt(output), tz = tz)
    } else {
        rlang::abort("Critical error")
    }

}

#' @rdname convert_to_date_time
#' @export
convert_to_date_time.hms <- function(x, class, orders = c("HMS", "HM", "H"),
                                     tz = "UTC") {

    class <- stringr::str_to_lower(class)

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
        rlang::warn("There's no date to parse")
        lubridate::as_date(NA)
    } else if (class == "posixct") {
        output <- as.POSIXct(hms::as_hms(x))
        lubridate::year(output) <- 0
        lubridate::force_tz(as.POSIXct(output), tz = tz)
    } else if (class == "posixlt") {
        output <- as.POSIXlt(hms::as_hms(x))
        lubridate::year(output) <- 0
        lubridate::force_tz(as.POSIXlt(output), tz = tz)
    } else {
        rlang::abort("Critical error")
    }

}

#' @rdname convert_to_date_time
#' @export
convert_to_date_time.Period <- function(x, class, orders = c("HMS", "HM", "H"),
                                        tz = "UTC") {

    convert_to_date_time.Duration(x, class, orders, tz)

}

#' @rdname convert_to_date_time
#' @export
convert_to_date_time.Date <- function(x, class,
                                      orders = c("HMS", "HM", "H"),
                                      tz = "UTC") {

    class <- stringr::str_to_lower(class)

    if (class == "character") {
        as.character(x)
    } else if (class == "numeric") {
        as.numeric(x)
    } else if (class == "difftime") {
        rlang::warn("There's no time to parse")
        as.numeric(NA)
    } else if (class == "duration") {
        rlang::warn("There's no time to parse")
        lubridate::as.duration(NA)
    } else if (class == "hms") {
        rlang::warn("There's no time to parse")
        hms::as_hms(NA)
    } else if (class == "period") {
        rlang::warn("There's no time to parse")
        lubridate::as.period(NA)
    } else if (class == "date") {
        lubridate::force_tz(x, tz)
    } else if (class == "posixct") {
        output <- as.POSIXct(lubridate::force_tz(x, tz))
        lubridate::force_tz(as.POSIXct(output), tz = tz)
    } else if (class == "posixlt") {
        output <- as.POSIXlt(lubridate::force_tz(x, tz))
        lubridate::force_tz(as.POSIXlt(output), tz = tz)
    } else {
        rlang::abort("Critical error")
    }

}

#' @rdname convert_to_date_time
#' @export
convert_to_date_time.POSIXt <- function(x, class, orders = c("HMS", "HM", "H"),
                              tz = "UTC") {

    class <- stringr::str_to_lower(class)

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

#' @rdname convert_to_date_time
#' @export
convert_to_date_time.Interval <- function(x, class,
                                          orders = c("HMS", "HM", "H"),
                                          tz = "UTC") {

    class <- stringr::str_to_lower(class)

    if (class == "character") {
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
        output <- as.POSIXct(hms::hms(x))
        lubridate::year(output) <- 0
        lubridate::force_tz(as.POSIXct(output), tz = tz)
    } else if (class == "posixlt") {
        output <- as.POSIXlt(hms::hms(x))
        lubridate::year(output) <- 0
        lubridate::force_tz(as.POSIXlt(output), tz = tz)
    } else {
        rlang::abort("Critical error")
    }

}

#' Convert date/time or radians to decimal time
#'
#' @description
#'
#' `convert_to_decimal` converts date/time objects __or__ `numeric` objects with
#' radian values to decimal time.
#'
#' @details
#'
#' ## `unit` valid values
#'
#' `convert_to_decimal` can do the following transformations. Use the value of
#' your choose in the `unit` argument.
#'
#' * `"H"`: for decimal hours.
#' * `"M"`: for decimal minutes.
#' * `"S"`: for decimal seconds.
#' * `"d"`: for decimal days.
#' * `"W"`: for decimal weeks.
#' * `"m"`: for decimal months.
#' * `"y"`: for decimal years.
#' * `"date_decimal"`: for decimal dates. See topic "Decimal dates" to know
#' more.
#' * `"custom_unit"`: for custom units. See "Custom unit" topic to know more.
#'
#' This list of formats correspond to the list used on
#' [lubridate::parse_date_time()].
#'
#' ## Year and month lengths
#'
#' The length of months and years can vary. For example, March have 31 days,
#' while April have have 30. Due to leap years, the same can be said to year
#' lengths.
#'
#' To address this problem, `convert_to_decimal` use as default the mean of
#' possible values for months and years, used to calculate month and year
#' durations on the lubridate package (see: [lubridate::dmonths()] and
#' [lubridate::dyears()]). However, if you like, you can reset this values
#' assigning other values to the arguments `month_length` and `year_length`.
#'
#' `month_length` and `year_length` values must be assigned with the number of
#' seconds equivalent to the unit duration.
#'
#' __Tip__: Use [lubridate::duration()] to help you assign values (_e.g._
#' `lubridate::ddays(30)`).
#'
#' __Example__: If you want to use a month with 30 days, set `month_length` to
#' `lubridate::ddays(30)`.
#'
#' ## Decimal dates
#'
#' `convert_to_decimal` can be used as a wrapper to [lubridate::decimal_date()]
#' by creating decimal dates that can be used on [lubridate::date_decimal()].
#'
#' Decimal dates are a kind of decimal time where the year corresponds to
#' the integer part of the value. The months, days, hours, minutes, and seconds
#' elements are picked so the date-time will accurately represent the fraction
#' of the year expressed by decimal.
#'
#' Beware that decimal dates are not very precise! It cannot be used as a way
#' to store time data.
#'
#' __Example__:
#'
#' ```
#' x <- lubridate::ymd("2009-02-10")
#' decimal <- lubridate::decimal_date(x) # 2009.11
#' lubridate::date_decimal(decimal)
#' #> [1] "2009-02-10 UTC"
#' ```
#'
#' ## Custom unit
#'
#' `convert_to_decimal` can convert time to decimal using any numeric custom
#' unit.
#'
#' When using a custom unit, the argument `custom_unit` must be assigned with
#' the number of seconds equivalent to the unit duration. You can also assign a
#' [lubridate duration object][lubridate::duration()] to `custom_unit` if you
#' like.
#'
#' __Tip__: Use [lubridate::duration()] to help you assign values (_e.g._
#' `lubridate::dhours(1)`).
#'
#' __Example__: if you want to convert a duration of 3 hours to decimal of 1.5
#' hours, do:
#'
#' ```
#' x <- lubridate::dhours(3)
#' convert_to_decimal(x, unit = "custom_unit",
#'                    custom_unit = lubridate::dhours(1.5))
#' #> [1] 2
#' ```
#'
#' ## Radian transformation
#'
#' `convert_to_decimal` assumes that any numeric value assigned on `x`
#' represents a radian value. Just assign it on `x` and set the unit of your
#' choose.
#'
#' __Example__: if you want to convert 1 radian to decimal hours, do:
#'
#' ```
#' x <- 1
#' convert_to_decimal(1)
#' #> [1] 3.819719
#' ```
#'
#' ## Round-off errors
#'
#' This function is not optimized to deal with [round-off
#' errors](https://en.wikipedia.org/wiki/Round-off_error). If you need a very
#' precise transformation, `convert_to_decimal` may not attend you.
#'
#' ## Interval objects
#'
#' Class `interval` objects used on `x` are treated like `difftime` objects,
#' _i.e_ `convert_to_decimal` will convert the time difference of the interval
#' to a decimal value.
#'
#' @param x A date/time object __or__ a `numeric` vector with radian values.
#'   Only the following classes are allowed for date/time objects: `difftime`,
#'    `Duration`, `hms`, `Period`, `Date`, `POSIXct`, `POSIXlt`, and `Interval`.
#' @param unit A `character` string indicating the unit of transformation. Go
#'   to details section to see the valid values (default: `"H"`).
#' @param round A `logical` value indicating if the output must be rounded
#'   (default: `FALSE`).
#' @param digits An integer number indicating the number of decimal places or
#'   significant digits to be used when `round = TRUE` (see [base::round] to
#'   know more) (only required when `round = TRUE`) (default: `3`).
#' @param custom_unit A `duration` object or a non negative numeric value with
#'   the number of seconds equivalent to the custom unit (optional) (see topic
#'   "Custom unit" in the details section to know more).
#'   .
#' @param month_length A `duration` object or a non negative numeric value with
#'   the number of seconds equivalent to the month length (optional) (see topic
#'   "Year and month lengths" in the details section to know more) (default:
#'   `lubridate::dmonths()`, which is equivalent to 30.4375 days).
#' @param year_length A `duration` object or a non negative numeric value with
#'   the number of seconds equivalent to the year length (optional) (see topic
#'   "Year and month lengths" in the details section to know more)(default:
#'   `lubridate::dyears()`, which is equivalent to 365.25 days).
#'
#' @return A numeric vector with decimal times.
#' @family Convert to date/time functions
#' @export
#'
#' @examples
#' ## ** conversion of date/time objects to decimal time **
#' x <- hms::as_hms("01:00:00")
#' convert_to_decimal(x, "S")
#' #> [1] 3600
#' convert_to_decimal(x, "M")
#' #> [1] 60
#' convert_to_decimal(x)
#' #> [1] 1
#' convert_to_decimal(x, "d")
#' #> [1] 0.04166667
#' convert_to_decimal(x, "d", round = TRUE, digits = 3)
#' #> [1] 0.042
#' convert_to_decimal(x, "W", round = TRUE, digits = 3)
#' #> [1] 0.006
#' convert_to_decimal(x, "W")
#' #> [1] 0.005952381
#' convert_to_decimal(x, "m")
#' #> [1] 0.001368925
#' convert_to_decimal(x, "m", month_length = lubridate::ddays(30.4375))
#' #> [1] 0.001368925
#' convert_to_decimal(x, "m", month_length = lubridate::ddays(30))
#' #> [1] 0.001388889
#' convert_to_decimal(x, "y")
#' #> [1] 0.0001140771
#' convert_to_decimal(x, "y", year_length = lubridate::ddays(365.25))
#' #> [1] 0.0001140771
#' convert_to_decimal(x, "y", year_length = lubridate::ddays(365))
#' #> [1] 0.0001141553
#' convert_to_decimal(x, "custom_unit", custom_unit = lubridate::dhours())
#' #> [1] 1
#' convert_to_decimal(x, "custom_unit", custom_unit = 3600)
#' #> [1] 1
#' convert_to_decimal(x, "custom_unit", custom_unit = 3600 * 2)
#' #> [1] 0.5
#'
#' ## ** conversion of date/time objects to decimal dates **
#' x <- lubridate::ymd("2020-02-01")
#' convert_to_decimal(x, "date_decimal")
#' #> [1] 2020.085
#' lubridate::date_decimal(convert_to_decimal(x, "date_decimal"))
#' #> [1] "2020-02-01 00:00:17 UTC" # Note the imprecision
#'
#' ## ** conversion of radian values to decimal time **
#' x <- convert_to_rad(hms::as_hms("23:30:30"))
#' convert_to_rad(hms::as_hms("23:30:30"))
#' #> [1] 6.154467
#' convert_to_decimal(x, "H")
#' #> [1] 23.5083
#' convert_to_date_time(lubridate::dhours(convert_to_decimal(x, "H")), "hms")
#' #> 23:30:30
convert_to_decimal <- function(x, unit = "H", round = FALSE, digits = 3,
                               custom_unit = NULL,
                               month_length = lubridate::dmonths(),
                               year_length = lubridate:: dyears()) {

    # Check arguments --------------------

    check <- function(x) {
        if (is_time(x)) as.numeric(convert_to_date_time(x, "duration")) else x
    }

    assert_custom_3(x)
    checkmate::assert_logical(round, any.missing = FALSE, len = 1)
    checkmate::assert_int(digits, null.ok = TRUE)

    custom_unit <- check(custom_unit)
    month_length <- check(month_length)
    year_length <- check(year_length)

    checkmate::assert_numeric(month_length, lower = 0, len = 1)
    checkmate::assert_numeric(year_length, lower = 0, len = 1)
    checkmate::assert_numeric(custom_unit, lower = 0, len = 1, null.ok = TRUE)

    checkmate::assert_choice(unit, c("H", "M", "S", "d", "W", "m", "y",
                                     "date_decimal", "custom_unit"))

    if (isTRUE(round) && is.null(digits)) {
        rlang::abort(glue::glue("When `round = TRUE`, the `digits` variable",
                                "cannot be `NULL`"))
    }

    if (unit == "custom_unit" && is.null(custom_unit)) {
        rlang::abort(glue::glue("When `unit` is equal to 'custom_unit', the ",
                                "`custom_unit` variable cannot be `NULL`"))
    }

    # Compute output --------------------

    if (any("numeric" %in% class(x)) && !is_time(x)) {
        x <- lubridate::duration(x / ((2 * pi) / 24 / 60 / 60))
    }

    if (!(unit == "date_decimal") && lubridate::is.Date(x)) {
        return(as.numeric(NA))
    } else  if (unit == "date_decimal") {
        if (lubridate::is.Date(x) || lubridate::is.POSIXt(x)) {
            output <- lubridate::decimal_date(x)
        } else {
            return(as.numeric(NA))
        }
    } else if (is_time(x) && !lubridate::is.Date(x)) {
        if (lubridate::is.POSIXt(x)) {
            x <- convert_to_date_time(x, "hms")
        } else {
            x <- as.numeric(x)
        }

        if (unit == "S") {
            output <- x
        } else if (unit == "M") {
            output <- x / as.numeric(lubridate::dminutes())
        } else if (unit == "H") {
            output <- x / as.numeric(lubridate::dhours())
        } else if (unit == "d") {
            output <- x / as.numeric(lubridate::ddays())
        } else if (unit == "W") {
            output <- x / as.numeric(lubridate::dweeks())
        } else if (unit == "m") {
            output <- x / month_length
        } else if (unit == "y") {
            output <- x / year_length
        } else if (unit == "custom_unit") {
            output <- x / as.numeric(custom_unit)
        } else {
            rlang::abort("Critical error")
        }
    } else {
        rlang::abort("Critical error")
    }

    if (isTRUE(round)) round(output, digits) else output

}

#' Convert date/time or decimal hours to radians
#'
#' @description
#'
#' `convert_to_rad` converts date/time objects __or__ `numeric` objects with
#' decimal hours to radians.
#'
#' @details
#'
#' ## Converting other units of decimal time
#'
#' `convert_rad` assumes that any numeric value on `x` represents decimal
#' __hours__. Check the instructions on [mctq::convert_to_date_time] details
#' section to learn how to convert other units of decimal time in radians.
#'
#'
#' ## Round-off errors
#'
#' This function is not optimized to deal with [round-off
#' errors](https://en.wikipedia.org/wiki/Round-off_error). If you need a very
#' precise transformation, `convert_to_rad` may not attend you.
#'
#' @param x A date/time object __or__ a `numeric` vector with decimal hours. Only
#'   the following classes are allowed for date/time objects: `difftime`,
#'   `Duration`, `hms`, `Period`, `POSIXct`, `POSIXlt`, and `Interval`.
#' @inheritParams convert_to_decimal
#'
#' @return A numeric vector with radians.
#' @family Convert to date/time functions
#' @export
#' @examples
#' ## ** conversion of date/time objects to radian values **
#' x <- lubridate::ymd_hms("2020-01-01 05:25:00")
#' convert_to_rad(x)
#' #> [1] 1.41808
#' convert_to_rad(x, round = TRUE, digits = 4)
#' #> [1] 1.4181
#'
#' x <- c(lubridate::dhours(1), lubridate::dhours(23))
#' convert_to_rad(x)
#' #> [1] 0.2617994 6.0213859
#'
#' x <- hms::as_hms("03:45:23")
#' convert_to_rad(x)
#' #> [1] 0.9834203
#'
#' ## ** conversion of decimal hours to radian value **
#' x <- 1
#' convert_to_rad(x)
#' #> [1] 0.2617994
#' convert_to_rad(x, round = TRUE, digits = 2)
#' #> [1] 0.26
#'
#' x <- c(24, 1)
#' convert_to_rad(x)
#' #> [1] 6.2831853 0.2617994
convert_to_rad <- function(x, round = FALSE, digits = 3) {

    # Check arguments --------------------

    assert_custom_2(x)
    checkmate::assert_logical(round, any.missing = FALSE, len = 1)
    checkmate::assert_int(digits, null.ok = TRUE)

    if (isTRUE(round) && is.null(digits)) {
        rlang::abort(glue::glue("When `round = TRUE`, the `digits` variable ",
                                "cannot be `NULL`"))
    }

    # Transform values --------------------

    if (lubridate::is.POSIXt(x)) {
        x <- as.numeric(convert_to_date_time(x, "hms"))
    } else if (is.numeric(x) & !is_time(x)) {
        # if(x >= 24) {
        #     x <- x - (24 * floor(x / 24))
        # }

        x <- x * as.numeric(lubridate::dhours())
    } else {
        x <- as.numeric(x)
    }

    # Compute output --------------------

    # rad_second <- (2 * pi) / 24 / 60 / 60
    rad_second <- pi / 12 / 60 / 60

    if (isTRUE(round)) {
        round(x * rad_second, digits)
    } else {
        x * rad_second
    }

}
