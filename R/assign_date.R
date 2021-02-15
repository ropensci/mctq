#' Assign dates to two sequential hours
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `assign_date()` assign dates to two sequential hours. It can facilitate
#' time arithmetic by locating time values without date reference on a
#' timeline.
#'
#' @details
#'
#' ## Class requirements
#'
#' The `mctq` package works with a set of object classes specially created to
#' hold time values. This classes can be found on [hms::hms-package] and
#' [lubridate::lubridate-package]. If your data do not conform to the object
#' classes required, you can use [mctq::convert()] to convert it
#' (see `vignette("converting-data", package = "mctq")`).
#'
#' ## `ambiguity` argument
#'
#' In cases when `start` is equal to `end`, there are two possibilities of
#' intervals between the two hours (ambiguity). That's because `start` and `end`
#' can be at the same point in time or they can distance themselves by one day,
#' as illustrated below.
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
#' hours, _i.e_ `start` and `end` are located at the same point in time
#' (default).
#' * `ambiguity = 24`: to consider the interval between `start` and `end` as 24
#' hours, _i.e._ `start` and `end` distance themselves by one day.
#' * `ambiguity = NA`: to disregard this cases, assigning `NA` as value.
#'
#' ## `return` argument
#'
#' `assign_date()` can return different outputs:
#'
#' * `return = "interval"`: returns a `start`---`end` `interval` object.
#' * `return = "list"`: returns a `list` object with two named elements
#' corresponding to `start` and `end` output.
#' * `return = "start"`: returns only the `start` output.
#' * `return = "end"`: returns only the `end` output.
#'
#' ## `start_name` and `end_name` arguments
#'
#' These arguments serve to instruct `assign_date()` on how to name the
#' list elements when `return == "list"`. As default, the function will name
#' this elements with the names of the variables assigned to `start` and `end`
#' arguments.
#'
#' If the number of characters (`nchar`) of `start_name` or `end_name` are equal
#' or greater than 30, `assign_date()` will name the list elements as `"start"`
#' and `"end"`.
#'
#' ## `POSIXt` objects
#'
#' `POSIXt` values passed as argument to `start` or `end` will be strip of their
#' dates. Only the hours will be considered.
#'
#' ## `NA` values
#'
#' `assign_date()` will return `NA` if `start` or `end` are `NA`.
#'
#' @param start,end A `hms` or `POSIXt` vector indicating the start or end
#'   hour.
#' @param ambiguity (optional) a `numeric` value to instruct `assign_date()` on
#'   how to deal with ambiguities (see Details) (default: `0`).
#' @param return (optional) a string indicating the type of the output (see
#'   Details) (default: `"interval"`).
#' @param start_name,end_name (optional) a string indicating a name associated
#'   with the `start` and `end` argument.
#'
#' @return
#'
#' * If `return = "interval"`, a `start`---`end` `interval` object.
#' * If `return = "list"`, a named list with `start` and `end` outputs.
#' * If `return = "start`, only the `start` output.
#' * If `return = "end"`, only the `end` output.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' ## __ Scalar example __
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
#' ## __ Vector example __
#' start <- c(hms::parse_hm("09:45"), hms::parse_hm("20:30"))
#' end <- c(hms::parse_hm("21:15"), hms::parse_hm("04:30"))
#' assign_date(start, end)
#' #> [1] 1970-01-01 09:45:00 UTC--1970-01-01 21:15:00 UTC # Expected
#' #> [2] 1970-01-01 20:30:00 UTC--1970-01-02 04:30:00 UTC # Expected
#'
#' ## __ To return `start` and `end` as interval (default)__
#' start <- hms::parse_hm("12:34")
#' end <- hms::parse_hm("01:25")
#' assign_date(start, end)
#' #> [1] 1970-01-01 12:34:00 UTC--1970-01-02 01:25:00 UTC # Expected
#'
#' ## __ To return `start` and `end` as list __
#' start <- hms::parse_hm("22:15")
#' end <- hms::parse_hm("00:01")
#' assign_date(start, end, return = "list")
#' #> $start # Expected
#' #> [1] "1970-01-01 22:15:00 UTC" # Expected
#' #> # Expected
#' #> $end # Expected
#' #> [1] "1970-01-02 00:01:00 UTC" # Expected
#'
#' ## __ To return only `start` or `end` __
#' start <- lubridate::parse_date_time("01:10:00", "HMS")
#' end <- lubridate::parse_date_time("11:45:00", "HMS")
#' assign_date(start, end, return = "start")
#' #> [1] "1970-01-01 01:10:00 UTC" # Expected
#' assign_date(start, end, return = "end")
#' #> [1] "1970-01-01 11:45:00 UTC" # Expected
#'
#' ## __ To assign a 24h interval to ambiguities __
#' start <- lubridate::as_datetime("1985-01-15 12:00:00")
#' end <- lubridate::as_datetime("2020-09-10 12:00:00")
#' assign_date(start, end, ambiguity = 24)
#' #> [1] 1970-01-01 12:00:00 UTC--1970-01-02 12:00:00 UTC # Expected
assign_date <- function(start, end, return = "interval", ambiguity = 0,
                        start_name = deparse(substitute(start)),
                        end_name = deparse(substitute(end))) {
    # Check arguments -----

    checkmate::assert_multi_class(start, c("hms", "POSIXct", "POSIXlt"))
    checkmate::assert_multi_class(end, c("hms", "POSIXct", "POSIXlt"))
    assert_identical(start, end, type = "length")
    checkmate::assert_numeric(as.numeric(hms::as_hms(start)),
                              lower = 0, upper = 86400)
    checkmate::assert_numeric(as.numeric(hms::as_hms(end)),
                              lower = 0, upper = 86400)
    checkmate::assert_choice(return, c("list", "interval", "start", "end"))
    checkmate::assert_choice(ambiguity, c(0, 24 , NA))
    checkmate::assert_string(start_name)
    checkmate::assert_string(end_name)

    # Set values -----

    start_name <- start_name[1]
    end_name <- end_name[1]

    # Convert `start` and `end` -----

    start <- flat_posixt(convert(start, "posixct", quiet = TRUE))
    end <- flat_posixt(convert(end, "posixct", quiet = TRUE))

    # Create intervals -----

    out <- dplyr::case_when(
        is.na(start) | is.na(end) ~ lubridate::as.interval(NA),
        start < end ~ lubridate::interval(start, end),
        start > end ~ lubridate::interval(start, end + lubridate::days()),
        is.na(ambiguity) ~ lubridate::as.interval(NA),
        TRUE ~ lubridate::as.interval(lubridate::hours(ambiguity), start)
    )

    # Return output -----

    if (return == "interval") {
        return(out)
    } else if (return == "start") {
        return(lubridate::int_start(out))
    } else if (return == "end") {
        return(lubridate::int_end(out))
    } else {
        out <- list(start = lubridate::int_start(out),
                    end = lubridate::int_end(out))

        if (nchar(start_name) < 30 && nchar(end_name) < 30) {
            names(out) <- c(start_name, end_name)
        }

        out
    }
}
