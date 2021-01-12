#' Assign dates to two sequential hour values
#'
#' @description
#'
#' `assign_date()` assign dates to two sequential hour values. It can facilitate
#' time arithmetic by locating time values without date reference on a
#' timeline.
#'
#' @details
#'
#' ## `ambiguity` argument
#'
#' In cases when `start` is equal to `end`, there are two possibilities of
#' intervals between the two hours, which results in a ambiguity. That's
#' because `start` and `end` can be at the same point in time or they can
#' distance themselves by one day, as illustrated below.
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
#' `assign_date()` can return three different outputs:
#'
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
#' If the number of characters (nchar) of `start_name` or `end_name` are equal
#' or greater than 30, `assign_date()` will name the list elements as `start`
#' and `end`.
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
#' @param ambiguity (optional) A numeric value to instruct `assign_date()` on
#'   how to deal with ambiguities (see Details) (default: `0`).
#' @param return (optional) A string indicating the type of output (see Details)
#'   (default: `"list"`).
#' @param start_name,end_name (optional) a string indicating a name associated
#'   with the `start` and `end` argument.
#'
#' @return
#'
#' * If `return = "list"`, a named list with `start` and `end` outputs.
#' * If `return = "start`, only the `start` output.
#' * If `return = "end"`, only the `end` output.
#'
#' @family time arithmetic functions
#' @export
#'
#' @examples
#' ## ** To return `start` and `end` outputs **
#' start <- hms::parse_hms("22:15:00")
#' end <- hms::parse_hms("00:00:00")
#' assign_date(start, end)
#' #> $start # Expected
#' #> [1] "0000-01-01 22:15:00 UTC"
#' #>
#' #> $end
#' #> [1] "0000-01-02 UTC"
#'
#' ## ** To return only the `start` output **
#' start <- lubridate::parse_date_time("01:10:00", "HMS")
#' end <- lubridate::parse_date_time("11:45:00", "HMS")
#' assign_date(start, end, return = "start")
#' #> [1] "0000-01-01 01:10:00 UTC" # Expected
#'
#' ## ** To assign a 24h interval to ambiguities **
#' start <- lubridate::as_datetime("1985-01-15 12:00:00")
#' end <- lubridate::as_datetime("2020-09-10 12:00:00")
#' assign_date(start, end, ambiguity = 24)
#' #> $start # Expected
#' #> [1] "0000-01-01 12:00:00 UTC"
#' #>
#' #> $end
#' #> [1] "0000-01-02 12:00:00 UTC"
assign_date <- function(start, end, ambiguity = 0, return = "list",
                        start_name = deparse(substitute(start)),
                        end_name = deparse(substitute(end))) {

    # Check arguments -----

    checkmate::check_multi_class(start, c("hms", "POSIXct", "POSIXlt"))
    checkmate::check_multi_class(end, c("hms", "POSIXct", "POSIXlt"))
    check_identical(start, end, "length")
    checkmate::assert_numeric(lubridate::hours(start), lower = 0, max.len = 23)
    checkmate::assert_numeric(lubridate::hours(end), lower = 0, max.len = 23)
    checkmate::assert_choice(ambiguity, c(0, 24 , NA))
    checkmate::assert_choice(return, c("list", "start", "end"))
    checkmate::assert_string(start_name)
    checkmate::assert_string(end_name)

    # Set values -----

    start_name <- start_name[1]
    end_name <- end_name[1]

    # Convert `start` and `end` -----

    start <- flat_posixt(convert_to(start, "posixct"))
    end <- flat_posixt(convert_to(end, "posixct"))

    # Create intervals -----

    out <- dplyr::case_when(
        any(is.na(c(start, end))) ~ lubridate::as.interval(NA),
        start < end ~ lubridate::interval(start, end),
        start > end ~ lubridate::interval(start, end + lubridate::days()),
        TRUE ~ lubridate::as.interval(lubridate::hours(ambiguity), start)
    )

    # Return output -----

    out <- list(start = lubridate::int_start(out),
                end = lubridate::int_end(out))

    if (nchar(start_name) < 30 && nchar(end_name) < 30) {
        names(out) <- c(start_name, end_name)
    }

    if (return == "start") {
        return(out[[1]])
    } else if (return == "end") {
        return(out[[2]])
    } else {
        out
    }

}
