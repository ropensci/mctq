#' Sum time values
#'
#' @description
#'
#' `sum_time()` returns the sum of the time from different kinds of date/time
#' objects. The sum can also be set to roll on a 24 hours clock basis, helping
#' with time arithmetic issues.
#'
#' @details
#'
#' ## `class` argument
#'
#' `sum_time()` is integrated with [mctq::convert_to()]. That way you can choose
#' what class of object will prefer for output.
#'
#' ## `POSIXt` objects
#'
#' `POSIXt` values in `...` will be strip of their dates. Only the hours will be
#' considered.
#'
#' ## `Period` objects
#'
#' `Period` objects are a special time of object develop by the
#' [lubridate::lubridate] team that track changes in clock times ignoring
#' time irregularities. That is to say that 1 day as `Period` will always
#' represent 1 day in the time line.
#'
#' `sum_time` ignores that property of `Period` objects, treating them as
#' objects of class `Duration`.
#'
#' ## Time line irregularities
#'
#' This function does not take into account time line irregularities (_e.g._
#' leap years, DST, leap seconds). This may not be a issue for most people, but
#' it must be considered when doing time arithmetic.
#'
#' @param ... Vectors belonging to one or more of the following classes:
#'   `Duration`, `Period`, `difftime`, `hms`, `POSIXct`, `POSIXlt`, or
#'   `Interval`.
#' @param class (optional) A string indicating the output class (default:
#'   `"hms"`).
#' @param clock (optional) A logical value indicating whether the sum should
#'   roll over on a 24 hour clock basis (default: `TRUE`).
#'
#' @return If `clock = TRUE` (default), an object of the indicated class in
#'   `class` (default: `"hms"`) with the sum of the time from objects in `...`
#'   rolled over on a 24 hour clock basis. Else, the same as previous, but
#'   __not__ rolled over on a 24 hours clock basis (accumulative).
#'
#' @family time arithmetic functions
#' @export
#'
#' @examples
#' sum_time(hms::parse_hm("11:45"), lubridate::dhours(5))
#' #> 16:45:00 # Expected
#' sum_time(lubridate::hours(25), lubridate::dhours(5), lubridate::minutes(50))
#' #> 06:50:00 # Expected
#' sum_time(lubridate::days(), lubridate::dhours(8), clock = FALSE)
#' #> 32:00:00 # Expected
#' x <- c(as.POSIXct("2020-01-01 15:00:00"), as.POSIXct("1999-05-04 17:30:00"))
#' y <- lubridate::as.interval(lubridate::dhours(7), as.Date("1970-05-08"))
#' sum_time(x, y, clock = FALSE, class = "duration")
#' #> [1] "142200s (~1.65 days)" # Expected
sum_time <- function(..., class = "hms", clock = TRUE) {

    out <- list(...)

    check <- function(x) {
        classes <- c("Duration", "Period", "difftime", "hms", "POSIXct",
                     "POSIXlt", "Interval")

        checkmate::assert_multi_class(x, classes)
    }

    fix <- function(x) {

        x <- ifelse(sapply(x, lubridate::is.POSIXt),
                    as.numeric(hms::as_hms(x)), as.numeric(x))
        ifelse(is.na(x), 0, x)
    }

    lapply(out, check)

    out <- lapply(out, fix)
    out <- do.call("c", out)
    out <- sum(out)
    if (isTRUE(clock)) out <- lubridate::as_datetime(out)
    convert_to(out, class)

}
