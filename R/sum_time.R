#' Sum time values
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `sum_time()` returns the sum of the time from different kinds of date/time
#' objects.
#'
#' This function supports vectorized operations and can also be set to roll the
#' sum on a 24 hours clock basis, helping with time arithmetic issues.
#'
#' @details
#'
#' ## `class` argument
#'
#' `sum_time()` is integrated with [mctq::convert_to()]. That way you can choose
#' what class of object will prefer for output.
#'
#' ## `vectorize` argument
#'
#' If `vectorize = FALSE` (default), `sum_time` will combine and sum all values
#' in `...`. In other words, in this setting, `sum_time(c(x, y), z)` will have
#' the same output as `sum_time(x, y, z)`.
#'
#' However, if `vectorize = TRUE`, `sum_time()` will require that all objects in
#' `...` have the same length, and will perform a paired sum between elements.
#' In other words, in this setting, `sum_time(c(x, y), c(w, z))` will return a
#' vector like `c(sum_time(x, w,), sum_time(y, z))`.
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
#' ## `NA` values
#'
#' `sum_time()` only return `NA` values when `vectorize = TRUE`.
#'
#' @param ... Vectors belonging to one or more of the following classes:
#'   `Duration`, `Period`, `difftime`, `hms`, `POSIXct`, `POSIXlt`, or
#'   `Interval`.
#' @param class (optional) a string indicating the output class (default:
#'   `"hms"`).
#' @param clock (optional) a logical value indicating whether the sum should
#'   roll over on a 24 hour clock basis (default: `FALSE`).
#' @param vectorize (optional) a logical value indicating if the function must
#'   operate in a vectorized fashion (default: `FALSE`).
#' @param na.rm (optional) a logical value indicating if the function must
#'   remove `NA` values while performing the sum (default: `FALSE`).
#'
#' @return
#'
#' * If `clock = TRUE` (default) and `vectorize = FALSE` (default), an object of
#' the indicated class in `class` (default: `"hms"`) with the sum of the time
#' from objects in `...` rolled over on a 24 hour clock basis.
#'
#' * If `clock = FALSE` and `vectorize = FALSE` (default), an object of the
#' indicated class in `class` (default: `"hms"`) with the cumulative sum of the
#' time from objects in `...`.
#'
#' * If `clock = TRUE` (default) and `vectorize = TRUE`, an object of the
#' indicated class in `class` (default: `"hms"`) with a vectorized sum of the
#' time from objects in `...` rolled over on a 24 hour clock basis.
#'
#' * If `clock = FALSE` and `vectorize = TRUE`, an object of the indicated class
#' in `class` (default: `"hms"`) with a vectorized and cumulative sum of the
#' time from objects in `...`.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' ## __ Cumulative non-vectorized sum __
#' x <- c(as.POSIXct("2020-01-01 15:00:00"), as.POSIXct("1999-05-04 17:30:00"))
#' y <- lubridate::as.interval(lubridate::dhours(7), as.Date("1970-05-08"))
#' sum_time(x, y, class = "duration")
#' #> [1] "142200s (~1.65 days)" # Expected
#'
#' ## __ Non-vectorized sum rolled over on a 24 hour clock basis __
#' x <- c(lubridate::hours(25), lubridate::dhours(5), lubridate::minutes(50))
#' sum_time(x, clock = TRUE)
#' #> 06:50:00 # Expected
#'
#' x <- c(lubridate::minutes(15), hms::parse_hm("02:30"), hms::as_hms(NA))
#' sum_time(x, clock = TRUE)
#' #> NA # Expected
#' sum_time(x, clock = TRUE, na.rm = TRUE)
#' #> 02:45:00 # Expected
#'
#' ## __ Cumulative vectorized sum __
#' x <- c(lubridate::dhours(6), NA)
#' y <- c(hms::parse_hm("23:00"), hms::parse_hm("10:00"))
#' sum_time(x, y, vectorize = TRUE)
#' #> 29:00:00 # Expected
#' #> NA # Expected
#' sum_time(x, y, vectorize = TRUE, na.rm = TRUE)
#' #> 29:00:00 # Expected
#' #> 10:00:00 # Expected
#'
#' ## __ Vectorized sum rolled over on a 24 hour clock basis __
#' x <- c(lubridate::dhours(6), NA)
#' y <- c(hms::parse_hm("23:00"), hms::parse_hm("10:00"))
#' sum_time(x, y, clock = TRUE, vectorize = TRUE)
#' #> 05:00:00 # Expected
#' #> NA # Expected
#' sum_time(x, y, clock = TRUE, vectorize = TRUE, na.rm = TRUE)
#' #> 05:00:00 # Expected
#' #> 10:00:00 # Expected
sum_time <- function(..., class = "hms", clock = FALSE, vectorize = FALSE,
                     na.rm = FALSE) {

    # List `...` -----

    out <- list(...)

    # Check arguments -----

    check <- function(x) {
        classes <- c("Duration", "Period", "difftime", "hms", "POSIXct",
                     "POSIXlt", "Interval")

        checkmate::assert_multi_class(x, classes)
    }

    checkmate::assert_string(class)
    checkmate::assert_flag(clock)
    checkmate::assert_flag(vectorize)
    checkmate::assert_flag(na.rm)
    lapply(out, check)

    if (isTRUE(vectorize) && !(length(unique(sapply(out, length))) == 1)) {
        rlang::abort(paste0(
            "When 'vetorize' is 'TRUE', all values in '...' must have the ",
            "the same length.")
            )
    }

    # Normalize values -----

    normalize <- function(x) {
        ifelse(sapply(x, lubridate::is.POSIXt),
               as.numeric(hms::as_hms(x)), as.numeric(x))
    }

    zero_nas <- function(x) {
        ifelse(sapply(x, is.na), 0, as.numeric(x))
    }

    out <- lapply(out, normalize)

    if(isTRUE(na.rm)) out <- lapply(out, zero_nas)

    # Sum time -----

    if (isTRUE(vectorize)) {
        out <- Reduce("+", out)
    } else {
        out <- do.call("c", out)
        out <- sum(out, na.rm = na.rm)
    }

    # Roll time -----

    if (isTRUE(clock)) out <- flat_posixt(lubridate::as_datetime(out))

    # Return output -----

    convert_to(out, class)

}

#' Round time values
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `round_time()` takes a time object and round it to the nearest value at
#' the seconds level.
#'
#' @param x An object belonging to one of the following classes: `Duration`,
#'   `Period`, `difftime`, `hms`.
#'
#' @return An date/time object with the time rounded at the seconds level.
#'
#' @family utility functions
#' @seealso [hms::round_hms()] [lubridate::round_date()]
#' @export
#'
#' @examples
#' lubridate::dhours(1.45678696454)
#' #> [1] "5244.433072344s (~1.46 hours)" # Expected
#' round_time(lubridate::dhours(1.45678696454))
#' #> [1] "5244s (~1.46 hours)" # Expected
#'
#' lubridate::microseconds(2454876956)
#' #> [1] "2454.876956S" # Expected
#' hms::as_hms(as.numeric(lubridate::microseconds(2454876956)))
#' #> 00:40:54.876956
#' round_time(lubridate::microseconds(2454876956))
#' #> [1] "40M 55S" # Expected
#'
#' hms::as_hms(12345.6789)
#' #> 03:25:45.6789 # Expected
#' round_time(hms::as_hms(12345.6789))
#' #> 03:25:46 # Expected
round_time <- function(x) {

    # To do -----
    #
    # * Include parameters (_e.g._ `H`, `M`) (use `convert_to_units()`) for
    #   that.
    # * Add routines to allow `POSIXt` and `Interval` objects (use
    #   lubridate::round_date())
    # * See hms::round_hms and hms::trunc_hms as inspiration.

    # Check arguments -----

    classes <- c("Duration", "Period", "difftime", "hms")
    checkmate::assert_multi_class(x, classes)

    # Compute and return output -----

    class <- class(x)[1]
    x <- round(as.numeric(x))
    convert_to(x, class)

}
