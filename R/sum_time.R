#' Sum time values
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `sum_time()` returns the sum of the time from different kinds of date/time
#' objects.
#'
#' This function supports vectorized operations and can also be set to roll the
#' sum in a 24 hours clock basis, helping with time arithmetic issues.
#'
#' @details
#'
#' ## `class` argument
#'
#' `sum_time()` is integrated with [mctq::convert()]. That way you can choose
#' what class of object you prefer as output.
#'
#' Valid `class` values are: `"character"`, `"integer"`, `"double"`,
#' `"numeric"`, `"Duration"`, `"Period"`, `"difftime"`, and `"hms"`
#' (case insensitive).
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
#' `Period` objects are a special time of object developed by the
#' [lubridate::lubridate-package] team that track changes in clock times
#' ignoring time irregularities. That is to say that 1 day as `Period` will
#' always represent 1 day in the timeline. `sum_time()` ignores that property of
#' `Period` objects, treating them as objects of class `Duration`.
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
#' @param class (optional) a string indicating the output class (default:
#'   `"hms"`).
#' @param clock (optional) a logical value indicating whether the sum should
#'   roll in a 24 hour clock basis (default: `FALSE`).
#' @param vectorize (optional) a logical value indicating if the function must
#'   operate in a vectorized fashion (default: `FALSE`).
#' @param na.rm (optional) a logical value indicating if the function must
#'   remove `NA` values while performing the sum (default: `FALSE`).
#'
#' @return
#'
#' * If `clock = TRUE` and `vectorize = FALSE` (default), an object of
#' the indicated class in `class` (default: `"hms"`) with the sum of the time
#' from objects in `...` rolled in a 24 hour clock basis.
#'
#' * If `clock = FALSE` (default) and `vectorize = FALSE` (default), an object
#' of the indicated class in `class` (default: `"hms"`) with the cumulative sum
#' of the time from objects in `...`.
#'
#' * If `clock = TRUE` and `vectorize = TRUE`, an object of the indicated class
#' in `class` (default: `"hms"`) with a vectorized sum of the time from objects
#' in `...` rolled in a 24 hour clock basis.
#'
#' * If `clock = FALSE` (default) and `vectorize = TRUE`, an object of the
#' indicated class in `class` (default: `"hms"`) with a vectorized and
#' cumulative sum of the time from objects in `...`.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' ## __ Cumulative non-vectorized sum __
#' x <- c(as.POSIXct("2020-01-01 15:00:00"), as.POSIXct("1999-05-04 17:30:00"))
#' y <- lubridate::as.interval(lubridate::dhours(7), as.Date("1970-05-08"))
#' sum_time(x, y, class = "duration")
#' #> [1] "142200s (~1.65 days)" # Expected
#'
#' ## __ Non-vectorized sum rolled over in a 24 hour clock basis __
#' x <- c(lubridate::hours(25), lubridate::dhours(5), lubridate::minutes(50))
#' sum_time(x, clock = TRUE)
#' #> 06:50:00 # Expected
#'
#' x <- c(hms::parse_hm("00:15"), hms::parse_hm("02:30"), hms::as_hms(NA))
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
#' #>       NA # Expected
#' sum_time(x, y, vectorize = TRUE, na.rm = TRUE)
#' #> 29:00:00 # Expected
#' #> 10:00:00 # Expected
#'
#' ## __ Vectorized sum rolled over in a 24 hour clock basis __
#' x <- c(lubridate::dhours(6), NA)
#' y <- c(hms::parse_hm("23:00"), hms::parse_hm("10:00"))
#' sum_time(x, y, clock = TRUE, vectorize = TRUE)
#' #> 05:00:00 # Expected
#' #>       NA # Expected
#' sum_time(x, y, clock = TRUE, vectorize = TRUE, na.rm = TRUE)
#' #> 05:00:00 # Expected
#' #> 10:00:00 # Expected
sum_time <- function(..., class = "hms", clock = FALSE, vectorize = FALSE,
                     na.rm = FALSE) {
    out <- list(...)

    assert_custom <- function(x) {
        classes <- c("Duration", "Period", "difftime", "hms", "POSIXct",
                     "POSIXlt", "Interval")

        checkmate::assert_multi_class(x, classes)
    }

    choices <- tolower(
        c("character", "integer", "double", "numeric", "Duration",
          "Period", "difftime", "hms"))

    checkmate::assert_choice(tolower(class), choices)
    checkmate::assert_flag(clock)
    checkmate::assert_flag(vectorize)
    checkmate::assert_flag(na.rm)
    lapply(out, assert_custom)

    if (isTRUE(vectorize) &&
        !(length(unique(vapply(out, length, integer(1)))) == 1)) {
        stop("When 'vetorize' is 'TRUE', all values in '...' must have ",
             "the same length.", call. = FALSE)
    }

    normalize <- function(x) {
        if (lubridate::is.POSIXt(x) || lubridate::is.difftime(x)) {
            as.numeric(hms::as_hms(x))
        } else {
            as.numeric(x)
        }
    }

    zero_nas <- function(x) dplyr::if_else(is.na(x), 0, x)

    out <- lapply(out, normalize)
    if(isTRUE(na.rm)) out <- lapply(out, zero_nas)

    if (isTRUE(vectorize)) {
        out <- Reduce("+", out)
    } else {
        out <- do.call("c", out)
        out <- sum(out, na.rm = na.rm)
    }

    if (isTRUE(clock)) out <- flat_posixt(lubridate::as_datetime(out))
    convert(out, class, quiet = TRUE)
}
