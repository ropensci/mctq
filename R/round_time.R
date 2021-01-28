#' Round time values
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `round_time()` takes a `Duration`, `Period`, `difftime`, or `hms` object and
#' round it at the seconds level.
#'
#' Note that `round_time()` convert time to seconds and them uses
#' [base::round()] to round it. That is to say that `round_time()` uses the same
#' IEC 60559 standard (_"go to the even digit"_) for rounding off a 5. Therefore
#' round(0.5) is 0 and round(-1.5) is -2. See `?round` to learn more.
#'
#' @param x An object belonging to one of the following classes: `Duration`,
#'   `Period`, `difftime`, `hms`.
#'
#' @return An time object with the time rounded at the seconds level.
#'
#' @family utility functions
#' @seealso [hms::round_hms()] [hms::trunc_hms()] [lubridate::round_date()].
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

    classes <- c("Duration", "Period", "difftime", "hms")
    checkmate::assert_multi_class(x, classes)

    class <- class(x)[1]
    x <- round(as.numeric(x))
    convert_to(x, class)

}
