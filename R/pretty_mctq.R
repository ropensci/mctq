#' Make an MCTQ dataset more presentable
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `pretty_mctq()` helps you to transform your Munich Chronotype Questionnaire
#' (MCTQ) data in many ways. See the Arguments and Details section to learn
#' more.
#'
#' @details
#'
#' ## Rounding
#'
#' Please note that by rounding MCTQ values you discard data. That is to say
#' that if you need to redo a computation, or do new ones, your values can be
#' off by a couple of seconds (see
#' [round-off error](https://en.wikipedia.org/wiki/Round-off_error)).
#'
#' Round your values only if and when you want to present them more clearly,
#' like in graphical representations. You can also round values to facilitate
#' data exporting to text formats (like `.csv`), but note that this will come
#' with a precision cost.
#'
#' Note also that `pretty_mctq()` uses [mctq::round_time()] for rounding.
#' `round_time()` is based on [base::round()], which uses the IEC 60559
#' standard. For more information see `?round_time`.
#'
#' @param data A `data.frame` object.
#' @param round (optional) a `logical` value indicating if `Duration`, `Period`,
#'   and `hms` objects must be rounded at the seconds level (default: `TRUE`).
#' @param hms (optional) a `logical` value indicating if `Duration`, `Period`,
#'   and `difftime` objects must be converted to `hms` (default: `TRUE`).
#'
#' @return A transformed `data.frame` object, as indicated in the arguments.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' data <- data.frame(
#'     a = 1,
#'     b = lubridate::duration(1.12345),
#'     c = lubridate::period(1.12345),
#'     d = hms::hms(1.12345))
#'
#' ## __ Rounding time objects from `data` __
#' pretty_mctq(data, round = TRUE, hms = FALSE)
#'
#' ## __ Converting non-`hms` time objects from `data` to `hms` __
#' pretty_mctq(data, round = FALSE, hms = TRUE)
pretty_mctq <- function(data, round = TRUE, hms = TRUE) {
    checkmate::assert_data_frame(data)
    checkmate::assert_flag(round)
    checkmate::assert_flag(hms)

    where <- NULL # R CMD Check variable bindings fix

    if (isTRUE(round)) {
        check <- function(x) {
            classes <- c("Duration", "Period", "hms")
            checkmate::test_multi_class(x, classes)
        }

        data <- dplyr::mutate(data, dplyr::across(where(check), round_time))
    }

    if (isTRUE(hms)) {
        check <- function(x) {
            classes <- c("Duration", "Period", "difftime")
            checkmate::test_multi_class(x, classes)
        }

        data <- convert(data, "hms", where = check)
    }

    data
}
