#' Make a MCTQ dataset more presentable
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `pretty_mctq()` helps you to transform your MCTQ data in many ways. See
#' parameters and Details section to learn more.
#'
#' You can also see `pretty_mctq()`in action in
#' `vignette("data_wrangling", package = "mctq")`.
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
#' Round your values only if and when you want to present it in a more clear
#' way, like in graphical representations. You can also round values to
#' facilitate data exporting to text formats (like `.csv`), but note that this
#' will come with a precision cost.
#'
#' Note also that `pretty_mctq()` uses [mctq::round_time()] for rounding.
#' `round_time()` is based on [base::round()], which uses the IEC 60559
#' standard. For more information see `?round_time`.
#'
#' @param data A data frame.
#' @param round (optional) a `logical` value indicating if `Duration`, `Period`,
#'   `difftime`, and `hms` objects must be rounded at the seconds level
#'   (default: `TRUE`).
#' @param hms (optional) a `logical` value indicating if `Duration`, `Period`,
#'   and `difftime` objects must be converted to `hms` (default: `TRUE`).
#'
#' @return A transformed data frame, as indicated in parameters.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' \dontrun{
#' pretty_mctq(mctq:::analyze_std_mctq(round = FALSE, hms = FALSE))
#' }
pretty_mctq <- function(data, round = TRUE, hms = TRUE) {

    where <- NULL # R CMD Check variable bindings fix

    test <- function(x) {
        classes <- c("Duration", "Period", "difftime", "hms")

        checkmate::test_multi_class(x, classes)
    }

    if (isTRUE(round)) {
        data <- data %>% dplyr::mutate(dplyr::across(where(test), round_time))
    }

    if (isTRUE(hms)) {
        data <- convert_to(data, "hms", where = test)
    }

    data

}