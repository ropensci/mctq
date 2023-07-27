#' Make an MCTQ dataset more presentable
#'
#' @description
#'
#' `r lifecycle::badge("maturing")`
#'
#' `pretty_mctq()` helps you to transform your Munich ChronoType Questionnaire
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
#' Note also that `pretty_mctq()` uses [`round()`][base::round()] for rounding,
#' which uses uses the IEC 60559 standard (_"go to the even digit"_) for
#' rounding off a 5. Therefore, `round(0.5)` is equal to 0 and `round(-1.5)` is
#' equal to -2. See [`?round`][base::round()] to learn more.
#'
#' @param data A [`data.frame`][base::data.frame()] object.
#' @param round (optional) a [`logical`][base::logical()] value indicating if
#'   [`Duration`][lubridate::duration()] and [`hms`][hms::hms()] objects must be
#'   rounded at the seconds level (default: `TRUE`).
#' @param hms (optional) a [`logical`][base::logical()] value indicating if
#'   [`Duration`][lubridate::duration()] and [`difftime`][base::as.difftime()]
#'   objects must be converted to [`hms`][hms::hms()] (default: `TRUE`).
#'
#' @return A transformed [`data.frame`][base::data.frame()] object, as indicated
#'   in the arguments.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' data <- data.frame(
#'     a = 1,
#'     b = lubridate::duration(1.12345),
#'     c = hms::hms(1.12345)
#'     )
#'
#' ## Rounding time objects from `data`
#'
#' pretty_mctq(data, round = TRUE, hms = FALSE)
#'
#' ## Converting non-'hms' time objects from 'data' to 'hms'
#'
#' pretty_mctq(data, round = FALSE, hms = TRUE)
pretty_mctq <- function(data, round = TRUE, hms = TRUE) {
  checkmate::assert_data_frame(data)
  checkmate::assert_flag(round)
  checkmate::assert_flag(hms)

  # R CMD Check variable bindings fix (see: https://bit.ly/3z24hbU)
  # nolint start: object_usage_linter.
  where <- NULL
  # nolint end

  if (isTRUE(round)) {
    check <- function(x) {
      classes <- c("Duration", "hms")
      checkmate::test_multi_class(x, classes)
    }

    data <-
      data |>
      dplyr::mutate(dplyr::across(where(check), round_time))
  }

  if (isTRUE(hms)) {
    check <- function(x) {
      classes <- c("Duration", "difftime")
      checkmate::test_multi_class(x, classes)
    }

    data <-
      data |>
      dplyr::mutate(
        dplyr::across(where(check), ~ hms::hms(extract_seconds(.x)))
      )
  }

  data
}
