#' Return a model MCTQ data
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `mctq` package comes bundled with fictional datasets for different versions
#' of the Munich Chronotype Questionnaire (mctq standard, mctq shift, and
#' \strong{\eqn{\mu}}mctq). `model_data()` make it easy to access them.
#'
#' At the moment, __only the standard MCTQ is available__.
#'
#' @param model A string indicating the data model to return. Valid values are:
#'   `"standard"`, "`shift"`, `"micro"`,  (default: `"standard"`).
#'
#' @return An invisible tibble with a MCTQ model data.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' \dontrun{
#' data <- model_data()
#' }
model_data <- function(model = "standard") {
    model <- stringr::str_to_lower(model)
    checkmate::assert_choice(model, c("std", "standard", "shift", "micro"))

    if (model %in% c("std", "standard")) {
        invisible(mctq::std_mctq)
    } else if (model == "shift") {
        NA # invisible(mctq::mctq_shift)
    } else if (model == "micro") {
        NA # invisible(mctq::micro_mctq)
    } else {
        rlang::abort("Critical error")
    }
}
