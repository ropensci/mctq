#' Return a model data for the MCTQ
#'
#' @description
#'
#' `mctq` package comes bundled with fictional datasets for different versions
#' of the Munich Chronotype Questionnaire (mctq standard, mctq shift, and
#' \strong{\eqn{\mu}}mctq). `model_data()` make it easy to access them.
#'
#' At the moment, __only the standard MCTQ is available__.
#'
#' @param model A `character` string indicating the data model to return. Valid
#'   values are: `"standard"`, "`shift"`, `"micro"`,  (default: `"standard"`).
#'
#' @return An invisible tibble with a MCTQ model data.
#' @family Utility functions
#' @export
#'
#' @examples
#' \dontrun{model_data()}
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

#' Get paths to MCTQ raw datasets
#'
#' @description
#'
#' `mctq` comes bundled with raw fictional datasets for testing and learning.
#' `raw_data()` make it easy to access their paths.
#'
#' @param file A character string with the raw dataset file name. If `NULL`,
#'   all raw dataset file names will be listed (default: `NULL`).
#'
#' @return If `path = NULL`, returns a character vector with all raw dataset
#'   file names available. Else, returns `file` path.
#'
#' @family Utility functions
#' @noRd
#'
#' @examples
#' \dontrun{
#' raw_data()
#' raw_data(raw_data()[1])
#' raw_data("std_mctq.csv")
#' }
raw_data <- function(file = NULL) {

    checkmate::assert_string(file, null.ok = TRUE)

    if (is.null(file)) {
        dir(system.file("extdata", package = "mctq"))
    } else {
        system.file("extdata", file, package = "mctq", mustWork = TRUE)
    }

}
