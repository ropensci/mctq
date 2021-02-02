#' Get paths to `mctq` raw datasets
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `mctq` comes bundled with raw fictional datasets for testing and learning
#' purposes. `raw_data()` make it easy to access their paths.
#'
#' @param file A string indicating the file name of the raw dataset. If `NULL`,
#'   all raw dataset file names will be listed (default: `NULL`).
#'
#' @return If `path = NULL`, returns a character vector with all raw dataset
#'   file names available. Else, returns the `file` path.
#'
#' @family utility functions
#' @export
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
