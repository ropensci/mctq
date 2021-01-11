#' Load a delimited file to R
#'
#' @description
#'
#' `load_data()` is a wrapper function for [readr::read_delim()] to help simple
#' data loading. You don't need to use this function if your file is already
#' loaded on R.
#'
#' If this function doesn't work for your file, we recommend using the
#' [readr::readr] package to load it. If you're using [RStudio
#' IDE](https://rstudio.com/), you can also go to `Import Dataset`, on the
#' environment tab, to load your data.
#'
#' @param file A character string indicating the file address to load. If
#'   unassigned, a dialog window will open allowing browsing.
#' @param delim A character string containing the field separator in `file`
#'   (default: `","`).
#' @param na A character vector indicating values that must be interpreted
#'   as `NA` (default: `c("", " ", "NA")`).
#' @param col_types A `NULL` value or a [readr::cols()] specification to set
#'   how `file` columns must be treated. Check [readr::read_delim()] to learn
#'   more (default: `readr::cols(.default = "c")`, which imports all columns
#'   as `character`).
#' @param trim_ws A logical value indicating if leading and/or trailing
#'   whitespaces must be trimmed from each field (default: `TRUE`).
#' @param skip An integerish value, greater than 0, indicating the number of
#'   rows to skip when reading `file` (default: `0`).
#' @param skip_empty_rows A logical value indicating if blank rows must be
#'   ignored altogether. If this option is `TRUE`, then blank rows will
#'   not be represented at all. If it is `FALSE` then they will be represented
#'   by `NA` values in all the columns (default: `TRUE`).
#'
#' @return An invisible tibble.
#' @family Utility functions
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{load_data(raw_data()[1])}
load_data <- function(file = file.choose(),
                      delim = ",",
                      na = c("", " ", "NA"),
                      col_types = readr::cols(.default = "c"),
                      trim_ws = TRUE,
                      skip = 0,
                      skip_empty_rows = TRUE) {

    checkmate::assert_string(file)
    checkmate::assert_file_exists(file)
    checkmate::assert_string(delim)
    checkmate::assert_character(na, any.missing = FALSE)
    checkmate::assert_class(col_types, "col_spec", null.ok = TRUE)
    checkmate::assert_flag(trim_ws)
    checkmate::assert_count(skip)
    checkmate::assert_flag(skip_empty_rows)

    invisible(
        file %>%
            readr::read_delim(delim = delim,
                              na = na,
                              col_types = col_types,
                              trim_ws = trim_ws,
                              skip = skip,
                              skip_empty_rows = skip_empty_rows) %>%
            dplyr::as_tibble())

}
