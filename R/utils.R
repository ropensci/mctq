#' Get paths to MCTQ raw datasets
#'
#' @description
#'
#' `mctq` comes bundled with raw fictional datasets for testing and learning.
#' This function make it easy to access their paths.
#'
#' @param file A character string with the raw dataset file name. If `NULL`,
#'   all raw dataset file names will be listed (default: `NULL`).
#'
#' @return If `path = NULL`, returns a character vector with all raw dataset
#'   file names available. Else, returns `file` path.
#'
#' @family Utility functions
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

#' Return a model data for the MCTQ
#'
#' @description
#'
#' `mctq` package comes bundled with fictional datasets for different versions
#' of the Munich Chronotype Questionnaire (mctq standard, mctq shift, and
#' \strong{\eqn{\mu}}mctq). `model_data` make it easy to access them.
#'
#' At the moment, __only the standard MCTQ is available__.
#'
#' @param model A `character` string indicating the data model to return. Valid
#'   values are: `"standard"`, "`shift"`, `"micro"`,  (default: `"standard"`).
#'
#' @return A tibble with a MCTQ model data.
#' @family Utility functions
#' @export
#'
#' @examples
#' \dontrun{model_data()}
model_data <- function(model = "standard") {

    model <- stringr::str_to_lower(model)
    checkmate::assert_choice(model, c("std", "standard", "shift", "micro"))

    if (model == "std" || model == "standard") {
        mctq::std_mctq
    } else if (model == "shift") {
        NA # mctq::mctq_shift
    } else if (model == "micro") {
        NA # mctq::micro_mctq
    } else {
        rlang::abort("Critical error")
    }

}

#' Load a delimited file to R
#'
#' @description
#'
#' `load_data` is a wrapper function for [readr::read_delim()] to help simple
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

#' Flat dates of `POSIXt` objects
#'
#' @description
#'
#' `flat_posixt` changes the dates of `POSIXt` objects to `0000-01-01`. This can
#' be use to standardizing a point origin to time values.
#'
#' @param x A `POSIXt` vector.
#'
#' @family Utility functions
#' @export
#'
#' @examples
#' \dontrun{
#' x <- lubridate::ymd_hms("1987-12-24 07:45:32")
#' flat_posixt(x)
#' #> [1] "0000-01-01 07:45:32" # Expected
#' }
flat_posixt = function(x) {

    assert_posixt(x, null.ok = FALSE)

    if (!(any(is.na(x)))) {
        x <- lubridate::force_tz(x, "UTC")
        lubridate::date(x) <- "0000-01-01"
    }

    x

}

#' Change dates by time of day
#'
#' @description
#'
#' `midday_change` changes the dates of `POSIXt` objects accordingly to the time
#' of day registered in the object values. The function do this by flatting the
#' date to `0000-01-01` and them adding a day if the hour is lower than 12.
#'
#' This can be use to help determine when a subject's sleep episode started and
#' ended, if all that you have is the time of sleep onset and offset. Note that
#' this method can only be used to this matter if the sleep duration have a too
#' high value.
#'
#' @param x A `POSIXt` vector.
#'
#' @family Utility functions
#' @export
#'
#' @examples
#' \dontrun{
#' x <- lubridate::ymd_hms("2021-01-15 20:02:01") # hour > 12h
#' midday_change(x)
#' #> [1] "0000-01-01 20:02:01" # Expected
#'
#' x <- lubridate::ymd_hms("1987-12-24 07:45:32") # hour < 12h
#' midday_change(x)
#' #> [1] "0000-01-02 10:25:00" # Expected
#' }
midday_change = function(x) {

    assert_posixt(x, null.ok = FALSE)

    x <- flat_posixt(x)

    for (i in seq_along(x)) {
        if (!(is.na(x[i]))) {
            if (lubridate::hour(x[i]) < 12) {
                lubridate::day(x[i]) <- 2
            }
        }
    }

    x

}

#' Check if a object is a date/time type
#'
#' @description
#'
#' `is_time` is a logical checker for objects of class `difftime`, `Duration`,
#' `hms`, `Period`, `Date`, `POSIXct`, `POSIXlt`, and `Interval`.
#'
#' @param x Any kind of object.
#' @param rm_date A logical value indicating if `Date` objects should be removed
#'   from the check (default: `FALSE`)
#'
#' @family Utility functions
#' @export
#'
#' @examples
#' \dontrun{
#' is_time(lubridate::dhours())
#' #> [1] TRUE # Expected
#' is_time(as.Date("2020-01-01"))
#' #> [1] TRUE # Expected
#' is_time(as.Date("2020-01-01"), rm_date = TRUE)
#' #> [1] FALSE # Expected
#' is_time(iris)
#' #> [1] FALSE # Expected
#' is_time(letters)
#' #> [1] FALSE # Expected
#' }
is_time <- function(x, rm_date = FALSE) {

    checkmate::assert_flag(rm_date)

    classes <-
        c("difftime", "Duration", "hms", "Period", "Date", "POSIXct",
          "POSIXlt", "Interval")

    if (isTRUE(rm_date)) {
        classes <- stringr::str_subset(classes, "^Date$", negate = TRUE)
    }

    checkmate::test_multi_class(x, classes)

}

#' @noRd
class_collapse <- function(x) {

    glue::single_quote(glue::glue_collapse(class(x), sep = '/'))

}
