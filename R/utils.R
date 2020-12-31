#' Get path to data example
#'
#' @description
#'
#' `mctq` comes bundled with a raw fictional dataset in its `inst/extdata`
#' directory. This function make it easy to access their paths.
#'
#' @param path A character string with the file name. If `NULL`, the example
#'   file will be listed (default: `NULL`).
#'
#' @return If path is equal to `NULL`, returns a character vector with all
#'   the example file names. Else, returns the full path where the file is
#'   located.
#'
#' @family Utility functions
#' @export
#'
#' @examples
#' data_example()
#' \dontrun{data_example("mctq_std.txt")}
data_example <- function(path = NULL) {

    checkmate::assert_string(path, null.ok = TRUE)

    if (is.null(path)) {
        dir(system.file("extdata", package = "mctq"))
    } else {
        system.file("extdata", path, package = "mctq", mustWork = TRUE)
    }

}

#' Return a model data for the MCTQ
#'
#' @description
#'
#' The `mctq` package comes bundled with fictional datasets for different
#' versions of the Munich Chronotype Questionnaire (mctq core, mctq shift, and
#' \strong{\eqn{\mu}}mctq). `model_data` make it easy to access them.
#'
#' At the moment, __only the standard MCTQ is available__.
#'
#' @param model A `character` string indicating the data model to return. Valid
#'   values are: `"standard"`, "`shift"`, `"micro"`,  (default: `"standard"`).
#'
#' @return A tibble with the model data.
#' @family Utility functions
#' @export
#'
#' @examples
#' \dontrun{model_data()}
model_data <- function(model = "standard") {

    model <- stringr::str_to_lower(model)
    checkmate::assert_choice(model, c("std", "standard", "shift", "micro"))

    if (model == "std" || model == "standard") {
        mctq::mctq_std
    } else if (model == "shift") {
        NA # mctq::mctq_shift
    } else if (model == "micro") {
        NA # mctq::micro_mctq
    } else {
        rlang::abort("Critical error")
    }

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
#' #> [1] "0000-01-01 07:45:32"
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
#' #> [1] "0000-01-01 20:02:01"
#'
#' x <- lubridate::ymd_hms("1987-12-24 07:45:32") # hour < 12h
#' midday_change(x)
#' #> [1] "0000-01-02 10:25:00"
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
#' #> [1] TRUE
#' is_time(as.Date("2020-01-01"))
#' #> [1] TRUE
#' is_time(as.Date("2020-01-01"), rm_date = TRUE)
#' #> [1] FALSE
#' is_time(iris)
#' #> [1] FALSE
#' is_time(letters)
#' #> [1] FALSE
#' }
is_time <- function(x, rm_date = FALSE) {

    checkmate::check_logical(rm_date, any.missing = FALSE, len = 1)

    check <- stringr::str_to_lower(
        c("difftime", "Duration", "hms", "Period", "Date", "POSIXct",
          "POSIXlt", "Interval"))

    if (isTRUE(rm_date)) {
        check <- stringr::str_subset(check, "^(?!date)")
    }

    any(stringr::str_to_lower(class(x)) %in% check) &&
            !any("numeric" %in% class(x))

}

#' @noRd
class_collapse <- function(x) {

    glue::single_quote(glue::glue_collapse(class(x), sep = '/'))

}
