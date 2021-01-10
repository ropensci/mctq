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

    if (model %in% c("std", "standard")) {
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
#' be use to standardizing a point of origin to time values.
#'
#' @param x A `POSIXt` vector.
#'
#' @return A vector of the same `POSIXt` class type as `x` with `0000-01-01` as
#'   date.
#'
#' @family Utility functions
#' @export
#'
#' @examples
#' x <- lubridate::ymd_hms("1987-12-24 07:45:32")
#' flat_posixt(x)
#' #> [1] "0000-01-01 07:45:32 UTC" # Expected
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
#' @return A vector of the same `POSIXt` class type as `x` with date
#'   `0000-01-01` or `0000-01-02`.
#'
#' @family Utility functions
#' @export
#'
#' @examples
#' x <- lubridate::ymd_hms("2021-01-15 20:02:01") # hour > 12h
#' midday_change(x)
#' #> [1] "0000-01-01 20:02:01 UTC" # Expected
#'
#' x <- lubridate::ymd_hms("1987-12-24 07:45:32") # hour < 12h
#' midday_change(x)
#' #> [1] "0000-01-02 07:45:32 UTC" # Expected
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

#' Assign dates to 2 sequential hours
#'
#' __UNDER DEVELOPMENT__
#'
#' `assign_date` is a simple utility function to assign dates to two sequential
#' hours. It can facilitate time arithmetic.
#'
#' @details
#'
#' `assign_date` can also be use for vectorized operations.
#'
#' `POSIXt` values passed as argument will be strip of their dates in favor
#' for the new date assignment.
#'
#' @param anterior,posterior A `hms` or `POSIXt` vector.
#'
#' @return A named list with `anterior` and `posterior` values transformed.
#'
#' @export
#'
#' @examples
#' x <- lubridate::ymd_hms("2000-01-01 10:00:00")
#' y <- hms::parse_hm("22:00")
#' assign_date()
assign_date <- function(anterior, posterior) {

    # checkmate::check_multi_class(anterior, c("hms", "POSIXct", "POSIXlt"))
    # checkmate::check_multi_class(posterior, c("hms", "POSIXct", "POSIXlt"))
    # check_identical(anterior, posterior, "length")
    #
    # out <- dplyr::tibble(anterior = anterior, posterior = posterior)
    #
    # if (lubridate::is.POSIXt(anterior)) anterior <- hms::as_hms(anterior)
    # if (lubridate::is.POSIXt(posterior)) posterior <- hms::as_hms(posterior)
    #
    # out <- out %>%
    #     mutate(dummy = case_when(
    #         anterior < posterior ~ 11,
    #         anterior > posterior ~ 12,
    #         TRUE, 11))

    # INCOMPLETE

}

#' Find the shortest interval between 2 hours
#'
#' @description
#'
#' __UNDER DEVELOPMENT__
#'
#' `POSIXt` values passed as argument will be strip of their dates in favor
#' for the new date assignment.
#'
#' @details
#'
#' __UNDER DEVELOPMENT__
#'
#' ```
#'    y                 x          y                x
#'  13:00            08:00      13:00            08:00
#' --|----------------|----------|----------------|---->
#'  19h            5h            19h
#'    [(24 - 13) + 8]   (13 - 8)    [(24 - 13) + 8]
#'      2nd interval  1st interval  2nd interval
#' ```
#'
#' @param x a
#'
#' @noRd
shortest_interval <- function(x, y, ambiguous_value = 0) {

    # checkmate::check_multi_class(x, c("hms", "POSIXct", "POSIXlt"))
    # checkmate::check_multi_class(y, c("hms", "POSIXct", "POSIXlt"))
    # check_identical(x, y, "length")
    # checkmate::assert_choice(ambiguous_value, c(0, 24))
    #
    # if (lubridate::is.POSIXt(x) || lubridate::is.POSIXt(y)) {
    #     x_start <- flat_posixt(x)
    #     y_start <- flat_posixt(y)
    # } else {
    #     x_start <- convert_to_date_time(x, "POSIXct")
    #     y_start <- convert_to_date_time(y, "POSIXct")
    # }
    #
    # x_start <- convert_to_date_time(x, "POSIXct")
    # y_start <- convert_to_date_time(y, "POSIXct")
    # x_duration <- lubridate::dhours(convert_to_decimal(x))
    # y_duration <- lubridate::dhours(convert_to_decimal(y))
    #
    # x_y_interval <- lubridate::as.interval(y_duration, x_start)
    # y_x_interval <- lubridate::as.interval(x_duration, y_start)
    #
    # if (x_y_interval == y_x_interval) {
    #
    # }

}

#' __UNDER DEVELOPMENT__
#'
#' @noRd
greater_interval <- function(anterior, posterior) {

    # out <- shortest_distance(anterior, posterior)
    # out <- convert_to_date_time(out, "POSIXct")
    # out <- convert_to_date_time(out - dhours(24), "hms")
    # out

}

#' __UNDER DEVELOPMENT__
#'
#' @noRd
sum_hms <- function(...) {

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
#' @return TRUE or FALSE depending on whether its argument is of character type or not.
#' @family Utility functions
#' @export
#'
#' @examples
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
is_time <- function(x, rm_date = FALSE) {

    checkmate::assert_flag(rm_date)

    classes <-
        c("difftime", "Duration", "hms", "Period", "Date", "POSIXct",
          "POSIXlt", "Interval", "Circular")

    if (isTRUE(rm_date)) {
        classes <- stringr::str_subset(classes, "^Date$", negate = TRUE)
    }

    if (circular::is.circular(x)) {
        return(TRUE)
    }

    checkmate::test_multi_class(x, classes)

}

#' @noRd
class_collapse <- function(x) {

    glue::single_quote(glue::glue_collapse(class(x), sep = '/'))

}

#' @noRd
inline_collapse <- function(x, serial_comma = TRUE) {

    for (i in seq_along(x)) {
        x[i] <- glue::single_quote(x[i])
    }

    if (length(x) <= 2 || isFALSE(serial_comma)) {
        glue::glue_collapse(x, sep = ", ", last = " and ")
    } else {
        glue::glue_collapse(x, sep = ", ", last = ", and ")
    }

}

#' @noRd
shush <- function(x, quiet = TRUE){

    if (quiet) {
        suppressMessages(suppressWarnings(x))
    } else {
        x
    }

}

#' @noRd
message_generator <- function(type = "ok", funny = FALSE) {

    type <- stringr::str_to_lower(type)

    if (type == "ok") {
        if (isTRUE(funny)) {
            ok_messages <- c("Noice!", "Everything's A-OK",
                             "Good job (yay!)",
                             "Everything appears to be in order, Sr.!")
        } else {
            ok_messages <- c("Success", "All in order")
        }
        rlang::inform(sample(ok_messages, 1))
    }

}

#' @noRd
close_round <- function(x, digits = 5) {

    pattern_9 <- paste0("\\.", paste(rep(9, digits), collapse = ""))
    pattern_0 <- paste0("\\.", paste(rep(0, digits), collapse = ""))

    if (stringr::str_detect(x, pattern_9) ||
        stringr::str_detect(x, pattern_0)) {
        round(x)
    } else {
        x
    }

}
