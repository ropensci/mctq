#' Change dates by time of day
#'
#' @description
#'
#' `midday_change()` changes the dates of `POSIXt` objects accordingly to the
#' time of day registered in the object values. The function do this by flatting
#' the date to `0000-01-01` and them adding a day if the hour is lower than 12.
#'
#' This can be use to help determine when a subject's sleep episode started and
#' ended, if all that you have is the time of sleep onset and offset. Note that
#' this method can only be used to this matter if the sleep duration have a too
#' high value.
#'
#' @param x A `POSIXt` vector.
#'
#' @return A vector of the same `POSIXt` class type as `x`.
#'
#' @family utility functions
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

    x <- dplyr::case_when(
        lubridate::hour(x) < 12 ~ change_day(x, 2),
        TRUE ~ x
    )

    x

}

#' Flat dates of `POSIXt` objects
#'
#' @description
#'
#' `flat_posixt()` changes the dates of `POSIXt` objects to `0000-01-01`. This
#' can be use to standardizing a point of origin to time values.
#'
#' @param x A `POSIXt` vector.
#'
#' @return A vector of the same `POSIXt` class type as `x` with `0000-01-01` as
#'   date.
#'
#' @family utility functions
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

#' Change day of a `Date` or `POSIXt` object
#'
#' @description
#'
#' `change_day()` is a utility function that help change days of `Date` or
#' `POSIXt` objects with the need for a direct reassignment.
#'
#' Note that, if your add a day that surpasses the days in  the monthy
#'
#' @param x A `Date` or `POSIXt` vector.
#' @param day A number, between 1-31 indicating the new day of `x`.
#' @return A `Date` or `POSIXt` vector with the indicated day.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' change_day(lubridate::as_date("1888-02-07"), 20)
#' #> [1] "1888-02-20" # Expected
#' change_day(lubridate::ymd_hms("1987-12-24 07:45:32"), 15)
#' #> [1] "1987-12-15 07:45:32 UTC" # Expected
change_day <- function(x, day) {

    classes <- c("Date", "POSIXct", "POSIXlt")

    checkmate::assert_multi_class(x, classes, null.ok = FALSE)
    checkmate::assert_number(day, lower = 1, upper = 31)

    lubridate::day(x) <- day

    x

}

#' Check if a object inherits a set of date/time classes
#'
#' @description
#'
#' `is_time()` returns a boolean flag checking for objects of class `Duration`,
#' `Period`, `difftime`, `hms`, `Date`, `POSIXct`, `POSIXlt`, `Interval`, or
#' `Circular`.
#'
#' @param x Any kind of R object.
#' @param rm (optional) A character vector indicating names of object classes to
#'   remove from the check (case sensitive) (default: `NULL`).
#'
#' @return If `rm` is `NULL`, a boolean flag checking if `x` inherits a
#'   `Duration`, `Period`, `difftime`, `hms`, `Date`, `POSIXct`, `POSIXlt`,
#'   `Interval`, or `Circular` class. Else, the same as the previous, but
#'   without the classes indicated in `rm`.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' is_time(lubridate::dhours())
#' #> [1] TRUE # Expected
#' is_time(as.Date("2020-01-01"))
#' #> [1] TRUE # Expected
#' is_time(as.Date("2020-01-01"), rm = "Date")
#' #> [1] FALSE # Expected
#' is_time(iris)
#' #> [1] FALSE # Expected
#' is_time(letters)
#' #> [1] FALSE # Expected
is_time <- function(x, rm = NULL) {

    checkmate::assert_character(rm, any.missing = FALSE, min.len = 1,
                                null.ok = TRUE)

    classes <-
        c("difftime", "Duration", "hms", "Period", "Date", "POSIXct",
          "POSIXlt", "Interval", "Circular")

    if (!is.null(rm)) {
        for (i in paste0("^", rm, "$")){
            classes <- stringr::str_subset(classes, i, negate = TRUE)
        }
    }

    if (circular::is.circular(x) && !("circular" %in% rm)) {
        return(TRUE)
    }

    checkmate::test_multi_class(x, classes)

}

#' Collapse class names
#'
#' @description
#'
#' `class_collapse()` is a utility function to help build return messages with
#' [glue::glue()]. It collapses the value of `class(x)` with a `/` and single
#' quote it.
#'
#' @param x Any kind of R object.
#'
#' @return A string with `class(x)` value collapsed with `/` encapsulated with
#'   single quotes.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' class_collapse("")
#' #> [1] "'character'" # Expected
#' class_collapse(1)
#' #> [1] "'numeric'" # Expected
class_collapse <- function(x) {

    glue::single_quote(glue::glue_collapse(class(x), sep = '/'))

}

#' @family utility functions
#' @noRd
is_numeric_ <- function(x) {

    any(class(x) %in% c("integer", "double", "numeric"))

}

#' @family utility functions
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

#' @family utility functions
#' @noRd
shush <- function(x, quiet = TRUE){

    if (quiet) {
        suppressMessages(suppressWarnings(x))
    } else {
        x
    }

}

#' @family utility functions
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

#' @family utility functions
#' @noRd
swap <- function(x, y) {

    check_identical(x, y, "length")
    check_identical(x, y, "class")

    a <- x
    b <- y

    x <- b
    y <- a

    list(x = x, y = y)

}

#' @family utility functions
#' @noRd
swap_if <- function(x, y, condition = "x > y") {

    choices <- c("x == y", "x < y", "x <= y", "x > y", "x >= y")

    assert_identical(x, y, "length")
    assert_identical(x, y, "class")
    checkmate::assert_choice(condition, choices)

    condition <- stringr::str_replace(condition, "x", "a")
    condition <- stringr::str_replace(condition, "y", "b")

    a <- x
    b <- y

    x <- dplyr::if_else(eval(parse(text = condition)), b, a)
    y <- dplyr::if_else(eval(parse(text = condition)), a, b)

    list(x = x, y = y)

}

#' @family utility functions
#' @noRd
count_na <- function(x) {

    length(which(is.na(x)))

}

#' @family utility functions
#' @noRd
escape_regex <- function(x) {
    gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)
}
