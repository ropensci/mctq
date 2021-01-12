#' @family check functions
#' @noRd
check_any_na <- function(x, name = deparse(substitute(x))) {

    if (any(is.na(x))) {
        glue::glue("{glue::backtick(name)} cannot have any missing values")
    } else {
        TRUE
    }

}

#' @family check functions
#' @noRd
assert_any_na <- checkmate::makeAssertionFunction(check_any_na)

#' @family check functions
#' @noRd
check_not_all_na <- function(x, name = deparse(substitute(x))) {

    if (all(is.na(x))) {
        glue::glue("{glue::backtick(name)} cannot have all values as missing")
    } else {
        TRUE
    }

}

#' @family check functions
#' @noRd
assert_not_all_na <- checkmate::makeAssertionFunction(check_not_all_na)

#' @family check functions
#' @noRd
check_length_one <- function(x, any.missing = TRUE,
                             name = deparse(substitute(x))) {

    checkmate::assert_flag(any.missing)

    if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{glue::backtick(name)} cannot have missing values")
    } else if (!(length(x) == 1)) {
        glue::glue("{glue::backtick(name)} must have length 1, not length ",
                   "{length(x)}")
    } else {
        TRUE
    }

}

#' @family check functions
#' @noRd
assert_length_one <- checkmate::makeAssertionFunction(check_length_one)

#' @family check functions
#' @noRd
check_has_length <- function(x, any.missing = TRUE,
                             name = deparse(substitute(x))) {

    checkmate::assert_flag(any.missing)

    if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{glue::backtick(name)} cannot have missing values")
    } else if (length(x) < 1) {
        glue::glue("{glue::backtick(name)} must have length greater than zero")
    } else {
        TRUE
    }

}

#' @family check functions
#' @noRd
assert_has_length <- checkmate::makeAssertionFunction(check_has_length)

#' @family check functions
#' @noRd
check_numeric_ <- function(x, any.missing = TRUE, null.ok = FALSE,
                         name = deparse(substitute(x))) {

    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (any(is.null(x)) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{glue::backtick(name)} cannot have missing values")
    } else if (any(is.null(x)) && isFALSE(null.ok)) {
        glue::glue("{glue::backtick(name)} cannot have `NULL` values")
    } else  if (!is_numeric_(x)) {
        glue::glue("Must be of type 'numeric', not {class_collapse(x)}")
    } else {
        TRUE
    }

}

#' @family check functions
#' @noRd
assert_numeric_ <- checkmate::makeAssertionFunction(check_numeric_)

#' @family check functions
#' @noRd
check_posixt <- function(x, any.missing = TRUE, null.ok = FALSE,
                         name = deparse(substitute(x))) {

    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (any(is.null(x)) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{glue::backtick(name)} cannot have missing values")
    } else if (any(is.null(x)) && isFALSE(null.ok)) {
        glue::glue("{glue::backtick(name)} cannot have `NULL` values")
    } else  if (!lubridate::is.POSIXt(x)) {
        glue::glue("Must be of type 'POSIXct' or 'POSIXlt', not ",
                   "{class_collapse(x)}")
    } else {
        TRUE
    }

}

#' @family check functions
#' @noRd
assert_posixt <- checkmate::makeAssertionFunction(check_posixt)

test_time <- function(x, any.missing = TRUE, null.ok = FALSE) {

    out <- check_time(x, any.missing, null.ok)
    if (isTRUE(out)) TRUE else FALSE

}

#' @family check functions
#' @noRd
check_time <- function(x, any.missing = TRUE, null.ok = FALSE,
                       name = deparse(substitute(x))) {

    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (any(is.null(x)) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{glue::backtick(name)} cannot have missing values")
    } else if (any(is.null(x)) && isFALSE(null.ok)) {
        glue::glue("{glue::backtick(name)} cannot have `NULL` values")
    } else if (!is_time(x)) {
        glue::glue("Must be a time object, not {class_collapse(x)}")
    } else {
        TRUE
    }

}

#' @family check functions
#' @noRd
assert_time <- checkmate::makeAssertionFunction(check_time)

#' @family check functions
#' @noRd
check_identical <- function(x, y, type = "value", any.missing = TRUE,
                            null.ok = FALSE,
                            x_name = deparse(substitute(x)),
                            y_name = deparse(substitute(y))) {

    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)
    checkmate::assert_choice(type, c("value", "length", "class"))

    if (type == "length") {
        error_message <- glue::glue("{glue::backtick(x_name)} and ",
                                    "{glue::backtick(y_name)} ",
                                    "must have identical lengths")
        x <- length(x)
        y <- length(y)
    } else if (type == "class") {
        error_message <- glue::glue("{glue::backtick(x_name)} and ",
                                    "{glue::backtick(y_name)} ",
                                    "must have identical classes")

        x <- class(x)
        y <- class(y)
    } else {
        error_message <- glue::glue("{glue::backtick(x_name)} and ",
                                    "{glue::backtick(y_name)} ",
                                    "must be identical")
    }

    if (any(is.null(x)) && any(is.null(y)) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && any(is.na(y)) && isFALSE(any.missing)) {
        glue::glue("{glue::backtick(x_name)} and {glue::backtick(y_name)} ",
                   "cannot have missing values")
    } else if (any(is.null(x)) && any(is.null(y)) && isFALSE(null.ok)) {
        glue::glue("{glue::backtick(x_name)} and {glue::backtick(y_name)} ",
                   "cannot have `NULL` values")
    } else if (!identical(x, y)) {
        error_message
    } else {
        TRUE
    }

}

#' @family check functions
#' @noRd
assert_identical <- checkmate::makeAssertionFunction(check_identical)

#' Used in swap_decimal()
#' @family check functions
#' @noRd
check_custom_1 <- function(x, any.missing = TRUE, null.ok = FALSE,
                           name = deparse(substitute(x))) {

    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (any(is.null(x)) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{glue::backtick(name)} cannot have missing values")
    } else if (any(is.null(x)) && isFALSE(null.ok)) {
        glue::glue("{glue::backtick(name)} cannot have `NULL` values")
    } else if (!(is.character(x) || is_numeric_(x))) {
        glue::glue("Must inherit from class 'character'/'numeric', ",
                   "but has class {class_collapse(x)}")
    } else {
        TRUE
    }

}

#' Used in swap_decimal()
#' @family check functions
#' @noRd
assert_custom_1 <- checkmate::makeAssertionFunction(check_custom_1)
