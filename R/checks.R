# Copyright free functions, copy as you wish.

#' @noRd
check_not_na <- function(x, name = deparse(substitute(x))) {

    if (any(is.na(x))) {
        glue::glue("{glue::backtick(name)} cannot have any missing values")
    } else {
        TRUE
    }

}

#' @noRd
assert_not_na <- checkmate::makeAssertionFunction(check_not_na)

#' @noRd
check_not_nnn <- function(x, name = deparse(substitute(x))) {

    if (any(is.na(x)) || any(is.nan(x)) || any(is.null(x))) {
        glue::glue("{glue::backtick(name)} cannot have `NA`, `NAN` or ",
                   "`NULL` values")
    } else {
        TRUE
    }

}

#' @noRd
assert_not_nnn <- checkmate::makeAssertionFunction(check_not_nnn)

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

#' @noRd
assert_length_one <- checkmate::makeAssertionFunction(check_length_one)

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

#' @noRd
assert_has_length <- checkmate::makeAssertionFunction(check_has_length)

#' @noRd
check_posixt <- function(x, any.missing = TRUE, null.ok = FALSE,
                         name = deparse(substitute(x))) {

    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (any(is.null(x)) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{backtick(name)} cannot have missing values")
    } else if (any(is.null(x)) && isFALSE(null.ok)) {
        glue::glue("{glue::backtick(name)} cannot have `NULL` values")
    } else  if (!lubridate::is.POSIXt(x)) {
        glue::glue("Must be of type POSIXt, not {class_collapse(x)}")
    } else {
        TRUE
    }

}

#' @noRd
assert_posixt <- checkmate::makeAssertionFunction(check_posixt)

test_time <- function(x, any.missing = TRUE, null.ok = FALSE) {

    out <- check_time(x, any.missing, null.ok)
    if (isTRUE(out)) TRUE else FALSE

}

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
        glue::glue("Must be a time vector object, not {class_collapse(x)}")
    } else {
        TRUE
    }

}

#' @noRd
assert_time <- checkmate::makeAssertionFunction(check_time)

#' @noRd
check_identical <- function(x, y, type = "value", any.missing = TRUE,
                            null.ok = FALSE,
                            x_name = deparse(substitute(x)),
                            y_name = deparse(substitute(y))) {

    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)
    checkmate::assert_choice(type, c("value", "length"))

    if (type == "length") {
        error_message <- glue::glue("{glue::backtick(x_name)} and ",
                                    "{glue::backtick(y_name)} ",
                                    "must have identical lengths")
        x <- length(x)
        y <- length(y)
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

#' @noRd
assert_identical <- checkmate::makeAssertionFunction(check_identical)

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
    } else if (!(is.character(x) || is.numeric(x) || is_time(x) ||
               (any(is.na(x)) && length(x) == 1))) {
        glue::glue("Check function documentation")
    } else {
        TRUE
    }

}

#' @noRd
assert_custom_1 <- checkmate::makeAssertionFunction(check_custom_1)

#' Used on convert_to_rad()
#' @noRd
check_custom_2 <- function(x, any.missing = TRUE, null.ok = FALSE,
                           name = deparse(substitute(x))) {

    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (any(is.null(x)) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{glue::backtick(name)} cannot have missing values")
    } else if (any(is.null(x)) && isFALSE(null.ok)) {
        glue::glue("{glue::backtick(name)} cannot have `NULL` values")
    } else if (!is_time(x, rm_date = TRUE) && !is.numeric(x)) {
        glue::glue("{glue::backtick(name)} is not a valid value. ",
                   "Check function documentation")
    } else {
        TRUE
    }

}

#' Used on convert_to_rad()
#' @noRd
assert_custom_2 <- checkmate::makeAssertionFunction(check_custom_2)

#' Used on convert_to_decimal()
#' @noRd
check_custom_3 <- function(x, any.missing = TRUE, null.ok = FALSE,
                           name = deparse(substitute(x))) {

    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (any(is.null(x)) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{glue::backtick(name)} cannot have missing values")
    } else if (any(is.null(x)) && isFALSE(null.ok)) {
        glue::glue("{glue::backtick(name)} cannot have `NULL` values")
    } else if (!is_time(x) && !is.numeric(x)) {
        glue::glue("{glue::backtick(name)} is not a valid value. ",
                   "Check function documentation")
    } else {
        TRUE
    }

}

#' Used on convert_to_decimal()
#' @noRd
assert_custom_3 <- checkmate::makeAssertionFunction(check_custom_3)

#' @noRd
check_custom_4 <- function(x, choices, any.missing = FALSE, null.ok = TRUE,
                           name = deparse(substitute(x))) {

    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (any(is.null(x)) && isTRUE(null.ok)) {
        TRUE
    } else if (!identical(class(x), class(choices))) {
        glue::glue("{glue::backtick(name)} and choices must have identical ",
                   "classes")
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{glue::backtick(name)} cannot have missing values")
    } else if (any(is.null(x)) && isFALSE(null.ok)) {
        glue::glue("{glue::backtick(name)} cannot have `NULL` values")
    } else if (!all(x %in% choices)) {
        violating <- x[!(x %in% choices)]
        if (length(violating) == 1) {
            glue::glue("{inline_collapse(violating)} is not a valid value. ",
                       "Check function documentation")
        } else {
            glue::glue("{inline_collapse(violating)} are not valid values. ",
                       "Check function documentation")
        }
    } else {
        TRUE
    }

}

#' @noRd
assert_custom_4 <- checkmate::makeAssertionFunction(check_custom_4)
