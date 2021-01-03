#' @importFrom glue backtick
#' @noRd
check_not_na <- function(x, name = deparse(substitute(x))) {

    if (any(is.na(x))) {
        glue::glue("{backtick(name)} cannot have any missing values")
    } else {
        TRUE
    }

}

#' @noRd
assert_not_na <- checkmate::makeAssertionFunction(check_not_na)

#' @importFrom glue backtick
#' @noRd
check_not_nnn <- function(x, name = deparse(substitute(x))) {

    if (any(is.na(x)) || any(is.nan(x)) || any(is.null(x))) {
        glue::glue("{backtick(name)} cannot have `NA`, `NAN` or `NULL` values")
    } else {
        TRUE
    }

}

#' @noRd
assert_not_nnn <- checkmate::makeAssertionFunction(check_not_nnn)

#' @importFrom glue backtick
#' @noRd
check_length_one <- function(x, any.missing = TRUE,
                             name = deparse(substitute(x))) {

    if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{backtick(name)} cannot have missing values")
    } else if (!(length(x) == 1)) {
        glue::glue("{backtick(name)} must have length 1, not length ",
                   "{length(x)}")
    } else {
        TRUE
    }

}

#' @noRd
assert_length_one <- checkmate::makeAssertionFunction(check_length_one)

#' @importFrom glue backtick
#' @noRd
check_has_length <- function(x, any.missing = TRUE,
                             name = deparse(substitute(x))) {

    if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{backtick(name)} cannot have missing values")
    } else if (length(x) < 1) {
        glue::glue("{backtick(name)} must have length greater than zero")
    } else {
        TRUE
    }

}

#' @noRd
assert_has_length <- checkmate::makeAssertionFunction(check_has_length)

#' @importFrom glue backtick
#' @noRd
check_posixt <- function(x, any.missing = TRUE, null.ok = FALSE,
                         name = deparse(substitute(x))) {

    if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{backtick(name)} cannot have missing values")
    } else if (any(is.null(x)) && isFALSE(null.ok)) {
        glue::glue("{backtick(name)} cannot have `NULL` values")
    } else  if (!lubridate::is.POSIXt(x)) {
        glue::glue("Must be of type POSIXt, not {class_collapse(x)}")
    } else {
        TRUE
    }

}

#' @noRd
assert_posixt <- checkmate::makeAssertionFunction(check_posixt)

#' @importFrom glue backtick
#' @noRd
check_time <- function(x, any.missing = TRUE, null.ok = FALSE,
                       name = deparse(substitute(x))) {

    if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{backtick(name)} cannot have missing values")
    } else if (any(is.null(x)) && isFALSE(null.ok)) {
        glue::glue("{backtick(name)} cannot have `NULL` values")
    } else if (!is_time(x)) {
        glue::glue("Must be a time vector object, not {class_collapse(x)}")
    } else {
        TRUE
    }

}

#' @noRd
test_time <- function(x, rm_date = FALSE) {

    is_time(x, rm_date)

}

#' @noRd
assert_time <- checkmate::makeAssertionFunction(check_time)

#' Used on convert_to_date_time()
#' @importFrom glue backtick
#' @noRd
check_custom_1 <- function(x, any.missing = TRUE, null.ok = FALSE,
                           name = deparse(substitute(x))) {

    if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{backtick(name)} cannot have missing values")
    } else if (any(is.null(x)) && isFALSE(null.ok)) {
        glue::glue("{backtick(name)} cannot have `NULL` values")
    } else if (!(is.character(x) || is.numeric(x) || is_time(x) ||
               (any(is.na(x)) && length(x) == 1))) {
        glue::glue("Check function documentation")
    } else {
        TRUE
    }

}

#' Used on convert_to_date_time()
#' @noRd
assert_custom_1 <- checkmate::makeAssertionFunction(check_custom_1)

#' Used on convert_to_rad()
#' @importFrom glue backtick
#' @noRd
check_custom_2 <- function(x, any.missing = TRUE, null.ok = FALSE,
                           name = deparse(substitute(x))) {

    if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{backtick(name)} cannot have missing values")
    } else if (any(is.null(x)) && isFALSE(null.ok)) {
        glue::glue("{backtick(name)} cannot have `NULL` values")
    } else if (!is_time(x, rm_date = TRUE) && !is.numeric(x)) {
        glue::glue("{backtick(name)} is not a valid value. ",
                   "Check function documentation")
    } else {
        TRUE
    }

}

#' Used on convert_to_rad()
#' @noRd
assert_custom_2 <- checkmate::makeAssertionFunction(check_custom_2)

#' Used on convert_to_decimal()
#' @importFrom glue backtick
#' @noRd
check_custom_3 <- function(x, any.missing = TRUE, null.ok = FALSE,
                           name = deparse(substitute(x))) {

    if (any(is.na(x)) && isFALSE(any.missing)) {
        glue::glue("{backtick(name)} cannot have missing values")
    } else if (any(is.null(x)) && isFALSE(null.ok)) {
        glue::glue("{backtick(name)} cannot have `NULL` values")
    } else if (!is_time(x) && !is.numeric(x)) {
        glue::glue("{backtick(name)} is not a valid value. ",
                   "Check function documentation")
    } else {
        TRUE
    }

}

#' Use on convert_to_decimal()
#' @noRd
assert_custom_3 <- checkmate::makeAssertionFunction(check_custom_3)
