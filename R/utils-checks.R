check_any_na <- function(x, name = deparse(substitute(x))) {
    if (any(is.na(x))) {
        paste0(single_quote_(name), " cannot have any missing values")
    } else {
        TRUE
    }
}

assert_any_na <- checkmate::makeAssertionFunction(check_any_na)

check_not_all_na <- function(x, name = deparse(substitute(x))) {
    if (all(is.na(x))) {
        paste0(single_quote_(name), " cannot have all values as missing")
    } else {
        TRUE
    }
}

assert_not_all_na <- checkmate::makeAssertionFunction(check_not_all_na)

check_length_one <- function(x, any.missing = TRUE,
                             name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)

    if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(single_quote_(name), " cannot have missing values")
    } else if (!(length(x) == 1)) {
        paste0(single_quote_(name), " must have length 1, not length ",
               length(x))
    } else {
        TRUE
    }
}

assert_length_one <- checkmate::makeAssertionFunction(check_length_one)

check_has_length <- function(x, any.missing = TRUE,
                             name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)

    if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(single_quote_(name), " cannot have missing values")
    } else if (length(x) < 1) {
        paste0(single_quote_(name), " must have length greater than zero")
    } else {
        TRUE
    }
}

assert_has_length <- checkmate::makeAssertionFunction(check_has_length)

check_whole_number <- function(x, any.missing = TRUE, null.ok = FALSE,
                             name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(single_quote_(name), " cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(single_quote_(name), " cannot be 'NULL'")
    } else  if (!all(is_whole_number(x), na.rm = TRUE)) {
        paste0(single_quote_(name), " must consist of whole numbers")
    } else {
        TRUE
    }
}

assert_whole_number <- checkmate::makeAssertionFunction(check_whole_number)

## `check_numeric_()` and `assert_numeric_()` were created as a workaround to
## deal with cases like `is.numeric(lubridate::duration())`. See
## https://github.com/tidyverse/lubridate/issues/942 to learn more.

check_numeric_ <- function(x, any.missing = TRUE, null.ok = FALSE,
                         name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(single_quote_(name), " cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(single_quote_(name), " cannot have 'NULL' values")
    } else  if (!is_numeric_(x)) {
        paste0("Must be of type 'numeric', not ", class_collapse(x))
    } else {
        TRUE
    }
}

assert_numeric_ <- checkmate::makeAssertionFunction(check_numeric_)

check_duration <- function(x, any.missing = TRUE, null.ok = FALSE,
                           name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(single_quote_(name), " cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(single_quote_(name), " cannot have 'NULL' values")
    } else  if (!lubridate::is.duration(x)) {
        paste0("Must be of type 'Duration', not ", class_collapse(x))
    } else {
        TRUE
    }
}

assert_duration <- checkmate::makeAssertionFunction(check_duration)

check_posixt <- function(x, any.missing = TRUE, null.ok = FALSE,
                         name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(single_quote_(name), " cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(single_quote_(name), " cannot have 'NULL' values")
    } else  if (!lubridate::is.POSIXt(x)) {
        paste0("Must be of type 'POSIXct' or 'POSIXlt', not ",
               class_collapse(x))
    } else {
        TRUE
    }
}

assert_posixt <- checkmate::makeAssertionFunction(check_posixt)

test_time <- function(x, any.missing = TRUE, null.ok = FALSE) {
    out <- check_time(x, any.missing, null.ok)
    if (isTRUE(out)) TRUE else FALSE
}

check_time <- function(x, any.missing = TRUE, null.ok = FALSE,
                       name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(single_quote_(name), " cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(single_quote_(name), " cannot have 'NULL' values")
    } else if (!is_time(x)) {
        paste0("Must be a time object, not ", class_collapse(x))
    } else {
        TRUE
    }
}

assert_time <- checkmate::makeAssertionFunction(check_time)

assert_identical <- function(..., type = "value", any.missing = TRUE,
                            null.ok = FALSE) {
    checkmate::assert_list(list(...), min.len = 2)
    checkmate::assert_choice(type, c("value", "length", "class"))
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    names <- get_names(...)
    out <- list(...)

    if (type == "length") {
        error_message <- paste0("Assertion failed: ", inline_collapse(names),
                                " must have identical lengths.")
        check <- length(unique(vapply(out, length, integer(1)))) == 1
    } else if (type == "class") {
        error_message <- paste0("Assertion failed: ", inline_collapse(names),
                                " must have identical classes.")
        check <- length(unique(lapply(out, class))) == 1
    } else {
        error_message <- paste0("Assertion failed: ", inline_collapse(names),
                                " must be identical.")
        check <- length(unique(out)) == 1
    }

    if (any(unlist(lapply(out, is.null)), na.rm = TRUE) && isTRUE(null.ok)) {
        invisible(TRUE)
    } else if (any(is.na(unlist(out))) && isFALSE(any.missing)) {
        stop(inline_collapse(names), " cannot have missing values.",
             call. = FALSE)
    } else if (any(is.null(unlist(out)), na.rm = TRUE) && isFALSE(null.ok)) {
        stop(inline_collapse(names), " cannot have 'NULL' values.",
             call. = FALSE)
    } else if (isFALSE(check)) {
        stop(error_message, call. = FALSE)
    } else {
       invisible(TRUE)
    }
}

# Used in swap_decimal() and parse_to_date_time()
check_custom_1 <- function(x, any.missing = TRUE, null.ok = FALSE,
                           name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(single_quote_(name), " cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(single_quote_(name), " cannot have 'NULL' values")
    } else if (!(is.character(x) || is_numeric_(x))) {
        paste0(single_quote_(name), " must inherit from class 'character', ",
               "'integer', or 'numeric', but has class ", class_collapse(x))
    } else {
        TRUE
    }
}

# Used in swap_decimal() and parse_to_date_time()
assert_custom_1 <- checkmate::makeAssertionFunction(check_custom_1)
