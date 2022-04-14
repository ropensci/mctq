# Sort by type or by alphabetical order.

test_length_one <- function(x) length(x) == 1

check_length_one <- function(x, name = deparse(substitute(x))) {
    if (!(test_length_one(x))) {
        paste0(single_quote_(name), " must have length 1, not length ",
               length(x))
    } else {
        TRUE
    }
}

assert_length_one <- checkmate::makeAssertionFunction(check_length_one)

test_has_length <- function(x) length(x) >= 1

check_has_length <- function(x, any.missing = TRUE,
                             name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)

    if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(single_quote_(name), " cannot have missing values")
    } else if (!test_has_length(x)) {
        paste0(single_quote_(name), " must have length greater than zero")
    } else {
        TRUE
    }
}

assert_has_length <- checkmate::makeAssertionFunction(check_has_length)

# `*_numeric_()` was created as a workaround to deal with cases like
# `is.numeric(lubridate::duration())`. See
# https://github.com/tidyverse/lubridate/issues/942 to learn more.

test_numeric_ <- function(x, lower = -Inf, upper = Inf, any.missing = TRUE,
                          null.ok = FALSE) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    classes <- c("integer", "double", "numeric")

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        FALSE
    } else if (checkmate::test_subset(class(x)[1], classes) &&
               !all(x >= lower & x <= upper, na.rm = TRUE)) {
        FALSE
    } else {
        checkmate::test_subset(class(x)[1], classes)
    }
}

check_numeric_ <- function(x, lower = - Inf, upper = Inf, any.missing = TRUE,
                           null.ok = FALSE,
                         name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    classes <- c("integer", "double", "numeric")

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(single_quote_(name), " cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(single_quote_(name), " cannot be 'NULL'")
    } else if (checkmate::test_subset(class(x)[1], classes) &&
               !all(x >= lower, na.rm = TRUE)) {
        paste0("Element ", which(x < lower)[1], " is not >= ", lower)
    } else if (checkmate::test_subset(class(x)[1], classes) &&
               !all(x <= upper, na.rm = TRUE)) {
        paste0("Element ", which(x > upper)[1], " is not <= ", upper)
    } else  if (!test_numeric_(x)) {
        paste0("Must be of type 'numeric', not ", class_collapse(x))
    } else {
        TRUE
    }
}

assert_numeric_ <- checkmate::makeAssertionFunction(check_numeric_)

test_duration <- function(x, lower = - Inf, upper = Inf, any.missing = TRUE,
                          null.ok = FALSE) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        FALSE
    } else if (lubridate::is.duration(x) &&
               !all(x >= lower & x <= upper, na.rm = TRUE)) {
        FALSE
    } else {
        lubridate::is.duration(x)
    }
}

check_duration <- function(x, lower = - Inf, upper = Inf, any.missing = TRUE,
                           null.ok = FALSE,
                           name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(single_quote_(name), " cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(single_quote_(name), " cannot be 'NULL'")
    } else if (lubridate::is.duration(x) && !all(x >= lower, na.rm = TRUE)) {
        paste0("Element ", which(x < lower)[1], " is not >= ", lower)
    } else if (lubridate::is.duration(x) && !all(x <= upper, na.rm = TRUE)) {
        paste0("Element ", which(x > upper)[1], " is not <= ", upper)
    } else  if (!test_duration(x)) {
        paste0("Must be of type 'Duration', not ", class_collapse(x))
    } else {
        TRUE
    }
}

assert_duration <- checkmate::makeAssertionFunction(check_duration)

test_hms <- function(x, lower = - Inf, upper = Inf, any.missing = TRUE,
                          null.ok = FALSE) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        FALSE
    } else if (hms::is_hms(x) &&
               !all(x >= lower & x <= upper, na.rm = TRUE)) {
        FALSE
    } else {
        hms::is_hms(x)
    }
}

check_hms <- function(x, lower = - Inf, upper = Inf, any.missing = TRUE,
                           null.ok = FALSE,
                           name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(single_quote_(name), " cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(single_quote_(name), " cannot be 'NULL'")
    } else if (hms::is_hms(x) && !all(x >= lower, na.rm = TRUE)) {
        paste0("Element ", which(x < lower)[1], " is not >= ", lower)
    } else if (hms::is_hms(x) && !all(x <= upper, na.rm = TRUE)) {
        paste0("Element ", which(x > upper)[1], " is not <= ", upper)
    } else  if (!test_hms(x)) {
        paste0("Must be of type 'hms', not ", class_collapse(x))
    } else {
        TRUE
    }
}

assert_hms <- checkmate::makeAssertionFunction(check_hms)

test_posixt <- function(x, lower = - Inf, upper = Inf, any.missing = TRUE,
                     null.ok = FALSE) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        FALSE
    } else if (lubridate::is.POSIXt(x) &&
               !all(x >= lower & x <= upper, na.rm = TRUE)) {
        FALSE
    } else {
        lubridate::is.POSIXt(x)
    }
}

check_posixt <- function(x, lower = - Inf, upper = Inf, any.missing = TRUE,
                      null.ok = FALSE,
                      name = deparse(substitute(x))) {
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    if (is.null(x) && isTRUE(null.ok)) {
        TRUE
    } else if (any(is.na(x)) && isFALSE(any.missing)) {
        paste0(single_quote_(name), " cannot have missing values")
    } else if (is.null(x) && isFALSE(null.ok)) {
        paste0(single_quote_(name), " cannot be 'NULL'")
    } else if (lubridate::is.POSIXt(x) && !all(x >= lower, na.rm = TRUE)) {
        paste0("Element ", which(x < lower)[1], " is not >= ", lower)
    } else if (lubridate::is.POSIXt(x) && !all(x <= upper, na.rm = TRUE)) {
        paste0("Element ", which(x > upper)[1], " is not <= ", upper)
    } else  if (!test_posixt(x)) {
        paste0("Must be of type 'POSIXct' or 'POSIXlt', not ",
               class_collapse(x))
    } else {
        TRUE
    }
}

assert_posixt <- checkmate::makeAssertionFunction(check_posixt)

assert_identical <- function(..., type = "value", any.missing = TRUE,
                            null.ok = FALSE) {

    if (!checkmate::test_list(list(...), min.len = 2)) {
        cli::cli_abort("'...' must have 2 or more elements.")
    }

    checkmate::assert_choice(type, c("value", "length", "class"))
    checkmate::assert_flag(any.missing)
    checkmate::assert_flag(null.ok)

    names <- get_names(...)
    out <- list(...)

    if (type == "length") {
        error_message <- paste0("Assertion failed: {single_quote_(names)} ",
                                " must have identical lengths.")
        check <- length(unique(vapply(out, length, integer(1)))) == 1
    } else if (type == "class") {
        error_message <- paste0("Assertion failed: {single_quote_(names)} ",
                                " must have identical classes.")
        check <- length(unique(lapply(out, class))) == 1
    } else {
        error_message <- paste0("Assertion failed: {single_quote_(names)} ",
                                " must be identical.")
        check <- length(unique(out)) == 1
    }

    if (any(unlist(lapply(out, is.null)), na.rm = TRUE) && isTRUE(null.ok)) {
        invisible(TRUE)
    } else if (any(is.na(unlist(out))) && isFALSE(any.missing)) {
        cli::cli_abort("{names} cannot have missing values.")
    } else if (any(is.null(unlist(out)), na.rm = TRUE) && isFALSE(null.ok)) {
        cli::cli_abort("{names} cannot be 'NULL'")
    } else if (isFALSE(check)) {
        cli::cli_abort(error_message)
    } else {
       invisible(TRUE)
    }
}
