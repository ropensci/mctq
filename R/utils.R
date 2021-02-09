#' @family utility functions
#' @noRd
midday_change <- function(x) {

    checkmate::assert_multi_class(x, c("hms", "POSIXct", "POSIXlt"))

    x <- flat_posixt(convert(x, "posixct"))

    x <- dplyr::case_when(
        lubridate::hour(x) < 12 ~ change_day(x, 2),
        TRUE ~ x
    )

    x

}

#' @rdname midday_change
#' @noRd
mdc <- function(x) midday_change(x)

#' @family utility functions
#' @noRd
flat_posixt <- function(x, force_utc = TRUE, base = "1970-01-01") {

    assert_posixt(x, null.ok = FALSE)
    checkmate::assert_flag(force_utc)
    checkmate::assert_string(base)

    lubridate::date(x) <- base

    if (isTRUE(force_utc)) {
        x <- lubridate::force_tz(x, "UTC")
    }

    x

}

#' @family utility functions
#' @noRd
change_date <- function(x, date) {

    classes <- c("Date", "POSIXct", "POSIXlt")
    checkmate::assert_multi_class(x, classes, null.ok = FALSE)

    classes <- c("character", "Date")
    checkmate::assert_multi_class(date, classes, null.ok = FALSE)
    assert_length_one(date)

    lubridate::date(x) <- date

    x

}

#' @family utility functions
#' @noRd
change_day <- function(x, day) {

    classes <- c("Date", "POSIXct", "POSIXlt")

    checkmate::assert_multi_class(x, classes, null.ok = FALSE)
    checkmate::assert_number(day, lower = 1, upper = 31)

    if (any(lubridate::month(x) %in% c(4, 6, 9, 11)) && day > 30) {
        stop("You can't assign more than 30 days to April, June, ",
             "September, or November.", call. = FALSE)
    }

    if (any(lubridate::month(x) == 2 & !lubridate::leap_year(x)) && day > 28) {
        stop("You can't assign more than 28 days to February in ",
             "non-leap years.", call. = FALSE)
    }

    if (any(lubridate::month(x) == 2 & lubridate::leap_year(x)) && day > 29) {
        stop("You can't assign more than 29 days to February in a leap year",
             call. = FALSE)
    }

    lubridate::day(x) <- day

    x

}

#' @family utility functions
#' @noRd
is_time <- function(x, rm = NULL) {

    checkmate::assert_character(rm, any.missing = FALSE, null.ok = TRUE)

    classes <- c("Duration", "Period", "difftime", "hms", "Date", "POSIXct",
                 "POSIXlt", "Interval")

    if (!is.null(rm)) {
        rm <- paste0("^", rm, "$", collapse = "|")
        classes <- str_subset_(classes, rm, negate = TRUE)
    }

    # if (circular::is.circular(x) && !("circular" %in% rm)) {
    #     return(TRUE)
    # }

    checkmate::test_multi_class(x, classes)

}

#' @family utility functions
#' @noRd
is_numeric_ <- function(x) {

    any(class(x) %in% c("integer", "double", "numeric"))

}

#' @family utility functions
#' @noRd
is_whole_number <- function(x, tol = .Machine$double.eps^0.5) {

    checkmate::assert_multi_class(x, c("integer", "numeric"))

    abs(x - round(x)) < tol # Example function from ?integer

}

#' @family utility functions
#' @noRd
single_quote_ <- function(x) {

    paste0("'", x, "'")

}

#' @family utility functions
#' @noRd
backtick_ <- function(x) {

    paste0("`", x, "`")

}

#' @family utility functions
#' @noRd
class_collapse <- function(x) {

    single_quote_(paste0(class(x), collapse = "/"))

}

#' @family utility functions
#' @noRd
paste_collapse <- function(x, sep = "", last = "") {

    checkmate::assert_string(sep)
    checkmate::assert_string(last)

    if (length(x) == 1) {
        x
    } else {
        paste0(paste(x[-length(x)], collapse = sep), last, x[length(x)])
    }

}

#' @family utility functions
#' @noRd
inline_collapse <- function(x, single_quote = TRUE, serial_comma = TRUE) {

    checkmate::assert_flag(single_quote)
    checkmate::assert_flag(serial_comma)

    if (isTRUE(single_quote)) x <- single_quote_(x)

    if (length(x) <= 2 || isFALSE(serial_comma)) {
        paste_collapse(x, sep = ", ", last = " and ")
    } else {
        paste_collapse(x, sep = ", ", last = ", and ")
    }

}

#' @family utility functions
#' @noRd
shush <- function(x, quiet = TRUE){

    if (isTRUE(quiet)) {
        suppressMessages(suppressWarnings(x))
    } else {
        x
    }

}

#' @family utility functions
#' @noRd
hms_interval <- function(start, end, tz = "UTC") {

    checkmate::check_multi_class(start, c("hms", "POSIXct", "POSIXlt"))
    checkmate::check_multi_class(end, c("hms", "POSIXct", "POSIXlt"))

    start <- flat_posixt(convert(start, "posixct", tz = tz), FALSE)
    end <- flat_posixt(convert(end, "posixct", tz = tz), FALSE)

    lubridate::interval(start, end)

}

#' @family utility functions
#' @noRd
close_round <- function(x, digits = 5) {

    pattern_9 <- paste0("\\.", paste(rep(9, digits), collapse = ""))
    pattern_0 <- paste0("\\.", paste(rep(0, digits), collapse = ""))

    dplyr::case_when(
        grepl(pattern_9, x) | grepl(pattern_0, x) ~ round(x),
        TRUE ~ x)

}

#' @family utility functions
#' @noRd
swap <- function(x, y) {

    assert_identical(x, y, type = "length")
    assert_identical(x, y, type = "class")

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

    assert_identical(x, y, type = "length")
    assert_identical(x, y, type = "class")
    checkmate::assert_choice(condition, choices)

    condition <- sub("x", "a", condition)
    condition <- sub("y", "b", condition)

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

#' @family utility functions
#' @noRd
get_names <- function(...) {

    out <- lapply(substitute(list(...))[-1], deparse)
    out <- vapply(out, unlist, character(1))
    out <- noquote(out)
    out <- gsub("\\\"","", out)

    out

}

#' @family utility functions
#' @noRd
clock_roll <- function(x, class = "hms") {

    out <- flat_posixt(lubridate::as_datetime(x))
    convert(out, class)

}

#' @family utility functions
#' @noRd
na_as <- function(x) {

    classes <- c("character", "integer", "double", "numeric", "Duration",
                 "Period", "difftime", "hms", "Date", "POSIXct", "POSIXlt")

    if (is.logical(x)) {
        as.logical(NA)
    } else if (checkmate::test_multi_class(x, classes)) {
        convert(NA, class(x)[1])
    } else {
        stop(paste0(
            "`na_as()` don't support objects of class ", class_collapse(x), "."
            ), call. = FALSE)
    }

}

#' @family utility functions
#' @noRd
get_class <- function(x) {

    foo <- function(x) {
        class(x)[1]
    }

    if (is.list(x) || is.data.frame(x)) {
        vapply(x, foo, character(1))
    } else {
        class(x)[1]
    }

}

#' @family utility functions
#' @noRd
fix_character <- function(x) {

    checkmate::assert_character(x)

    x <- trimws(x)

    for (i in c("", "NA")) {
        x <- dplyr::na_if(x, i)
    }

    x

}

#' @family utility functions
#' @noRd
str_extract_ <- function(string, pattern, ignore.case = FALSE, perl = TRUE,
                         fixed = FALSE, useBytes = FALSE, invert = FALSE) {

    checkmate::assert_string(pattern)
    checkmate::assert_flag(ignore.case)
    checkmate::assert_flag(perl)
    checkmate::assert_flag(fixed)
    checkmate::assert_flag(useBytes)
    checkmate::assert_flag(invert)

    match <- regexpr(pattern, string, ignore.case = ignore.case, perl = perl,
                     fixed = fixed, useBytes = useBytes)
    out <- regmatches(string, match, invert = invert)

    if (length(out) == 0) as.character(NA) else out

}

#' @family utility functions
#' @noRd
str_subset_ <- function(string, pattern, negate = FALSE, ignore.case = FALSE,
                        perl = TRUE, fixed = FALSE, useBytes = FALSE) {

    checkmate::assert_string(pattern)
    checkmate::assert_flag(negate)

    match <- grepl(pattern, string, ignore.case = ignore.case, perl = perl,
                   fixed = fixed, useBytes = useBytes)

    if (isTRUE(negate)) {
        out <- subset(string, !match)
    } else {
        out <- subset(string, match)
    }

    if (length(out) == 0) as.character(NA) else out

}
