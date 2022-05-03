# Sort by type or alphabetical order.

flat_posixt <- function(posixt, base = as.Date("1970-01-01"),
                        force_tz = TRUE, tz = "UTC") {
    assert_posixt(posixt, null.ok = FALSE)
    checkmate::assert_date(base, len = 1, all.missing = FALSE)
    checkmate::assert_flag(force_tz)
    checkmate::assert_choice(tz, OlsonNames())

    lubridate::date(posixt) <- base

    if (isTRUE(force_tz)) {
        lubridate::force_tz(posixt, tz)
    } else {
        posixt
    }
}

midday_change <- function(time) {
    checkmate::assert_multi_class(time, c("hms", "POSIXct", "POSIXlt"))

    if (hms::is_hms(time)) time <- as.POSIXct(time)
    time <- flat_posixt(time)

    dplyr::case_when(
        lubridate::hour(time) < 12 ~ change_day(time, 2),
        TRUE ~ time
    )
}

interval_mean <- function(start, end, ambiguity = 24) {
    classes <- c("Duration", "difftime", "hms", "POSIXct", "POSIXlt")

    checkmate::assert_multi_class(start, classes)
    checkmate::assert_multi_class(end, classes)
    checkmate::assert_choice(ambiguity, c(0, 24 , NA))

    start <- cycle_time(hms::hms(extract_seconds(start)),
                        cycle = lubridate::ddays())
    end <- cycle_time(hms::hms(extract_seconds(end)),
                      cycle = lubridate::ddays())
    interval <- shush(assign_date(start, end, ambiguity = ambiguity))
    mean <- as.numeric(start) + (as.numeric(interval) / 2)

    hms::hms(mean)
}

extract_seconds <- function(x) {
    classes <- c("Duration", "difftime", "hms", "POSIXct", "POSIXlt",
                 "Interval")

    checkmate::assert_multi_class(x, classes)

    if (lubridate::is.POSIXt(x) || lubridate::is.difftime(x)) {
        as.numeric(hms::as_hms(x))
    } else {
        as.numeric(x)
    }
}

change_day <- function(x, day) {
    classes <- c("Date", "POSIXct", "POSIXlt")

    checkmate::assert_multi_class(x, classes, null.ok = FALSE)
    checkmate::assert_number(day, lower = 1, upper = 31)

    if (any(lubridate::month(x) %in% c(4, 6, 9, 11), na.rm = TRUE)
        && day > 30) {
        cli::cli_abort(paste0(
            "You can't assign more than 30 days to April, June, ",
            "September, or November."
        ))
    }

    if (any(lubridate::month(x) == 2 & !lubridate::leap_year(x)) && day > 28) {
        cli::cli_abort(paste0(
            "You can't assign more than 28 days to February in ",
            "non-leap years."
        ))
    }

    if (any(lubridate::month(x) == 2 & lubridate::leap_year(x), na.rm = TRUE) &&
        day > 29) {
        cli::cli_abort(paste0(
            "You can't assign more than 29 days to February in a leap year."
        ))
    }

    lubridate::day(x) <- day

    x
}

shush <- function(x, quiet = TRUE) {
    if (isTRUE(quiet)) {
        suppressMessages(suppressWarnings(x))
    } else {
        x
    }
}

swap <- function(x, y, condition = TRUE) {
    assert_identical(x, y, type = "class")
    assert_identical(x, y, condition, type = "length")
    checkmate::assert_logical(condition)

    first_arg <- x
    second_arg <- y

    x <- dplyr::if_else(condition, second_arg, first_arg)
    y <- dplyr::if_else(condition, first_arg, second_arg)

    list(x = x, y = y)
}

count_na <- function(x) {
    checkmate::assert_atomic(x)

    length(which(is.na(x)))
}

get_names <- function(...) {
    out <- lapply(substitute(list(...))[-1], deparse) %>%
        vapply(unlist, character(1)) %>%
        noquote()

    gsub("\\\"","", out)
}

get_class <- function(x) {
    if (is.list(x)) {
        vapply(x, function(x) class(x)[1], character(1))
    } else {
        class(x)[1]
    }
}

single_quote_ <- function(x) paste0("'", x, "'")
double_quote_ <- function(x) paste0("\"", x, "\"")
class_collapse <- function(x) single_quote_(paste0(class(x), collapse = "/"))

str_extract_ <- function(string, pattern, ignore_case = FALSE, perl = TRUE,
                         fixed = FALSE, use_bytes = FALSE, invert = FALSE) {
    checkmate::assert_string(pattern)
    checkmate::assert_flag(ignore_case)
    checkmate::assert_flag(perl)
    checkmate::assert_flag(fixed)
    checkmate::assert_flag(use_bytes)
    checkmate::assert_flag(invert)

    match <- regexpr(pattern, string, ignore.case = ignore_case, perl = perl,
                     fixed = fixed, useBytes = use_bytes)
    out <- rep(NA, length(string))
    out[match != -1 & !is.na(match)] <- regmatches(string, match,
                                                   invert = invert)
    out
}

require_pkg <- function(...) {
    out <- list(...)

    lapply(out, checkmate::assert_string,
           pattern = "^[A-Za-z][A-Za-z0-9.]+[A-Za-z0-9]$")

    if (!identical(unique(unlist(out)), unlist(out))) {
        cli::cli_abort("'...' cannot have duplicated values.")
    }

    pkg <- unlist(out)
    namespace <- vapply(pkg, require_namespace, logical(1),
                        quietly = TRUE, USE.NAMES = FALSE)
    pkg <- pkg[!namespace]

    if (length(pkg) == 0) {
        invisible(NULL)
    } else {
        cli::cli_abort(paste0(
            "This function requires the {single_quote_(pkg)} package{?s} ",
             "to run. You can install {?it/them} by running:", "\n\n",
             "install.packages(",
             "{paste(double_quote_(pkg), collapse = ', ')})"
            ))
    }
}
