flat_posixt <- function(posixt, force_utc = TRUE, base = "1970-01-01") {
    assert_posixt(posixt, null.ok = FALSE)
    checkmate::assert_flag(force_utc)
    checkmate::assert_string(base, pattern = "\\d{4}-\\d{2}-\\d{2}")

    lubridate::date(posixt) <- base

    if (isTRUE(force_utc)) {
        lubridate::force_tz(posixt, "UTC")
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

interval_mean <- function(start, end, class = "hms", ambiguity = 24,
                          circular = FALSE) {
    classes <- c("Duration", "Period", "difftime", "hms", "POSIXct", "POSIXlt")

    checkmate::assert_multi_class(start, classes)
    checkmate::assert_multi_class(end, classes)

    classes <- c("character", "integer", "double", "numeric", "Duration",
                 "Period", "difftime", "hms", "POSIXct", "POSIXlt")

    checkmate::assert_choice(tolower(class), tolower(classes))
    checkmate::assert_choice(ambiguity, c(0, 24 , NA))
    checkmate::assert_flag(circular)

    start <- clock_roll(convert(start, "hms", quiet = TRUE))
    end <- clock_roll(convert(end, "hms", quiet = TRUE))
    interval <- shush(assign_date(start, end, ambiguity = ambiguity))
    mean <- as.numeric(start) + (as.numeric(interval) / 2)

    if (isTRUE(circular)) {
        convert(hms::as_hms(lubridate::as_datetime(mean)), class, quiet = TRUE)
    } else {
        convert(mean, class, quiet = TRUE)
    }
}

change_date <- function(x, date) {
    classes <- c("Date", "POSIXct", "POSIXlt")
    checkmate::assert_multi_class(x, classes)

    classes <- c("character", "Date")
    checkmate::assert_multi_class(date, classes)
    assert_length_one(date)

    lubridate::date(x) <- date

    x
}

change_day <- function(x, day) {
    classes <- c("Date", "POSIXct", "POSIXlt")

    checkmate::assert_multi_class(x, classes, null.ok = FALSE)
    checkmate::assert_number(day, lower = 1, upper = 31)

    if (any(lubridate::month(x) %in% c(4, 6, 9, 11), na.rm = TRUE)
        && day > 30) {
        stop("You can't assign more than 30 days to April, June, ",
             "September, or November.", call. = FALSE)
    }

    if (any(lubridate::month(x) == 2 & !lubridate::leap_year(x)) && day > 28) {
        stop("You can't assign more than 28 days to February in ",
             "non-leap years.", call. = FALSE)
    }

    if (any(lubridate::month(x) == 2 & lubridate::leap_year(x), na.rm = TRUE) &&
        day > 29) {
        stop("You can't assign more than 29 days to February in a leap year.",
             call. = FALSE)
    }

    lubridate::day(x) <- day

    x
}

single_quote_ <- function(x) paste0("'", x, "'")
double_quote_ <- function(x) paste0("\"", x, "\"")
backtick_ <- function(x) paste0("`", x, "`")
class_collapse <- function(x) single_quote_(paste0(class(x), collapse = "/"))

paste_collapse <- function(x, sep = "", last = sep) {
    checkmate::assert_string(sep)
    checkmate::assert_string(last)

    if (length(x) == 1) {
        x
    } else {
        paste0(paste(x[-length(x)], collapse = sep), last, x[length(x)])
    }
}

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

shush <- function(x, quiet = TRUE) {
    if (isTRUE(quiet)) {
        suppressMessages(suppressWarnings(x))
    } else {
        x
    }
}

close_round <- function(x, digits = 3) {
    checkmate::assert_numeric(x)
    checkmate::assert_number(digits)

    pattern_9 <- paste0("\\.", paste(rep(9, digits), collapse = ""))
    pattern_0 <- paste0("\\.", paste(rep(0, digits), collapse = ""))

    dplyr::case_when(
        grepl(pattern_9, x) | grepl(pattern_0, x) ~ round(x),
        TRUE ~ x)
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

count_na <- function(x) length(which(is.na(x)))
escape_regex <- function(x) gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)

get_names <- function(...) {
    out <- lapply(substitute(list(...))[-1], deparse)
    out <- vapply(out, unlist, character(1))
    out <- noquote(out)
    out <- gsub("\\\"","", out)

    out
}

get_class <- function(x) {
    if (is.list(x)) {
        vapply(x, function(x) class(x)[1], character(1))
    } else {
        class(x)[1]
    }
}

fix_character <- function(x) {
    checkmate::assert_character(x)

    x <- trimws(x)

    for (i in c("", "NA")) {
        x <- dplyr::na_if(x, i)
    }

    x
}

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

str_subset_ <- function(string, pattern, negate = FALSE, ignore_case = FALSE,
                        perl = TRUE, fixed = FALSE, use_bytes = FALSE) {
    checkmate::assert_string(pattern)
    checkmate::assert_flag(negate)
    checkmate::assert_flag(ignore_case)
    checkmate::assert_flag(perl)
    checkmate::assert_flag(fixed)
    checkmate::assert_flag(use_bytes)

    match <- grepl(pattern, string, ignore.case = ignore_case, perl = perl,
                   fixed = fixed, useBytes = use_bytes)

    if (isTRUE(negate)) {
        out <- subset(string, !match)
    } else {
        out <- subset(string, match)
    }

    if (length(out) == 0) as.character(NA) else out
}

package_startup_message <- function(..., domain = NULL, appendLF = TRUE) {
    if (is_interactive()) {
        packageStartupMessage(..., domain = domain, appendLF = appendLF)
    }

    invisible(NULL)
}

require_pkg <- function(...) {
    out <- list(...)

    lapply(out, checkmate::assert_string,
           pattern = "^[A-Za-z][A-Za-z0-9.]+[A-Za-z0-9]$")

    if (!identical(unique(unlist(out)), unlist(out))) {
        stop("'...' cannot have duplicated values.", call. = FALSE)
    }

    pkg <- unlist(out)
    namespace <- vapply(pkg, require_namespace, logical(1),
                        quietly = TRUE, USE.NAMES = FALSE)
    pkg <- pkg[!namespace]

    if (length(pkg) == 0) {
        invisible(NULL)
    } else {
        stop("This function requires the ", inline_collapse(pkg), " ",
             ifelse(length(pkg) == 1, "package", "packages"), " ",
             "to run. You can install ",
             ifelse(length(pkg) == 1, "it", "them"), " ",
             "by running: \n\n",
             "install.packages(",
             paste(double_quote_(pkg), collapse = ", "), ")",
             call. = FALSE)
    }
}
