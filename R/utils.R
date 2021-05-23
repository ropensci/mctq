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

midday_change <- function(x) {
    checkmate::assert_multi_class(x, c("hms", "POSIXct", "POSIXlt"))

    x <- flat_posixt(convert(x, "POSIXct"))

    x <- dplyr::case_when(
        lubridate::hour(x) < 12 ~ change_day(x, 2),
        TRUE ~ x
    )

    x
}

clock_roll <- function(x) {
    classes <- c("Duration", "Period", "difftime", "hms")
    checkmate::assert_multi_class(x, classes)

    class <- class(x)[1]
    out <- x

    if (class == "difftime") {
        out <- hms::as_hms(x)
        units <- units(x)
    }

    if (all(as.numeric(out) > 0 & as.numeric(out) < 86400, na.rm = TRUE)) {
        x
    } else {
        out <- flat_posixt(lubridate::as_datetime(out))
        out <- convert(out, class, quiet = TRUE)

        if (class == "difftime") {
            units(out) <- units
            out
        } else {
            out
        }
    }
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
    checkmate::assert_multi_class(x, classes, null.ok = FALSE)

    classes <- c("character", "Date")
    checkmate::assert_multi_class(date, classes, null.ok = FALSE)
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

is_time <- function(x, rm = NULL) {
    checkmate::assert_character(rm, any.missing = FALSE, null.ok = TRUE)

    classes <- c("Duration", "Period", "difftime", "hms", "Date", "POSIXct",
                 "POSIXlt", "Interval")

    if (!is.null(rm)) {
        rm <- paste0("^", rm, "$", collapse = "|")
        classes <- str_subset_(classes, rm, negate = TRUE)
    }

    checkmate::test_subset(class(x)[1], classes)
}

is_numeric_ <- function(x) {
    classes <- c("integer", "double", "numeric")
    checkmate::test_subset(class(x)[1], classes)
}

is_whole_number <- function(x, tol = .Machine$double.eps^0.5) {
    if (!is_numeric_(x) || !identical(x, abs(x))) {
        FALSE
    } else {
        abs(x - round(x)) < tol # Example function from `?integer`
    }
}

single_quote_ <- function(x) {
    paste0("'", x, "'")
}

backtick_ <- function(x) {
    paste0("`", x, "`")
}

class_collapse <- function(x) {
    single_quote_(paste0(class(x), collapse = "/"))
}

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

swap <- function(x, y) {
    a <- x
    b <- y

    x <- b
    y <- a

    list(x = x, y = y)
}

swap_if <- function(x, y, condition = "x > y") {
    choices <- c("x == y", "x < y", "x <= y", "x > y", "x >= y")
    checkmate::assert_choice(condition, choices)

    condition <- sub("x", "a", condition)
    condition <- sub("y", "b", condition)

    a <- x
    b <- y

    x <- dplyr::if_else(eval(parse(text = condition)), b, a)
    y <- dplyr::if_else(eval(parse(text = condition)), a, b)

    list(x = x, y = y)
}

count_na <- function(x) {
    length(which(is.na(x)))
}

escape_regex <- function(x) {
    gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)
}

get_names <- function(...) {
    out <- lapply(substitute(list(...))[-1], deparse)
    out <- vapply(out, unlist, character(1))
    out <- noquote(out)
    out <- gsub("\\\"","", out)

    out
}

na_as <- function(x) {
    classes <- c("character", "integer", "double", "numeric", "Duration",
                 "Period", "difftime", "hms", "Date", "POSIXct", "POSIXlt")

    if (is.logical(x)) {
        as.logical(NA)
    } else if (is.character(x)) {
        as.character(NA)
    } else if (is.integer(x)) {
        as.integer(NA)
    } else if (is_numeric_(x)) {
        as.numeric(NA)
    } else if (lubridate::is.duration(x)) {
        lubridate::as.duration(NA)
    } else if (lubridate::is.period(x)) {
        lubridate::as.period(NA)
    } else if (class(x)[1] == "difftime") {
        as.difftime(as.numeric(NA), units = attributes(x)$units)
    } else if (hms::is_hms(x)) {
        hms::as_hms(NA)
    } else if (lubridate::is.Date(x)) {
        as.Date(NA)
    } else if (lubridate::is.POSIXct(x)) {
        out <- as.POSIXct(NA)
        attributes(out)$tzone <- attributes(x)$tzone
        out
    } else if (lubridate::is.POSIXlt(x)) {
        out <- as.POSIXlt(NA)
        attributes(out)$tzone <- attributes(x)$tzone
        out
    } else {
        stop("`na_as()` don't support objects of class ",
             class_collapse(x), ".", call. = FALSE)
    }
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
    out <- rep(NA, length(string))
    out[match != -1 & !is.na(match)] <- regmatches(string, match,
                                                   invert = invert)
    out
}

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
