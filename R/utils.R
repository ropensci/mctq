#' Transform a MCTQ dataset
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `pretty_mctq()` helps you to transform your MCTQ data in many ways. See
#' parameters to learn more.
#'
#' @param data A data frame.
#' @param round (optional) a `logical` value indicating if `Duration`, `Period`,
#'   `difftime`, and `hms` objects must be rounded at the level of seconds
#'   (default: `TRUE`).
#' @param hms (optional) a `logical` value indicating if `Duration`, `Period`,
#'   and `difftime` objects must be converted to `hms` (default: `TRUE`).
#'
#' @return A transformed data frame, as indicated in parameters.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' \dontrun{
#' pretty_mctq(mctq:::analyze_std_mctq(round = FALSE, hms = FALSE))
#' }
pretty_mctq <- function(data, round = TRUE, hms = TRUE) {

    where <- NULL # R CMD Check variable bindings fix

    test <- function(x) {
        classes <- c("Duration", "Period", "difftime", "hms")

        checkmate::test_multi_class(x, classes)
    }

    if (isTRUE(round)) {
        data <- data %>% dplyr::mutate(dplyr::across(where(test), round_time))
    }

    if (isTRUE(hms)) {
        data <- convert_to(data, "hms", where = test)
    }

    data

}

#' Return a model MCTQ data
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `mctq` package comes bundled with fictional datasets for different versions
#' of the Munich Chronotype Questionnaire (mctq standard, mctq shift, and
#' \strong{\eqn{\mu}}mctq). `model_data()` make it easy to access them.
#'
#' At the moment, __only the standard MCTQ is available__.
#'
#' @param model A string indicating the data model to return. Valid values are:
#'   `"standard"`, "`shift"`, `"micro"`,  (default: `"standard"`).
#'
#' @return An invisible tibble with a MCTQ model data.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' \dontrun{
#' data <- model_data()
#' }
model_data <- function(model = "standard") {

    model <- stringr::str_to_lower(model)
    checkmate::assert_choice(model, c("std", "standard", "shift", "micro"))

    if (model %in% c("std", "standard")) {
        invisible(mctq::std_mctq)
    } else if (model == "shift") {
        NA # invisible(mctq::mctq_shift)
    } else if (model == "micro") {
        NA # invisible(mctq::micro_mctq)
    } else {
        rlang::abort("Critical error")
    }

}

#' Get paths to `mctq` raw datasets
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `mctq` comes bundled with raw fictional datasets for testing and learning.
#' `raw_data()` make it easy to access their paths.
#'
#' @param file A string indicating the file name of the raw dataset. If `NULL`,
#'   all raw dataset file names will be listed (default: `NULL`).
#'
#' @return If `path = NULL`, returns a character vector with all raw dataset
#'   file names available. Else, returns the `file` path.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' \dontrun{
#' raw_data()
#' raw_data(raw_data()[1])
#' raw_data("std_mctq.csv")
#' }
raw_data <- function(file = NULL) {

    checkmate::assert_string(file, null.ok = TRUE)

    if (is.null(file)) {
        dir(system.file("extdata", package = "mctq"))
    } else {
        system.file("extdata", file, package = "mctq", mustWork = TRUE)
    }

}

#' Change dates by time of day
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `midday_change()` changes the dates of `POSIXt` objects accordingly to the
#' time of day registered in the object values. The function do this by flatting
#' the date to `1970-01-01` and them adding a day if the hour is lower than 12.
#'
#' `hms` objects are also allowed. In this case `midday_change()` will convert
#' all values to `POSIXct` and do the same operations described above.
#'
#' This method can help with time charting, allowing a continuity after the
#' midnight hour.
#'
#' `mdc()` is just a shorter version of `midday_change()` for convenience.
#'
#' @param x A `hms` or `POSIXt` object.
#'
#' @return A `POSIXct`object when `x` is `hms` or `POSIXct`, or a `POSIXlt`
#'   object when `x` is `POSIXlt`.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' x <- lubridate::ymd_hms("2021-01-15 20:02:01") # hour > 12h
#' midday_change(x)
#' #> [1] "1970-01-01 20:02:01 UTC" # Expected
#'
#' x <- lubridate::ymd_hms("1987-12-24 07:45:32") # hour < 12h
#' midday_change(x)
#' #> [1] "1970-01-02 07:45:32 UTC" # Expected
midday_change = function(x) {

    checkmate::assert_multi_class(x, c("hms", "POSIXct", "POSIXlt"))

    x <- flat_posixt(convert_to(x, "posixct"))

    x <- dplyr::case_when(
        lubridate::hour(x) < 12 ~ change_day(x, 2),
        TRUE ~ x
    )

    x

}

#' @rdname midday_change
#' @noRd
mdc <- function(x) midday_change(x)

#' Flat dates of `POSIXt` objects
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `flat_posixt()` changes the dates of `POSIXt` objects to a base reference.
#' This can be use to standardizing a point of origin to time values.
#'
#' Please note that, accordingly to `?base::POSIXct`, not all OSes can
#' correctly convert times before 1902 or after 2037. It's best to maintain the
#' base date as the [UNIX epoch](https://en.wikipedia.org/wiki/Unix_time),
#' _i.e_ `"1970-01-01"` (default).
#'
#' @param x A `POSIXt` object.
#' @param force_utc (optional) a `logical` value indicating if the time zone of
#'   `x` must be forced to `"UTC"` (default: `TRUE`).
#' @param base (optional) a string indicating the base date for flatting `x`
#'   in `%Y-%m-%d` format (default: `"1970-01-01"`).
#'
#' @return An object of the same `POSIXt` class type as `x` with dates and time
#'   zone transformed.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' x <- lubridate::ymd_hms("2001-09-15 11:15:05", tz = "EST")
#' flat_posixt(x)
#' #> [1] "1970-01-01 11:15:05 UTC" # Expected
#' flat_posixt(x, force_utc = FALSE)
#' #> [1] "1970-01-01 11:15:05 EST" # Expected
#' flat_posixt(x, base = "2020-01-01")
#' #> [1] "2020-01-01 11:15:05 UTC"
flat_posixt = function(x, force_utc = TRUE, base = "1970-01-01") {

    assert_posixt(x, null.ok = FALSE)
    checkmate::assert_flag(force_utc)
    checkmate::assert_string(base)

    lubridate::date(x) <- base

    if (isTRUE(force_utc)) {
        x <- lubridate::force_tz(x, "UTC")
    }

    x

}

#' Change date of `Date` or `POSIXt` objects
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `change_date()` help you change dates of `Date` or `POSIXt` objects without
#' the need for a direct assignment.
#'
#' @param x A `Date` or `POSIXt` object.
#' @param date A `Date` or `character` object of length 1 indicating the date
#'   for `x`.
#'
#' @return An object of the same class as `x` with the indicated date in `date`.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' change_date(lubridate::as_date("1888-02-07"), "2020-01-01")
#' #> [1] "2020-01-01" # Expected
#' change_date(lubridate::ymd_hms("1987-12-24 07:45:32"), as.Date("1999-02-25"))
#' #> [1] "1999-02-25 07:45:32 UTC" # Expected
change_date <- function(x, date) {

    classes <- c("Date", "POSIXct", "POSIXlt")
    checkmate::assert_multi_class(x, classes, null.ok = FALSE)

    classes <- c("character", "Date")
    checkmate::assert_multi_class(date, classes, null.ok = FALSE)
    assert_length_one(date)

    lubridate::date(x) <- date

    x

}

#' Change day of `Date` or `POSIXt` objects
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
#'
#' `change_day()` help you change days of `Date` or `POSIXt` objects without the
#' need for a direct assignment.
#'
#' @param x A `Date` or `POSIXt` object.
#' @param day A number between 1-31 indicating the new day for `x`.
#'
#' @return An object of the same class as `x` with the indicated day in `day`.
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

    if (any(lubridate::month(x) %in% c(4, 6, 9, 11)) && day > 30) {
        rlang::abort(paste0(
            "You can't assign more than 30 days to April, June, ",
            "September, or November."
            ))
    }

    if (any(lubridate::month(x) == 2 & !lubridate::leap_year(x)) && day > 28) {
        rlang::abort(paste0(
            "You can't assign more than 28 days to February in ",
            "non-leap years."
        ))
    }

    if (any(lubridate::month(x) == 2 & lubridate::leap_year(x)) && day > 29) {
        rlang::abort(paste0(
            "You can't assign more than 29 days to February in a leap year"
        ))
    }

    lubridate::day(x) <- day

    x

}

#' Test if an object inherits a class from a set of date/time classes
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' `is_time()` returns a boolean flag testing for objects of class `Duration`,
#' `Period`, `difftime`, `hms`, `Date`, `POSIXct`, `POSIXlt`, or `Interval`.
#'
#' @param x Any kind of R object.
#' @param rm (optional) a character vector indicating names of object classes to
#'   remove from the test (case sensitive) (default: `NULL`).
#'
#' @return If `rm` is `NULL`, a boolean flag checking if `x` inherits a
#'   `Duration`, `Period`, `difftime`, `hms`, `Date`, `POSIXct`, `POSIXlt`, or
#'   `Interval` class. Else, the same as previous, but without the classes
#'   indicated in `rm`.
#'
#' @family utility functions
#' @noRd
#'
#' @examples
#' is_time(as.Date("2020-01-01"))
#' #> [1] TRUE # Expected
#' is_time(as.Date("2020-01-01"), rm = "Date")
#' #> [1] FALSE # Expected
#' is_time(datasets::iris)
#' #> [1] FALSE # Expected
is_time <- function(x, rm = NULL) {

    checkmate::assert_character(rm, any.missing = FALSE, null.ok = TRUE)

    classes <- c("difftime", "Duration", "hms", "Period", "Date", "POSIXct",
                 "POSIXlt", "Interval")

    if (!is.null(rm)) {
        rm <- paste0("^", rm, "$", collapse = "|")
        classes <- stringr::str_subset(classes, rm, negate = TRUE)
    }

    # if (circular::is.circular(x) && !("circular" %in% rm)) {
    #     return(TRUE)
    # }

    checkmate::test_multi_class(x, classes)

}

#' Collapse class names
#'
#' @description
#'
#' `r lifecycle::badge("experimental")`
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
#' class_collapse(hms::parse_hm("00:00"))
#' #> [1] "'hms/difftime'" # Expected
class_collapse <- function(x) {

    glue::single_quote(glue::glue_collapse(class(x), sep = '/'))

}

#' @family utility functions
#' @noRd
hms_interval <- function(start, end, tz = "UTC") {

    checkmate::check_multi_class(start, c("hms", "POSIXct", "POSIXlt"))
    checkmate::check_multi_class(end, c("hms", "POSIXct", "POSIXlt"))

    start <- flat_posixt(convert_to(start, "posixct", tz = tz), FALSE)
    end <- flat_posixt(convert_to(end, "posixct", tz = tz), FALSE)

    lubridate::interval(start, end)

}

#' @family utility functions
#' @noRd
is_numeric_ <- function(x) {

    any(class(x) %in% c("integer", "double", "numeric"))

}

#' @family utility functions
#' @noRd
inline_collapse <- function(x, single_quote = TRUE, serial_comma = TRUE) {

    checkmate::assert_character(x)
    checkmate::assert_flag(single_quote)
    checkmate::assert_flag(serial_comma)

    if (isTRUE(single_quote)) x <- glue::single_quote(x)

    if (length(x) <= 2 || isFALSE(serial_comma)) {
        glue::glue_collapse(x, sep = ", ", last = " and ")
    } else {
        glue::glue_collapse(x, sep = ", ", last = ", and ")
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

#' @family utility functions
#' @noRd
get_names <- function(...) {

    out <- lapply(substitute(list(...))[-1], deparse)
    out <- sapply(out, unlist)
    out <- noquote(out)
    out <- gsub("\\\"","", out)

    out

}

#' @family utility functions
#' @noRd
check_that_ <- function(data, ...) {

    checkmate::assert_data_frame(data)

    confront <- validate::check_that(data, ...)
    validate::summary(confront)

}

#' @family utility functions
#' @noRd
sample_time <- function(class = "hms", min = hms::parse_hms("00:00:00"),
                        max = hms::parse_hms("23:59:59"),
                        by = lubridate::dminutes(5), size = 1,
                        replace = FALSE, prob = NULL) {

    classes <- c("Duration", "Period", "difftime", "hms", "integer", "numeric")

    checkmate::assert_choice(tolower(class), tolower(classes))
    checkmate::assert_multi_class(min, classes)
    checkmate::assert_multi_class(max, classes)
    checkmate::assert_multi_class(by, classes)
    assert_length_one(min)
    assert_length_one(max)
    assert_length_one(by)
    checkmate::assert_flag(replace)
    checkmate::assert_number(size, lower = 0)
    checkmate::assert_numeric(prob, null.ok = TRUE)

    min <- as.numeric(min)
    max <- as.numeric(max)
    by <- as.numeric(by)

    if (size > length(seq(min, max, by)) && isFALSE(replace)) {
        rlang::abort("You cannot take a sample larger than the population ",
                     "when 'replace = FALSE'.")
    }

    sample <- sample(seq(min, max, by), size = size, replace = replace,
                     prob = prob)

    convert_to(sample, class)

}

#' @family utility functions
#' @noRd
clock_roll <- function(x, class = "hms") {

    out <- flat_posixt(lubridate::as_datetime(x))
    convert_to(out, class)

}

#' @family utility functions
#' @noRd
na_as <- function(x) {

    classes <- c("character", "integer", "double", "numeric", "Duration",
                 "Period", "difftime", "hms", "Date", "POSIXct", "POSIXlt")

    if (is.logical(x)) {
        as.logical(NA)
    } else if (checkmate::test_multi_class(x, classes)) {
        convert_to(NA, class(x)[1])
    } else {
        rlang::abort(glue::glue(
            "`na_as()` don't support objects of class {class_collapse(x)}."
            ))
    }

}

#' @family utility functions
#' @noRd
get_class <- function(x) {

    foo <- function(x) {
        class(x)[1]
    }

    if (is.list(x) || is.data.frame(x)) {
        sapply(x, foo)
    } else {
        class(x)[1]
    }

}
