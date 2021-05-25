test_that("flat_posixct() | general test", {
    x <- lubridate::dmy_hms("17/04/1995 12:00:00")
    force_utc <- TRUE
    base <- "1970-01-01"
    object <- flat_posixt(x, force_utc = force_utc, base = base)
    expect_equal(object, lubridate::ymd_hms("1970-01-01 12:00:00"))

    x <- lubridate::dmy_hms("17/04/1995 12:00:00", tz = "EST")
    force_utc <- FALSE
    base <- "1970-01-01"
    object <- flat_posixt(x, force_utc = force_utc, base = base)
    expect_equal(object, lubridate::ymd_hms("1970-01-01 12:00:00", tz = "EST"))

    x <- lubridate::dmy_hms("17/04/1995 12:00:00", tz = "EST")
    force_utc <- TRUE
    base <- "2000-01-01"
    object <- flat_posixt(x, force_utc = force_utc, base = base)
    expect_equal(object, lubridate::ymd_hms("2000-01-01 12:00:00"))

    # Error test
    expect_error(flat_posixt(1, TRUE, ""))
    expect_error(flat_posixt(lubridate::as_datetime(1), "", ""))
    expect_error(flat_posixt(lubridate::as_datetime(1), "", 1))
})

test_that("midday_change() | general test", {
    expect_equal(midday_change(hms::parse_hm("18:00")),
                 lubridate::ymd_hms("1970-01-01 18:00:00"))
    expect_equal(midday_change(lubridate::ymd_hms("2000-05-04 06:00:00")),
                 lubridate::ymd_hms("1970-01-02 06:00:00"))
    expect_equal(midday_change(c(lubridate::ymd_hms("2020-01-01 18:00:00"),
                                 lubridate::ymd_hms("2020-01-01 06:00:00"))),
                 c(lubridate::ymd_hms("1970-01-01 18:00:00"),
                   lubridate::ymd_hms("1970-01-02 06:00:00")))

    # Assert error test
    expect_error(midday_change(1), "but has class 'numeric'")
})

test_that("change_date() | general test", {
    x <- as.Date("1970-01-01")
    date <- "2000-01-01"
    object <- change_date(x, date)
    expect_equal(object, as.Date("2000-01-01"))

    x <- lubridate::as_datetime(0)
    date <- as.Date("1990-01-01")
    object <- change_date(x, date)
    expect_equal(object, lubridate::ymd_hms("1990-01-01 00:00:00"))

    # Error test
    expect_error(change_date(1, ""))
    expect_error(change_date(as.Date("1970-01-01"), 1))
})

test_that("change_day() | general test", {
    x <- as.Date("1970-01-01")
    day <- 10
    object <- change_day(x, day)
    expect_equal(object, as.Date("1970-01-10"))

    x <- lubridate::as_datetime(0)
    day <- 25
    object <- change_day(x, day)
    expect_equal(object, lubridate::ymd_hms("1970-01-25 00:00:00"))

    # Error test
    expect_error(change_day(1, 1))
    expect_error(change_day(as.Date("1970-01-01"), ""))

    # "You can't assign more than 30 days to April, June, [...]"
    expect_error(change_day(as.Date("1970-04-01"), 31))

    # "You can't assign more than 28 days to February in [...]"
    expect_error(change_day(as.Date("1970-02-01"), 31))

    # "You can't assign more than 29 days to February in a leap year."
    expect_error(change_day(as.Date("1972-02-01"), 31))
})

test_that("is_time() | general test", {
    expect_false(is_time(1))
    expect_false(is_time(letters))
    expect_false(is_time(datasets::iris))

    test <- list(
        "Duration" = lubridate::dhours(),
        "Period" = lubridate::hours(),
        "difftime" = as.difftime(1, units = "secs"),
        "hms" = hms::hms(1),
        "Date" = as.Date("2000-01-01"),
        "POSIXct" = lubridate::as_datetime(1),
        "POSIXlt" = as.POSIXlt(lubridate::as_datetime(1)),
        "Interval" = lubridate::as.interval(lubridate::dhours(),
                                            lubridate::as_datetime(0))
    )

    for (i in names(test)) {
        x <- test[[i]]
        rm <- i
        expect_true(is_time(x))
        expect_false(is_time(x, rm = rm))
    }

    # Error test
    expect_error(is_time(1, rm = 1))
})

test_that("is_numeric_() | general test", {
    expect_false(is_numeric_(lubridate::dhours()))
    expect_false(is_numeric_(letters))
    expect_false(is_numeric_(datasets::iris))

    expect_true(is_numeric_(as.integer(1)))
    expect_true(is_numeric_(as.double(1)))
    expect_true(is_numeric_(as.numeric(1)))
})

test_that("is_whole_number() | general test", {
    expect_false(is_whole_number(lubridate::dhours()))
    expect_false(is_whole_number(letters))
    expect_false(is_whole_number(datasets::iris))

    expect_false(is_whole_number(-1L))
    expect_false(is_whole_number(-55))
    expect_false(is_whole_number(1.58))

    expect_true(is_whole_number(0))
    expect_true(is_whole_number(as.integer(1)))
    expect_true(is_whole_number(as.double(11)))
    expect_true(is_whole_number(as.numeric(475)))
})

test_that("single_quote_() | general test", {
    test <- list("test", 1, lubridate::dhours())

    for (i in test) {
        checkmate::expect_character(single_quote_(i))
        expect_equal(single_quote_(i), paste0("'", i, "'"))
    }
})

test_that("backtick_() | general test", {
    test <- list("test", 1, lubridate::dhours())

    for (i in test) {
        checkmate::expect_character(backtick_(i))
        expect_equal(backtick_(i), paste0("`", i, "`"))
    }
})

test_that("class_collapse() | general test", {
    test <- list("test", 1, lubridate::dhours())

    for (i in test) {
        checkmate::expect_character(class_collapse(i))
        expect_equal(class_collapse(i),
                     single_quote_(paste0(class(i), collapse = "/")))
    }
})

test_that("paste_collapse() | general test", {
    x <- "test"
    expect_equal(paste_collapse(x), x)

    x <- c(1, 2, 3)
    sep <- ", "
    last <- ", and "
    object <- paste_collapse(x, sep = sep, last = last)
    expect_equal(object, "1, 2, and 3")

    # Error test
    expect_error(paste_collapse("", 1, ""))
    expect_error(paste_collapse("", "", 1))
})

test_that("inline_collapse() | general test", {
    x <- "test"
    single_quote <- FALSE
    serial_comma <- FALSE
    object <- inline_collapse(x, single_quote = single_quote,
                              serial_comma = serial_comma)
    expect_equal(object, x)

    x <- "test"
    single_quote <- TRUE
    serial_comma <- FALSE
    object <- inline_collapse(x, single_quote = single_quote,
                              serial_comma = serial_comma)
    expect_equal(object, paste0("'", x, "'"))

    x <- c(1, 2)
    single_quote <- FALSE
    serial_comma <- FALSE
    object <- inline_collapse(x, single_quote = single_quote,
                              serial_comma = serial_comma)
    expect_equal(object, paste0(1, " and ", 2))

    x <- c(1, 2)
    single_quote <- TRUE
    serial_comma <- FALSE
    object <- inline_collapse(x, single_quote = single_quote,
                              serial_comma = serial_comma)
    expect_equal(object, paste0("'1'", " and ", "'2'"))

    x <- c(1, 2, 3)
    single_quote <- TRUE
    serial_comma <- TRUE
    object <- inline_collapse(x, single_quote = single_quote,
                              serial_comma = serial_comma)
    expect_equal(object, paste0("'1'", ", ", "'2'", ", and ", "'3'"))

    # Error test
    expect_error(inline_collapse("", "", TRUE))
    expect_error(inline_collapse("", TRUE, ""))
})

test_that("shush() | general test", {
    expect_equal(shush("test", quiet = FALSE), "test")

    test <- function() {
        warning("test", call. = FALSE)
        "test"
    }

    expect_equal(shush(test(), quiet = TRUE), "test")
    expect_warning(shush(test(), quiet = FALSE))
})

test_that("close_round() | general test", {
    x <- 1.999999
    digits <- 5
    expect_equal(close_round(x, digits = digits), 2)

    x <- 1.000001
    digits <- 5
    expect_equal(close_round(x, digits = digits), 1)

    x <- 1.001
    digits <- 2
    expect_equal(close_round(x, digits = digits), 1)

    x <- c(1.000001, 1.999999)
    digits <- 5
    expect_equal(close_round(x, digits = digits), c(1, 2))

    # Error test
    expect_error(close_round("", 1))
    expect_error(close_round(1, ""))
})

test_that("swap() | general test", {
    x <- 1
    y <- 2
    expect_equal(swap(x, y), list(x = y, y = x))

    x <- 1
    y <- TRUE
    expect_equal(swap(x, y), list(x = TRUE, y = 1))

    x <- c(1, 1)
    y <- ""
    expect_equal(swap(x, y), list(x = "", y = c(1, 1)))
})

test_that("swap_if() | general test", {
    x <- 2
    y <- 1
    condition <- "x > y"
    expect_equal(swap_if(x, y, condition = condition), list(x = y, y = x))

    x <- 1
    y <- 1
    condition <- "x > y"
    expect_equal(swap_if(x, y, condition = condition), list(x = x, y = y))

    # Error test
    expect_error(swap_if(1, 1, ""))
})

test_that("count_na() | general test", {
    x <- c(1, NA, 1, NA)
    expect_equal(count_na(x), 2)
})

test_that("escape_regex() | general test", {
    x <- "test.test"
    expect_equal(escape_regex(x), "test\\.test")
})

test_that("get_names() | general test", {
    object <- get_names(x, y, z)
    expect_equal(object, noquote(c("x", "y", "z")))
})

test_that("get_class() | general test", {
    test <- function(x) class(x)[1]

    expect_equal(get_class(1), "numeric")
    expect_equal(get_class(datasets::iris),
                 vapply(datasets::iris, test, character(1)))
    expect_equal(get_class(list(a = 1, b = 1)),
                 vapply(list(a = 1, b = 1), test, character(1)))
})

test_that("fix_character() | general test", {
    x <- c("1   ", "   1", "", "NA")
    expect_equal(fix_character(x), c("1", "1", NA, NA))

    # Error test
    expect_error(fix_character(1))
})

test_that("str_extract_() | general test", {
    string <- "test123"
    pattern <- "\\d+$"
    perl <- TRUE
    object <- str_extract_(string, pattern, perl = perl)
    expected <- regmatches(string, regexpr(pattern, string, perl = TRUE))
    expect_equal(object, expected)

    string <- "test123"
    pattern <- "^0$"
    perl <- TRUE
    object <- str_extract_(string, pattern, perl = perl)
    expected <- as.character(NA)
    expect_equal(object, expected)

    # Error test (argument check)
    expect_error(str_extract_(1, 1, TRUE, TRUE, TRUE, TRUE, TRUE))
    expect_error(str_extract_(1, TRUE, "", TRUE, TRUE, TRUE, TRUE))
    expect_error(str_extract_(1, TRUE, TRUE, "", TRUE, TRUE, TRUE))
    expect_error(str_extract_(1, TRUE, TRUE, TRUE, "", TRUE, TRUE))
    expect_error(str_extract_(1, TRUE, TRUE, TRUE, TRUE, "", TRUE))
    expect_error(str_extract_(1, TRUE, TRUE, TRUE, TRUE, TRUE, ""))
})

test_that("str_subset_() | general test", {
    string <- month.name
    pattern <- "^J.+"
    perl <- TRUE
    negate <- FALSE
    object <- str_subset_(string, pattern, perl = perl, negate = negate)
    expected <- subset(string, grepl(pattern, string, perl = perl))
    expect_equal(object, expected)

    string <- month.name
    pattern <- "^J.+"
    perl <- TRUE
    negate <- TRUE
    object <- str_subset_(string, pattern, perl = perl, negate = negate)
    expected <- subset(string, !grepl(pattern, string, perl = perl))
    expect_equal(object, expected)

    string <- month.name
    pattern <- "^z$"
    perl <- TRUE
    negate <- FALSE
    object <- str_subset_(string, pattern, perl = perl, negate = negate)
    expected <- as.character(NA)
    expect_equal(object, expected)

    # Error test
    expect_error(str_subset_(1, 1, TRUE, TRUE, TRUE, TRUE, TRUE))
    expect_error(str_subset_(1, TRUE, "", TRUE, TRUE, TRUE, TRUE))
    expect_error(str_subset_(1, TRUE, TRUE, "", TRUE, TRUE, TRUE))
    expect_error(str_subset_(1, TRUE, TRUE, TRUE, "", TRUE, TRUE))
    expect_error(str_subset_(1, TRUE, TRUE, TRUE, TRUE, "", TRUE))
    expect_error(str_subset_(1, TRUE, TRUE, TRUE, TRUE, TRUE, ""))
})

test_that("interval_mean() | general test", {
    start <- hms::parse_hm("22:00")
    end <- hms::parse_hm("06:00")

    object <- interval_mean(start, end)
    expect_equal(object, hms::hms(26 * 3600))

    object <- interval_mean(start, end, class = "Duration")
    expect_equal(object, lubridate::dhours(26))

    object <- interval_mean(start, end, circular = TRUE)
    expect_equal(object, hms::parse_hm("02:00"))

    start <- hms::parse_hm("00:00")
    end <- hms::parse_hm("10:00")
    object <- interval_mean(start, end)
    expect_equal(object, hms::parse_hm("05:00"))

    # Error test
    expect_error(interval_mean(1, hms::hms(1)))
    expect_error(interval_mean(hms::hms(1), 1))
    expect_error(interval_mean(hms::hms(1), hms::hms(1), class = 1))
    expect_error(interval_mean(hms::hms(1), hms::hms(1), ambiguity = 1))
    expect_error(interval_mean(hms::hms(1), hms::hms(1), circular = ""))
})

test_that("package_startup_message() | general test", {
    # is_interactive <- mctq:::is_interactive

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            package_startup_message())
    }

    # mock()
    expect_null(mock(), NULL)

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) FALSE,
            package_startup_message())
    }

    # mock()
    expect_null(mock(), NULL)
})
