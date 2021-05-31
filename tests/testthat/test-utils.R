test_that("flat_posixct() | general test", {
    expect_equal(flat_posixt(lubridate::dmy_hms("17/04/1995 12:00:00"),
                             TRUE,
                             "1970-01-01"),
                 lubridate::ymd_hms("1970-01-01 12:00:00"))
    expect_equal(flat_posixt(lubridate::dmy_hms("17/04/1995 12:00:00",
                                                tz = "EST"),
                             FALSE,
                             "1970-01-01"),
                 lubridate::ymd_hms("1970-01-01 12:00:00", tz = "EST"))
    expect_equal(flat_posixt(lubridate::dmy_hms("17/04/1995 12:00:00",
                                                tz = "EST"),
                             TRUE,
                             "2000-01-01"),
                 lubridate::ymd_hms("2000-01-01 12:00:00"))
})

test_that("flat_posixct() | error test", {
    expect_error(flat_posixt(1, TRUE, ""), "Assertion on 'posixt' failed")
    expect_error(flat_posixt(lubridate::as_datetime(1), "", ""),
                 "Assertion on 'force_utc' failed")
    expect_error(flat_posixt(lubridate::as_datetime(1), TRUE, 1),
                 "Assertion on 'base' failed")
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
})

test_that("midday_change() | error test", {
    expect_error(midday_change(1), "Assertion on 'time' failed")
})

test_that("interval_mean() | general test", {
    expect_equal(interval_mean(hms::parse_hm("22:00"), hms::parse_hm("06:00")),
                 hms::hms(26 * 3600))
    expect_equal(interval_mean(hms::parse_hm("22:00"), hms::parse_hm("06:00"),
                               class = "Duration"),
                 lubridate::dhours(26))
    expect_equal(interval_mean(hms::parse_hm("22:00"), hms::parse_hm("06:00"),
                               circular = TRUE),
                 hms::parse_hm("02:00"))
    expect_equal(interval_mean(hms::parse_hm("00:00"), hms::parse_hm("10:00")),
                 hms::parse_hm("05:00"))
})

test_that("interval_mean() | error test", {
    expect_error(interval_mean(1, hms::hms(1)), "Assertion on 'start' failed")
    expect_error(interval_mean(hms::hms(1), 1), "Assertion on 'end' failed")
    expect_error(interval_mean(hms::hms(1), hms::hms(1), class = 1),
                 "Assertion on 'tolower\\(class\\)' failed")
    expect_error(interval_mean(hms::hms(1), hms::hms(1), ambiguity = 1),
                 "Assertion on 'ambiguity' failed")
    expect_error(interval_mean(hms::hms(1), hms::hms(1), circular = ""),
                 "Assertion on 'circular' failed")
})

test_that("change_date() | general test", {
    expect_equal(change_date(as.Date("1970-01-01"), "2000-01-01"),
                 as.Date("2000-01-01"))
    expect_equal(change_date(lubridate::as_datetime(0), as.Date("1990-01-01")),
                 lubridate::ymd_hms("1990-01-01 00:00:00"))
})

test_that("change_date() | error test", {
    expect_error(change_date(1, ""), "Assertion on 'x' failed")
    expect_error(change_date(as.Date("1970-01-01"), 1),
                 "Assertion on 'date' failed")
})

test_that("change_day() | general test", {
    expect_equal(change_day(as.Date("1970-01-01"), 10), as.Date("1970-01-10"))
    expect_equal(change_day(lubridate::as_datetime(0), 25),
                 lubridate::ymd_hms("1970-01-25 00:00:00"))
})

test_that("change_day() | general test", {
    expect_error(change_day(1, 1), "Assertion on 'x' failed")
    expect_error(change_day(as.Date("1970-01-01"), ""),
                 "Assertion on 'day' failed")

    expect_error(change_day(as.Date("1970-04-01"), 31),
                 "You can't assign more than 30 days to April, June, ")
    expect_error(change_day(as.Date("1970-02-01"), 31),
                 "You can't assign more than 28 days to February in non-leap ")
    expect_error(change_day(as.Date("1972-02-01"), 31),
                 "You can't assign more than 29 days to February in a leap ")
})

test_that("single_quote_() | general test", {
    expect_equal(single_quote_("test"), paste0("'", "test", "'"))
    expect_equal(single_quote_(1), paste0("'", 1, "'"))
    expect_equal(single_quote_(lubridate::dhours()),
                 paste0("'", lubridate::dhours(), "'"))
})

test_that("double_quote_() | general test", {
    expect_equal(double_quote_("test"), paste0("\"", "test", "\""))
    expect_equal(double_quote_(1), paste0("\"", 1, "\""))
    expect_equal(double_quote_(lubridate::dhours()),
                 paste0("\"", lubridate::dhours(), "\""))
})

test_that("backtick_() | general test", {
    expect_equal(backtick_("test"), paste0("`", "test", "`"))
    expect_equal(backtick_(1), paste0("`", 1, "`"))
    expect_equal(backtick_(lubridate::dhours()),
                 paste0("`", lubridate::dhours(), "`"))
})

test_that("class_collapse() | general test", {
    expect_equal(class_collapse("test"),
                 single_quote_(paste0(class("test"), collapse = "/")))
    expect_equal(class_collapse(1),
                 single_quote_(paste0(class(1), collapse = "/")))
    expect_equal(class_collapse(lubridate::dhours()),
                 single_quote_(paste0(class(lubridate::dhours()),
                                      collapse = "/")))
})

test_that("paste_collapse() | general test", {
    expect_equal(paste_collapse("test"), "test")
    expect_equal(paste_collapse(c(1, 2, 3), sep = ", ", last = ", and "),
                 "1, 2, and 3")
})

test_that("paste_collapse() | error test", {
    expect_error(paste_collapse("", 1, ""), "Assertion on 'sep' failed")
    expect_error(paste_collapse("", "", 1), "Assertion on 'last' failed")
})

test_that("inline_collapse() | general test", {
    expect_equal(inline_collapse("test", FALSE, FALSE), "test")
    expect_equal(inline_collapse("test", TRUE, FALSE),
                 paste0("'", "test", "'"))
    expect_equal(inline_collapse(c(1, 2), FALSE, FALSE),
                 paste0(1, " and ", 2))
    expect_equal(inline_collapse(c(1, 2), TRUE, FALSE),
                 paste0("'1'", " and ", "'2'"))
    expect_equal(inline_collapse(c(1, 2, 3), TRUE, TRUE),
                 paste0("'1'", ", ", "'2'", ", and ", "'3'"))
})

test_that("inline_collapse() | error test", {
    expect_error(inline_collapse("", "", TRUE),
                 "Assertion on 'single_quote' failed")
    expect_error(inline_collapse("", TRUE, ""),
                 "Assertion on 'serial_comma' failed")
})

test_that("shush() | general test", {
    expect_equal(shush("test", quiet = FALSE), "test")

    test <- function() {
        warning("test", call. = FALSE)
        "test"
    }

    expect_equal(shush(test(), quiet = TRUE), "test")
    expect_warning(shush(test(), quiet = FALSE), "test")
})

test_that("close_round() | general test", {
    expect_equal(close_round(1.999999, 5), 2)
    expect_equal(close_round(1.000001, 5), 1)
    expect_equal(close_round(1.001, 2), 1)
    expect_equal(close_round(1.0001, 5), 1.0001)
    expect_equal(close_round(c(1.000001, 1.999999, 1.11), 5),
                 c(1, 2, 1.11))
})

test_that("close_round() | error test", {
    expect_error(close_round("", 1), "Assertion on 'x' failed")
    expect_error(close_round(1, ""), "Assertion on 'digits' failed")
})

test_that("swap() | general test", {
    expect_equal(swap(5, 1), list(x = 1, y = 5))
    expect_equal(swap(1, 5, 1 > 5), list(x = 1, y = 5))
    expect_equal(swap(5, 1, 2 > 1), list(x = 1, y = 5))
})

test_that("swap() | error test", {
    expect_error(swap(1, 1, 1), "Assertion on 'condition' failed")
})

test_that("count_na() | general test", {
    expect_equal(count_na(c(1, NA, 1, NA)), 2)
})

test_that("escape_regex() | general test", {
    expect_equal(escape_regex("test.test"), "test\\.test")
})

test_that("get_names() | general test", {
    expect_equal(get_names(x, y, z), noquote(c("x", "y", "z")))
})

test_that("get_class() | general test", {
    expect_equal(get_class(1), "numeric")
    expect_equal(get_class(datasets::iris),
                 vapply(datasets::iris, function(x) class(x)[1], character(1)))
    expect_equal(get_class(list(a = 1, b = 1)),
                 vapply(list(a = 1, b = 1), function(x) class(x)[1],
                        character(1)))
})

test_that("fix_character() | general test", {
    expect_equal(fix_character(c("1   ", "   1", "", "NA")),
                 c("1", "1", NA, NA))
})

test_that("fix_character() | error test", {
    expect_error(fix_character(1), "Assertion on 'x' failed")
})

test_that("str_extract_() | general test", {
    expect_equal(str_extract_("test123", "\\d+$", TRUE),
                 regmatches("test123", regexpr("\\d+$", "test123",
                                               perl = TRUE)))
    expect_equal(str_extract_("test123", "^0$", TRUE), as.character(NA))
})

test_that("str_extract_() | error test", {
    expect_error(str_extract_(1, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
                 "Assertion on 'pattern' failed")
    expect_error(str_extract_(1, "a", "", TRUE, TRUE, TRUE, TRUE),
                 "Assertion on 'ignore_case' failed")
    expect_error(str_extract_(1, "a", TRUE, "", TRUE, TRUE, TRUE),
                 "Assertion on 'perl' failed")
    expect_error(str_extract_(1, "a", TRUE, TRUE, "", TRUE, TRUE),
                 "Assertion on 'fixed' failed")
    expect_error(str_extract_(1, "a", TRUE, TRUE, TRUE, "", TRUE),
                 "Assertion on 'use_bytes' failed")
    expect_error(str_extract_(1, "a", TRUE, TRUE, TRUE, TRUE, ""),
                 "Assertion on 'invert' failed")
})

test_that("str_subset_() | general test", {
    expect_equal(str_subset_(month.name, "^J.+", perl = TRUE, negate = FALSE),
                 subset(month.name, grepl("^J.+", month.name, perl = TRUE)))
    expect_equal(str_subset_(month.name, "^J.+", perl = TRUE, negate = TRUE),
                 subset(month.name, !grepl("^J.+", month.name, perl = TRUE)))
    expect_equal(str_subset_(month.name, "^z$", perl = TRUE, negate = FALSE),
                 as.character(NA))
})

test_that("str_subset_() | error test", {
    expect_error(str_subset_(1, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
                 "Assertion on 'pattern' failed")
    expect_error(str_subset_(1, "a", "", TRUE, TRUE, TRUE, TRUE),
                 "Assertion on 'negate' failed")
    expect_error(str_subset_(1, "a", TRUE, "", TRUE, TRUE, TRUE),
                 "Assertion on 'ignore_case' failed")
    expect_error(str_subset_(1, "a", TRUE, TRUE, "", TRUE, TRUE),
                 "Assertion on 'perl' failed")
    expect_error(str_subset_(1, "a", TRUE, TRUE, TRUE, "", TRUE),
                 "Assertion on 'fixed' failed")
    expect_error(str_subset_(1, "a", TRUE, TRUE, TRUE, TRUE, ""),
                 "Assertion on 'use_bytes' failed")
})

test_that("package_startup_message() | general test", {
    # ## Don't forget to run devtools::load_all(".") and uncomment the variables
    # ## before trying to run the tests interactively.
    #
    # is_interactive <- mctq:::is_interactive

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) TRUE,
            package_startup_message())
    }

    # mock()
    expect_null(mock())

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            is_interactive = function(...) FALSE,
            package_startup_message())
    }

    # mock()
    expect_null(mock())
})

test_that("require_pkg() | general test", {
    expect_null(require_pkg("base"))
    expect_error(require_pkg("test"),
                 "This function requires the 'test' package to run. ")
    expect_error(require_pkg("test1", "test2"),
                 "This function requires the 'test1' and 'test2' packages ")

    # ## Don't forget to run devtools::load_all(".") and uncomment the variables
    # ## before trying to run the tests interactively.
    #
    # require_namespace <- mctq:::require_namespace

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            require_namespace = function(...) TRUE,
            require_pkg("test"))
    }

    # mock()
    expect_null(mock())
})

test_that("require_pkg() | error test", {
    expect_error(require_pkg(1), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg(".test"), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg("test."), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg("tes_t"), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg("tést"), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg("test", "test"),
                 "'...' cannot have duplicated values.")
})
