test_that("flat_posixct() | general test", {
    expect_equal(flat_posixt(
        posixt = lubridate::dmy_hms("17/04/1995 12:00:00"),
        base = as.Date("1970-01-01"), force_tz = TRUE,  tz = "UTC"
    ),
    lubridate::ymd_hms("1970-01-01 12:00:00")
    )

    expect_equal(flat_posixt(
        posixt = lubridate::dmy_hms("17/04/1995 12:00:00", tz = "EST"),
        base = as.Date("1970-01-01"), force_tz = FALSE,  tz = "UTC"
    ),
    lubridate::ymd_hms("1970-01-01 12:00:00", tz = "EST")
    )

    expect_equal(flat_posixt(
        posixt = lubridate::dmy_hms("17/04/1995 12:00:00", tz = "EST"),
        base = as.Date("2000-01-01"), force_tz = TRUE,  tz = "UTC"
    ),
    lubridate::ymd_hms("2000-01-01 12:00:00")
    )
})

test_that("flat_posixct() | error test", {
    # assert_posixt(posixt, null.ok = FALSE)
    expect_error(flat_posixt(
        posixt = 1, base = as.Date("1970-01-01"), force_tz = TRUE, tz = "UTC"
    ),
    "Assertion on 'posixt' failed"
    )

    # checkmate::assert_date(base, len = 1, all.missing = FALSE)
    expect_error(flat_posixt(
        posixt = lubridate::as_datetime(1), base = "", force_tz = TRUE,
        tz = "UTC"
    ),
    "Assertion on 'base' failed"
    )

    expect_error(flat_posixt(
        posixt = lubridate::as_datetime(1),
        base = c(as.Date("1970-01-01"), as.Date("1970-01-01")),
        force_tz = TRUE, tz = "UTC"
    ),
    "Assertion on 'base' failed"
    )

    expect_error(flat_posixt(
        posixt = lubridate::as_datetime(1),  base = as.Date(NA),
        force_tz = TRUE, tz = "UTC"
    ),
    "Assertion on 'base' failed"
    )

    # checkmate::assert_flag(force_tz)
    expect_error(flat_posixt(
        posixt = lubridate::as_datetime(1), base = as.Date("1970-01-01"),
        force_tz = 1, tz = "UTC"
    ),
    "Assertion on 'force_tz' failed"
    )

    # checkmate::assert_choice(tz, OlsonNames())
    expect_error(flat_posixt(
        posixt = lubridate::as_datetime(1), base = as.Date("1970-01-01"),
        force_tz = TRUE, tz = ""
    ),
    "Assertion on 'tz' failed"
    )
})

test_that("midday_change() | general test", {
    expect_equal(midday_change(
        time = hms::parse_hm("18:00")
    ),
    lubridate::ymd_hms("1970-01-01 18:00:00")
    )

    expect_equal(midday_change(
        time = lubridate::ymd_hms("2000-05-04 06:00:00")
    ),
    lubridate::ymd_hms("1970-01-02 06:00:00")
    )

    expect_equal(midday_change(
        time = c(lubridate::ymd_hms("2020-01-01 18:00:00"),
                 lubridate::ymd_hms("2020-01-01 06:00:00")
        )
    ),
    c(lubridate::ymd_hms("1970-01-01 18:00:00"),
      lubridate::ymd_hms("1970-01-02 06:00:00")
    ))
})

test_that("midday_change() | error test", {
    # checkmate::assert_multi_class(time, c("hms", "POSIXct", "POSIXlt"))
    expect_error(midday_change(time = 1), "Assertion on 'time' failed")
})

test_that("interval_mean() | general test", {
    expect_equal(interval_mean(
        start = hms::parse_hm("22:00"), end = hms::parse_hm("06:00"),
        ambiguity = 24
    ),
    hms::hms(26 * 3600)
    )

    expect_equal(interval_mean(
        start = hms::parse_hm("00:00"), end = hms::parse_hm("10:00"),
        ambiguity = 24
    ),
    hms::parse_hm("05:00")
    )
})

test_that("interval_mean() | error test", {
    # checkmate::assert_multi_class(start, classes)
    expect_error(interval_mean(
        start = 1, end = hms::hms(1), ambiguity = 24
    ),
    "Assertion on 'start' failed"
    )

    # checkmate::assert_multi_class(end, classes)
    expect_error(interval_mean(
        start = hms::hms(1), end = 1, ambiguity = 24
    ), "Assertion on 'end' failed"
    )

    # checkmate::assert_choice(ambiguity, c(0, 24 , NA))
    expect_error(interval_mean(
        start = hms::hms(1), end = hms::hms(1), ambiguity = 1
    ),
    "Assertion on 'ambiguity' failed"
    )
})

test_that("extract_seconds() | general test", {
    expect_equal(extract_seconds(x = lubridate::dhours(1)), 3600)
    expect_equal(extract_seconds(x = as.difftime(3600, units = "secs")), 3600)
    expect_equal(extract_seconds(x = hms::hms(3600)), 3600)

    expect_equal(extract_seconds(
        x = as.POSIXct("2020-01-01 01:00:00", tz = "UTC")
    ),
    3600
    )

    expect_equal(extract_seconds(
        x = as.POSIXlt("2020-01-01 01:00:00", tz = "UTC")
    ),
    3600
    )

    expect_equal(extract_seconds(
        x = lubridate::as.interval(lubridate::dhours(1), lubridate::origin)
    ),
    3600
    )
})

test_that("extract_seconds() | error test", {
    # checkmate::assert_multi_class(x, classes)
    expect_error(extract_seconds(x = 1), "Assertion on 'x' failed")
})

test_that("swap() | general test", {
    expect_equal(swap(x = 5, y = 1, condition = TRUE), list(x = 1, y = 5))
    expect_equal(swap(x = 1, y = 5, condition = 1 > 5), list(x = 1, y = 5))
    expect_equal(swap(x = 5, y = 1, condition = 2 > 1), list(x = 1, y = 5))
})

test_that("swap() | error test", {
    # assert_identical(x, y, type = "class")
    expect_error(swap(
        x = 1, y = "a", condition = TRUE
    ))

    # assert_identical(x, y, condition, type = "length")
    expect_error(swap(
        x = 1L, y = 1:2, condition = TRUE
    ))

    # checkmate::assert_logical(condition)
    expect_error(swap(
        x = 1, y = 1, condition = 1
    ),
    "Assertion on 'condition' failed"
    )
})

test_that("class_collapse() | general test", {
    expect_equal(class_collapse(
        x = "test"
    ),
    single_quote_(paste0(class("test"), collapse = "/"))
    )

    expect_equal(class_collapse(
        x = 1
    ),
    single_quote_(paste0(class(1), collapse = "/"))
    )

    expect_equal(class_collapse(
        x = lubridate::dhours()
    ),
    single_quote_(paste0(class(lubridate::dhours()), collapse = "/"))
    )
})

test_that("count_na() | general test", {
    expect_equal(count_na(x = c(1, NA, 1, NA)), 2)
})

test_that("get_class() | general test", {
    expect_equal(get_class(x = 1), "numeric")

    expect_equal(get_class(
        x = datasets::iris
    ),
    vapply(datasets::iris, function(x) class(x)[1], character(1))
    )

    expect_equal(get_class(
        x = list(a = 1, b = 1)
    ),
    vapply(list(a = 1, b = 1), function(x) class(x)[1], character(1))
    )
})

test_that("get_names() | general test", {
    expect_equal(get_names(x, y, z), noquote(c("x", "y", "z")))
})

test_that("single_quote_() | general test", {
    expect_equal(single_quote_(x = "a"), paste0("'", "a", "'"))
    expect_equal(single_quote_(x = 1), paste0("'", 1, "'"))
})

test_that("double_quote_() | general test", {
    expect_equal(double_quote_(x = "a"), paste0("\"", "a", "\""))
    expect_equal(double_quote_(x = 1), paste0("\"", 1, "\""))
})

test_that("str_extract_() | general test", {
    expect_equal(str_extract_(
        string = "test123", pattern = "\\d+$", ignore_case = TRUE, perl = TRUE,
        fixed = FALSE, use_bytes = FALSE, invert = FALSE
    ),
    regmatches("test123", regexpr("\\d+$", "test123", perl = TRUE))
    )

    expect_equal(str_extract_(
        string = "test123", pattern = "^0$", ignore_case = TRUE, perl = TRUE,
        fixed = FALSE, use_bytes = FALSE, invert = FALSE
    ),
    as.character(NA)
    )
})

test_that("str_extract_() | error test", {
    # checkmate::assert_string(pattern)
    expect_error(str_extract_(
        string = 1, pattern = TRUE, ignore_case = TRUE, perl = TRUE,
        fixed = TRUE, use_bytes = TRUE, invert = TRUE
    ),
    "Assertion on 'pattern' failed"
    )

    # checkmate::assert_flag(ignore_case)
    expect_error(str_extract_(
        string = 1, pattern = "a", ignore_case = "", perl = TRUE, fixed = TRUE,
        use_bytes = TRUE, invert = TRUE
    ),
    "Assertion on 'ignore_case' failed"
    )

    # checkmate::assert_flag(perl)
    expect_error(str_extract_(
        string = 1, pattern = "a", ignore_case = TRUE, perl = "", fixed = TRUE,
        use_bytes = TRUE, invert = TRUE
    ),
    "Assertion on 'perl' failed"
    )

    # checkmate::assert_flag(fixed)
    expect_error(str_extract_(
        string = 1, pattern = "a", ignore_case = TRUE, perl = TRUE, fixed = "",
        use_bytes = TRUE, invert = TRUE
    ),
    "Assertion on 'fixed' failed"
    )

    # checkmate::assert_flag(use_bytes)
    expect_error(str_extract_(
        string = 1, pattern = "a", ignore_case = TRUE, perl = TRUE,
        fixed = TRUE,  use_bytes = "", invert = TRUE
    ),
    "Assertion on 'use_bytes' failed"
    )

    # checkmate::assert_flag(invert)
    expect_error(str_extract_(
        string = 1, pattern = "a", ignore_case = TRUE, perl = TRUE,
        fixed = TRUE,  use_bytes = TRUE, invert = ""
    ),
    "Assertion on 'invert' failed"
    )
})

test_that("require_pkg() | general test", {
    expect_null(require_pkg("base"))
    expect_error(require_pkg("test65464564"))
    expect_error(require_pkg("test1654654", "test265464564"))

    mock <- function(.parent = parent.frame(), .env = topenv(.parent)) {
        mockr::with_mock(
            require_namespace = function(...) TRUE,
            {require_pkg("test")}
        )
    }

    expect_null(mock())
})

test_that("require_pkg() | error test", {
    # lapply(out, checkmate::assert_string,
    #        pattern = "^[A-Za-z][A-Za-z0-9.]+[A-Za-z0-9]$")
    expect_error(require_pkg(1), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg(".test"), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg("test."), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg("tes_t"), "Assertion on 'X\\[\\[i\\]\\]' failed")
    expect_error(require_pkg("tést"), "Assertion on 'X\\[\\[i\\]\\]' failed")

    # (!identical(unique(unlist(out)), unlist(out)))
    expect_error(require_pkg(
        "test", "test"
    ),
    "'...' cannot have duplicated values."
    )
})

test_that("shush() | general test", {
    expect_equal(shush(x = "a", quiet = FALSE), "a")

    test <- function() {
        warning("test", call. = FALSE)
        "test"
    }

    expect_equal(shush(x = test(), quiet = TRUE), "test")
    expect_warning(shush(x = test(), quiet = FALSE), "test")
})

