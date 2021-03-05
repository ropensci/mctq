test_that("convert() | general test", {
    expect_equal(convert(NA, "character", quiet = TRUE), NA)
})

test_that("convert.Period() | general test", {
    x <- lubridate::hours()
    expect_equal(convert(x, "character", quiet = TRUE), "01:00:00")
})

test_that("convert.difftime() | general test", {
    x <- as.difftime(1, units = "hours")
    expect_equal(convert(x, "character", quiet = TRUE), "01:00:00")
})

test_that("convert_tu() | general test", {
    expect_equal(convert_tu(lubridate::dhours(), "M", quiet = TRUE), 60)
})

test_that("convert_ut() | general test", {
    expected <- hms::parse_hm("01:00")
    expect_equal(convert_ut(1, "hms", "H", quiet = TRUE), expected)
})

test_that("convert_uu() | general test", {
    expect_equal(convert_uu(1, "rad", "H", quiet = TRUE), (12 / pi))
})

test_that("convert_pt() | general test", {
    expected <- hms::parse_hm("01:00")
    expect_equal(convert_pt(1, "hms", "H", quiet = TRUE), expected)
})

test_that("convert_pu() | general test", {
    expect_equal(convert_pu(1, "H", "deg", quiet = TRUE), 15)
})

test_that("parser_1() | general test", {
    object <- parser_1(" 1 ", "duration", input_unit = "H", quiet = TRUE)
    expect_equal(object, lubridate::dhours())

    object <- parser_1(1, "double", input_unit = "H", output_unit = "deg",
                       quiet = TRUE)
    expect_equal(object, 15)

    object <- parser_1(1, "hms", input_unit = "H", quiet = TRUE)
    expect_equal(object, hms::parse_hm("01:00"))
})

test_that("parser_1() | error test", {
    # "To convert 'character' objects to units, all values must be [...]"
    expect_error(parser_1(c("1", " NA", "a"), "hms"))

    # "'x' can only be converted to 'output_unit' if 'input_unit' [...]"
    expect_error(parser_1(1, "hms", output_unit = "deg"))

    # "'x' can only be converted to 'output_unit' if 'class' [...]"
    expect_error(parser_1(1, "hms", input_unit = "H",  output_unit = "rad"))

    # "input_unit' and 'output_unit' must both be assigned, or be [...]"
    expect_error(parser_1(1, "integer"))
})

test_that("parser_2() | general test", {
    object <- parser_2(" 1 ", "duration", orders = "H", quiet = TRUE)
    expect_equal(object, lubridate::dhours())

    object <- parser_2(1, "integer", orders = "H", output_unit = "deg",
                       quiet = TRUE)
    expect_equal(object, 15L)

    object <- parser_2(1, "numeric", orders = "H", output_unit = "rad",
                       quiet = TRUE)
    expect_equal(object, (2 * pi) / 24)

    object <- parser_2(1, "hms", orders = "H", quiet = TRUE)
    expect_equal(object, hms::parse_hm("01:00:00"))
})

test_that("parser_3() | general test", {
    object <- parser_3(lubridate::dhours(), "integer", output_unit = "H",
                       quiet = TRUE)
    expect_equal(object, 1L)

    object <- parser_3(lubridate::dhours(), "numeric", output_unit = "M",
                       quiet = TRUE)
    expect_equal(object, 60)
})

test_that("parser_3() | error test", {
    # "'x' can be only be converted to 'output_unit' if 'class' [...]"
    expect_error(parser_3(lubridate::dhours(), "hms"))
})

test_that("parse_to_date_time() | general test", {
    expect_equal(parse_to_date_time(" 1", "H"), hms::parse_hm("01:00"))
    expect_equal(parse_to_date_time("NA", "H"), lubridate::as_datetime(NA))
    expect_equal(parse_to_date_time(1, "H"), hms::parse_hm("01:00"))
    expect_equal(parse_to_date_time("01:59", "HM"), hms::parse_hm("01:59"))

    expected <- hms::parse_hms("01:59:59")
    expect_equal(parse_to_date_time("01:59:59", "HMS"), expected)

    expected <- lubridate::ymd_hms("0000-01-01 01:00:59")
    expect_equal(parse_to_date_time("01:59", "HS"), expected)

    expected <- lubridate::ymd_hms("2000-01-01 01:00:00")
    expect_equal(parse_to_date_time("2000 1", "YH"), expected)
})

test_that("parse_to_date_time() | warning test", {
    # "All formats failed to parse. No formats found."
    expect_warning(parse_to_date_time("a", "H", quiet = FALSE))

    # na_diff, " failed to parse."
    expect_warning(parse_to_date_time(c("1", "a"), "H", quiet = FALSE))
})

test_that("parse_to_date_time() | error test", {
    # Invalid values for `x`, `orders`, `tz`, and `quiet`
    expect_error(parse_to_date_time(lubridate::dhours()))
    expect_error(parse_to_date_time(1, orders = 1))
    expect_error(parse_to_date_time(1, tz = 1))
    expect_error(parse_to_date_time(x, quiet = ""))
})

test_that("convert_to_seconds() | general test", {
    month_length <- 30 * 24 * 60 * 60
    year_length <- month_length * 12

    # if (any(class(x) %in% c("integer", "double", "numeric")))
    expect_equal(convert_to_seconds(1, input_unit = "S"), 1)
    expect_equal(convert_to_seconds(1, input_unit = "M"), 60)
    expect_equal(convert_to_seconds(1, input_unit = "H"), 3600)
    expect_equal(convert_to_seconds(1, input_unit = "d"), 3600 * 24)
    expect_equal(convert_to_seconds(1, input_unit = "W"), 3600 * (24 * 7))
    expect_equal(convert_to_seconds(
        1, input_unit = "m", month_length = month_length), 3600 * (24 * 30))
    expect_equal(convert_to_seconds(
        1, input_unit = "y", year_length = year_length), 3600 * (24 * 30 * 12))
    # pi = C / d; d = 2r; C = 24; pi = 24 / 2r; pi = 12 / r; r = 12 / pi
    expect_equal(convert_to_seconds
                 (1, input_unit = "rad"), (12 / pi) * 60 * 60)
    # 360 deg = 24h; 360 deg = (24 * 60 * 60)s; deg = (24 * 60 * 60) / 360
    expect_equal(convert_to_seconds(
        1, input_unit = "deg"), (24 * 60 * 60) / 360)

    # if (is_time(x))
    expect_equal(convert_to_seconds(lubridate::dhours()), 3600)
    expect_equal(convert_to_seconds(lubridate::hours()), 3600)
    expect_equal(convert_to_seconds(as.difftime(3600, units = "secs")), 3600)
    expect_equal(convert_to_seconds(hms::parse_hm("01:00")), 3600)

    object <- convert_to_seconds( as.Date("2000-01-01"), ignore_date = FALSE)
    expect_equal(object, 946684800)

    object <- convert_to_seconds( as.Date("2000-01-01"), ignore_date = TRUE)
    expect_equal(object, 0)

    object <- convert_to_seconds(
        lubridate::as_datetime("2020-01-01 01:00:00"), ignore_date = FALSE)
    expect_equal(object, 1577840400)

    object <- convert_to_seconds(
        lubridate::as_datetime("2020-01-01 01:00:00"), ignore_date = TRUE)
    expect_equal(object, 3600)

    object <- convert_to_seconds(
        lubridate::as.interval(lubridate::dhours(), as.Date("2020-01-01")))
    expect_equal(object, 3600)
})

test_that("convert_to_seconds() | error test", {
    x <- lubridate::dhours()

    # Invalid values for `x`, `input_unit`, `month_length`, `year_length`,
    # `ignore_date`, and `quiet`
    expect_error(convert_to_seconds(list()))
    expect_error(convert_to_seconds(x, input_unit = "Z"))
    expect_error(convert_to_seconds(x, month_length = ""))
    expect_error(convert_to_seconds(x, month_length = -1))
    expect_error(convert_to_seconds(x, year_length = ""))
    expect_error(convert_to_seconds(x, year_length = -1))
    expect_error(convert_to_seconds(x, ignore_date = ""))
    expect_error(convert_to_seconds(x, quiet = ""))

    # !is_time(x) && is.null(input_unit)
    expect_error(convert_to_seconds(1))
})

test_that("convert_to_unit() | general test", {
    x <- lubridate::dhours()
    month_length <- 30 * 24 * 60 * 60
    year_length <- month_length * 12

    expect_equal(convert_to_unit(x, output_unit = "S"), 3600)
    expect_equal(convert_to_unit(x, output_unit = "M"), 60)
    expect_equal(convert_to_unit(x, output_unit = "H"), 1)
    expect_equal(convert_to_unit(x, output_unit = "d"), 1 / 24)
    expect_equal(convert_to_unit(x, output_unit = "W"), 1 / (24 * 7))
    expect_equal(convert_to_unit(x, output_unit = "m",
                                 month_length = month_length), 1 / (24 * 30))
    expect_equal(convert_to_unit(x, output_unit = "y",
                                 year_length = year_length,
                                 close_round = FALSE), 1 / (24 * 30 * 12))
    expect_equal(convert_to_unit(x, output_unit = "rad"), 0.2617994)
    expect_equal(convert_to_unit(x, output_unit = "deg"), 15)

    x <- lubridate::dhours(1.999)
    object <- convert_to_unit(x, output_unit = "H", close_round = FALSE)
    expect_equal(object, 1.999)
})

test_that("convert_to_unit() | error test", {
    x <- lubridate::dhours()

    # Invalid values for `output_unit` and `close_round`
    expect_error(convert_to_unit(x, output_unit = "Z", close_round = TRUE))
    expect_error(convert_to_unit(x, output_unit = "H", close_round = ""))
})

test_that("convert_to_date_time() | general test", {
    x <- 1
    object <- convert_to_date_time(x, "hms", input_unit = "H", quiet = TRUE)
    expect_equal(object, hms::parse_hm("01:00:00"))

    x <- 3600
    object <- convert_to_date_time(x, "duration", input_unit = "S",
                                   quiet = TRUE)
    expect_equal(object, lubridate::dhours(1))
})

test_that("convert_to_date_time() | error test", {
    # Invalid value for `x`
    expect_error(convert_to_date_time(lubridate::dhours(), "hms", "H"))
})
