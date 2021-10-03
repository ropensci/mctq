test_that("convert() | general test", {
    expect_equal(convert(NA, "character", quiet = TRUE), NA)
})

test_that("convert.Period() | general test", {
    expect_equal(convert(lubridate::hours(), "character", quiet = TRUE),
                 "01:00:00")
})

test_that("convert.difftime() | general test", {
    expect_equal(convert(as.difftime(1, units = "hours"), "character",
                         quiet = TRUE), "01:00:00")
})

test_that("convert_tu() | general test", {
    expect_equal(convert_tu(lubridate::dhours(), "M", quiet = TRUE), 60)
})

test_that("convert_ut() | general test", {
    expect_equal(convert_ut(1, "hms", "H", quiet = TRUE),
                 hms::parse_hm("01:00"))
})

test_that("convert_uu() | general test", {
    expect_equal(convert_uu(1, "rad", "H", quiet = TRUE), (12 / pi))
})

test_that("convert_pt() | general test", {
    expect_equal(convert_pt(1, "hms", "H", quiet = TRUE),
                 hms::parse_hm("01:00"))
})

test_that("convert_pu() | general test", {
    expect_equal(convert_pu(1, "H", "deg", quiet = TRUE), 15)
})

test_that("parser_1() | general test", {
    expect_equal(parser_1(" 1 ", "duration", input_unit = "H", quiet = TRUE),
                 lubridate::dhours())
    expect_equal(parser_1(1, "double", input_unit = "H", output_unit = "deg",
                          quiet = TRUE),
                 15)
    expect_equal(parser_1(1, "hms", input_unit = "H", quiet = TRUE),
                 hms::parse_hm("01:00"))
})

test_that("parser_1() | error test", {
    expect_error(parser_1(c("1", " NA", "a"), "hms"),
                 "To convert 'character' objects to units, all values ")
    expect_error(parser_1(1, "hms", output_unit = "deg"),
                 "'x' can only be converted to 'output_unit' if 'input_unit' ")
    expect_error(parser_1(1, "hms", input_unit = "H",  output_unit = "rad"),
                 "'x' can only be converted to 'output_unit' if 'class' ")
    expect_error(parser_1(1, "integer"),
                 "input_unit' and 'output_unit' must both be assigned, or be ")
})

test_that("parser_2() | general test", {
    expect_equal(parser_2(" 1 ", "duration", orders = "H", quiet = TRUE),
                 lubridate::dhours())
    expect_equal(parser_2(1, "integer", orders = "H", output_unit = "deg",
                          quiet = TRUE),
                 15L)
    expect_equal(parser_2(1, "numeric", orders = "H", output_unit = "rad",
                          quiet = TRUE),
                 (2 * pi) / 24)
    expect_equal(parser_2(1, "hms", orders = "H", quiet = TRUE),
                 hms::parse_hm("01:00:00"))
})

test_that("parser_3() | general test", {
    expect_equal(parser_3(lubridate::dhours(), "integer", output_unit = "H",
                          quiet = TRUE),
                 1L)
    expect_equal(parser_3(lubridate::dhours(), "numeric", output_unit = "M",
                          quiet = TRUE),
                 60)
})

test_that("parser_3() | error test", {
    expect_error(parser_3(lubridate::dhours(), "hms"),
                 "'x' can be only be converted to 'output_unit' if 'class' ")
})

test_that("parse_to_date_time() | general test", {
    expect_equal(parse_to_date_time(" 1", "H"), hms::parse_hm("01:00"))
    expect_equal(parse_to_date_time("NA", "H"), lubridate::as_datetime(NA))
    expect_equal(parse_to_date_time(1, "H"), hms::parse_hm("01:00"))
    expect_equal(parse_to_date_time("01:59", "HM"), hms::parse_hm("01:59"))
    expect_equal(parse_to_date_time("01:59:59", "HMS"),
                 hms::parse_hms("01:59:59"))
    expect_equal(parse_to_date_time("01:59", "HS"),
                 lubridate::ymd_hms("0000-01-01 01:00:59"))
    expect_equal(parse_to_date_time("2000 1", "YH"),
                 lubridate::ymd_hms("2000-01-01 01:00:00"))
})

test_that("parse_to_date_time() | warning test", {
    expect_message(parse_to_date_time("a", "H", quiet = FALSE),
                   "All formats failed to parse. No formats found.")
    expect_message(parse_to_date_time(c("1", "a"), "H", quiet = FALSE),
                   "1 failed to parse.")
})

test_that("parse_to_date_time() | error test", {
    expect_error(parse_to_date_time(lubridate::dhours()),
                 "Assertion on 'x' failed")
    expect_error(parse_to_date_time(1, orders = 1),
                 "Assertion on 'orders' failed")
    expect_error(parse_to_date_time(1, tz = 1),
                 "Assertion on 'tz' failed")
    expect_error(parse_to_date_time(1, quiet = ""),
                 "Assertion on 'quiet' failed")
})

test_that("convert_to_seconds() | general test", {
    expect_equal(convert_to_seconds(1, input_unit = "S"), 1)
    expect_equal(convert_to_seconds(1, input_unit = "M"), 60)
    expect_equal(convert_to_seconds(1, input_unit = "H"), 3600)
    expect_equal(convert_to_seconds(1, input_unit = "d"), 3600 * 24)
    expect_equal(convert_to_seconds(1, input_unit = "W"), 3600 * (24 * 7))
    expect_equal(convert_to_seconds(1, input_unit = "m",
                                    month_length = 30 * 24 * 60 * 60),
                 3600 * (24 * 30))
    expect_equal(convert_to_seconds(1, input_unit = "y",
                                    year_length = 12 * 30 * 24 * 60 * 60),
                 3600 * (12 * 30 * 24))
    # pi = C / d; d = 2r; C = 24; pi = 24 / 2r; pi = 12 / r; r = 12 / pi
    expect_equal(convert_to_seconds(1, input_unit = "rad"),
                 (12 / pi) * 60 * 60)
    # 360 deg = 24h; 360 deg = (24 * 60 * 60)s; deg = (24 * 60 * 60) / 360
    expect_equal(convert_to_seconds(1, input_unit = "deg"),
                 (24 * 60 * 60) / 360)

    # if (test_temporal(x))
    expect_equal(convert_to_seconds(lubridate::dhours()), 3600)
    expect_equal(convert_to_seconds(lubridate::hours()), 3600)
    expect_equal(convert_to_seconds(as.difftime(3600, units = "secs")), 3600)
    expect_equal(convert_to_seconds(hms::parse_hm("01:00")), 3600)
    expect_equal(convert_to_seconds( as.Date("2000-01-01"),
                                     ignore_date = FALSE),
                 946684800)
    expect_equal(convert_to_seconds( as.Date("2000-01-01"), ignore_date = TRUE),
                 0)
    expect_equal(convert_to_seconds(
        lubridate::as_datetime("2020-01-01 01:00:00"), ignore_date = FALSE),
        1577840400)
    expect_equal(convert_to_seconds(
        lubridate::as_datetime("2020-01-01 01:00:00"), ignore_date = TRUE),
        3600)
    expect_equal(convert_to_seconds(
        lubridate::as.interval(lubridate::dhours(), as.Date("2020-01-01"))),
        3600)
})

test_that("convert_to_seconds() | error test", {
    expect_error(convert_to_seconds(list()),
                 "Assertion on 'x' failed")
    expect_error(convert_to_seconds(lubridate::dhours(), input_unit = "Z"),
                 "Assertion on 'input_unit' failed")
    expect_error(convert_to_seconds(lubridate::dhours(), month_length = ""),
                 "Assertion on 'month_length' failed")
    expect_error(convert_to_seconds(lubridate::dhours(), month_length = -1),
                 "Assertion on 'month_length' failed")
    expect_error(convert_to_seconds(lubridate::dhours(), year_length = ""),
                 "Assertion on 'year_length' failed")
    expect_error(convert_to_seconds(lubridate::dhours(), year_length = -1),
                 "Assertion on 'year_length' failed")
    expect_error(convert_to_seconds(lubridate::dhours(), ignore_date = ""),
                 "Assertion on 'ignore_date' failed")
    expect_error(convert_to_seconds(lubridate::dhours(), quiet = ""),
                 "Assertion on 'quiet' failed")

    expect_error(convert_to_seconds(1),
                 "When 'x' is 'integer' or 'numeric', 'input_unit' ")
})

test_that("convert_to_unit() | general test", {
    expect_equal(convert_to_unit(lubridate::dhours(), output_unit = "S"), 3600)
    expect_equal(convert_to_unit(lubridate::dhours(), output_unit = "M"), 60)
    expect_equal(convert_to_unit(lubridate::dhours(), output_unit = "H"), 1)
    expect_equal(convert_to_unit(lubridate::dhours(), output_unit = "d"),
                 1 / 24)
    expect_equal(convert_to_unit(lubridate::dhours(), output_unit = "W"),
                 1 / (24 * 7))
    expect_equal(convert_to_unit(lubridate::dhours(), output_unit = "m",
                                 month_length = 30 * 24 * 60 * 60),
                 1 / (24 * 30))
    expect_equal(convert_to_unit(lubridate::dhours(), output_unit = "y",
                                 year_length = 12 * 30 * 24 * 60 * 60,
                                 close_round = FALSE),
                 1 / (24 * 30 * 12))
    expect_equal(convert_to_unit(lubridate::dhours(), output_unit = "rad"),
                 0.2617994)
    expect_equal(convert_to_unit(lubridate::dhours(), output_unit = "deg"), 15)
    expect_equal(convert_to_unit(lubridate::dhours(1.999), output_unit = "H",
                                 close_round = FALSE),
                 1.999)
})

test_that("convert_to_unit() | error test", {
    expect_error(convert_to_unit(lubridate::dhours(), output_unit = "Z",
                                 close_round = TRUE),
                 "Assertion on 'output_unit' failed")
    expect_error(convert_to_unit(lubridate::dhours(), output_unit = "H",
                                 close_round = ""),
                 "Assertion on 'close_round' failed")
})

test_that("convert_to_date_time() | general test", {
    expect_equal(convert_to_date_time(1, "hms", input_unit = "H",
                                      quiet = TRUE),
                 hms::parse_hm("01:00:00"))
    expect_equal(convert_to_date_time(3600, "duration", input_unit = "S",
                                      quiet = TRUE),
                 lubridate::dhours(1))
})

test_that("convert_to_date_time() | error test", {
    expect_error(convert_to_date_time(lubridate::dhours(), "hms", "H"),
                 "Assertion on 'x' failed")
})
