test_that("convert.hms() | convert test", {
    expect_equal(convert(hms::parse_hm("01:00"), "logical", quiet = TRUE), NA)
    expect_equal(convert(hms::parse_hm("01:00"), "character", quiet = TRUE),
                 "01:00:00")
    expect_equal(convert(hms::parse_hm("01:00"), "integer", quiet = TRUE), 3600L)
    expect_equal(convert(hms::parse_hm("01:00"), "double", quiet = TRUE), 3600)
    expect_equal(convert(hms::parse_hm("01:00"), "numeric", quiet = TRUE), 3600)
    expect_equal(convert(hms::parse_hm("01:00"), "Duration", quiet = TRUE),
                 lubridate::dhours())
    expect_equal(convert(hms::parse_hm("01:00"), "Period", quiet = TRUE),
                 lubridate::hours())
    expect_equal(convert(hms::parse_hm("01:00"), "difftime", quiet = TRUE),
                 as.difftime(3600, units = "secs"))
    expect_equal(convert(hms::parse_hm("01:00"), "hms", quiet = TRUE),
                 hms::parse_hm("01:00"))
    expect_equal(convert(hms::parse_hm("01:00"), "Date", quiet = TRUE),
                 as.Date(NA))
    expect_equal(convert(hms::parse_hm("01:00"), "POSIXct", tz = "EST",
                         quiet = TRUE),
                 lubridate::force_tz(
                     lubridate::as_datetime("1970-01-01 01:00:00"), "EST"))
    expect_equal(convert(hms::parse_hm("01:00"), "POSIXlt", tz = "EST",
                         quiet = TRUE),
                 as.POSIXlt(lubridate::force_tz(
                     lubridate::as_datetime("1970-01-01 01:00:00"), "EST")))
})

test_that("convert.hms() | transform test", {
    expect_identical(convert(hms::parse_hm("01:00"), "numeric",
                             output_unit = "M", quiet = TRUE),
                     60)
})

test_that("convert.hms() | warning test", {
    expect_message(convert(hms::parse_hm("01:00"), "logical", quiet = FALSE),
                   "'x' cannot be converted to 'logical'.")
    expect_message(convert(hms::parse_hm("01:00"), "character", quiet = FALSE),
                   "'x' was formatted as HMS.")
    expect_message(convert(hms::parse_hm("01:00"), "integer", quiet = FALSE),
                   "'x' was converted to total of full seconds.")
    expect_message(convert(hms::parse_hm("01:00"), "double", quiet = FALSE),
                   "'x' was converted to total of seconds.")
    expect_message(convert(hms::parse_hm("01:00"), "numeric", quiet = FALSE),
                   "'x' was converted to total of seconds.")
    expect_message(convert(hms::parse_hm("01:00"), "difftime", quiet = FALSE),
                   "'difftime' units was set to seconds.")
    expect_message(convert(hms::parse_hm("01:00"), "date", quiet = FALSE),
                   "There's no date to convert.")
})

test_that("convert.hms() | error test", {
    expect_error(convert(hms::parse_hm("01:00"), 1, tz = "", quiet = TRUE),
                 "Assertion on 'tolower\\(class\\)' failed")
    expect_error(convert(hms::parse_hm("01:00"), "hms", tz = 1, quiet = TRUE),
                 "Assertion on 'tz' failed")
    expect_error(convert(hms::parse_hm("01:00"), "hms", tz = "", quiet = ""),
                 "Assertion on 'quiet' failed")
})
