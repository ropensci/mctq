test_that("convert.Duration() | convert test", {
    expect_equal(convert(lubridate::dhours(), "logical", quiet = TRUE), NA)
    expect_equal(convert(lubridate::dhours(), "character", quiet = TRUE),
                 "01:00:00")
    expect_equal(convert(lubridate::dhours(), "integer", quiet = TRUE), 3600L)
    expect_equal(convert(lubridate::dhours(), "double", quiet = TRUE), 3600)
    expect_equal(convert(lubridate::dhours(), "numeric", quiet = TRUE), 3600)
    expect_equal(convert(lubridate::dhours(), "Duration", quiet = TRUE),
                 lubridate::dhours())
    expect_equal(convert(lubridate::dhours(), "Period", quiet = TRUE),
                 lubridate::hours())
    expect_equal(convert(lubridate::dhours(), "difftime", quiet = TRUE),
                 as.difftime(3600, units = "secs"))
    expect_equal(convert(lubridate::dhours(), "hms", quiet = TRUE),
                 hms::parse_hm("01:00"))
    expect_equal(convert(lubridate::dhours(), "Date", quiet = TRUE),
                 as.Date(NA))
    expect_equal(convert(lubridate::dhours(), "POSIXct", tz = "EST",
                         quiet = TRUE),
                 lubridate::force_tz(
                     lubridate::as_datetime("1970-01-01 01:00:00"), "EST"))
    expect_equal(convert(lubridate::dhours(), "POSIXlt", tz = "EST",
                         quiet = TRUE),
                 as.POSIXlt(lubridate::force_tz(
                     lubridate::as_datetime("1970-01-01 01:00:00"), "EST")))
})

test_that("convert.Duration() | transform test", {
    expect_identical(convert(lubridate::dhours(), "numeric", output_unit = "M",
                             quiet = TRUE),
                     60)
})

test_that("convert.Duration() | warning test", {
    expect_warning(convert(lubridate::dhours(), "logical", quiet = FALSE),
                   "'x' cannot be converted to 'logical'.")
    expect_warning(convert(lubridate::dhours(), "character", quiet = FALSE),
                   "'x' was formatted as HMS.")
    expect_warning(convert(lubridate::dhours(), "integer", quiet = FALSE),
                   "'x' was converted to total of full seconds.")
    expect_warning(convert(lubridate::dhours(), "double", quiet = FALSE),
                   "'x' was converted to total of seconds.")
    expect_warning(convert(lubridate::dhours(), "numeric", quiet = FALSE),
                   "'x' was converted to total of seconds.")
    expect_warning(convert(lubridate::dhours(), "difftime", quiet = FALSE),
                   "'difftime' units was set to seconds.")
    expect_warning(convert(lubridate::dhours(), "date", quiet = FALSE),
                   "There's no date to convert.")
})

test_that("convert.Duration() | error test", {
    expect_error(convert(lubridate::dhours(), 1, tz = "", quiet = TRUE),
                 "Assertion on 'tolower\\(class\\)' failed")
    expect_error(convert(lubridate::dhours(), "hms", tz = 1, quiet = TRUE),
                 "Assertion on 'tz' failed")
    expect_error(convert(lubridate::dhours(), "hms", tz = "", quiet = ""),
                 "Assertion on 'quiet' failed")
})
