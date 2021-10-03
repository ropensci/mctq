test_that("convert.Interval() | convert test", {
    expect_equal(convert(lubridate::as.interval(lubridate::dhours(),
                                                as.Date("2020-01-01")),
                         "logical", quiet = TRUE),
                 NA)
    expect_equal(convert(lubridate::as.interval(lubridate::dhours(),
                                                as.Date("2020-01-01")),
                         "character", quiet = TRUE),
                 "2020-01-01 UTC--2020-01-01 01:00:00 UTC")
    expect_equal(convert(lubridate::as.interval(lubridate::dhours(),
                                                as.Date("2020-01-01")),
                         "integer", quiet = TRUE),
                 3600L)
    expect_equal(convert(lubridate::as.interval(lubridate::dhours(),
                                                as.Date("2020-01-01")),
                         "double", quiet = TRUE),
                 3600)
    expect_equal(convert(lubridate::as.interval(lubridate::dhours(),
                                                as.Date("2020-01-01")),
                         "numeric", quiet = TRUE),
                 3600)
    expect_equal(convert(lubridate::as.interval(lubridate::dhours(),
                                                as.Date("2020-01-01")),
                         "Duration", quiet = TRUE),
                 lubridate::dhours())
    expect_equal(convert(lubridate::as.interval(lubridate::dhours(),
                                                as.Date("2020-01-01")),
                         "Period", quiet = TRUE),
                 lubridate::hours())
    expect_equal(convert(lubridate::as.interval(lubridate::dhours(),
                                                as.Date("2020-01-01")),
                         "difftime", quiet = TRUE),
                 as.difftime(3600, units = "secs"))
    expect_equal(convert(lubridate::as.interval(lubridate::dhours(),
                                                as.Date("2020-01-01")),
                         "hms", quiet = TRUE),
                 hms::parse_hm("01:00"))
    expect_equal(convert(lubridate::as.interval(lubridate::dhours(),
                                                as.Date("2020-01-01")),
                         "Date", quiet = TRUE),
                 as.Date(NA))
    expect_equal(convert(lubridate::as.interval(lubridate::dhours(),
                                                as.Date("2020-01-01")),
                         "POSIXct", tz = "EST", quiet = TRUE),
                 lubridate::force_tz(
                     lubridate::as_datetime("1970-01-01 01:00:00"), "EST"))
    expect_equal(convert(lubridate::as.interval(lubridate::dhours(),
                                                as.Date("2020-01-01")),
                         "POSIXlt", tz = "EST", quiet = TRUE),
                 as.POSIXlt(lubridate::force_tz(
                     lubridate::as_datetime("1970-01-01 01:00:00"), "EST")))
})

test_that("convert.Interval() | transform test", {
    expect_identical(convert(lubridate::as.interval(lubridate::dhours(),
                                                    as.Date("2020-01-01")),
                             "numeric", output_unit = "M", quiet = TRUE),
                     60)
})

test_that("convert.Interval() | warning test", {
    expect_message(convert(lubridate::as.interval(lubridate::dhours(),
                                                  as.Date("2020-01-01")),
                           "logical", quiet = FALSE),
                   "'x' cannot be converted to 'logical'.")

    expect_message(convert(lubridate::as.interval(lubridate::dhours(),
                                                  as.Date("2020-01-01")),
                           "integer", quiet = FALSE),
                   "'x' was converted to total of full seconds of the ")

    expect_message(convert(lubridate::as.interval(lubridate::dhours(),
                                                  as.Date("2020-01-01")),
                           "double", quiet = FALSE),
                   "'x' was converted to total of seconds of the ")
    expect_message(convert(lubridate::as.interval(lubridate::dhours(),
                                                  as.Date("2020-01-01")),
                           "numeric", quiet = FALSE),
                   "'x' was converted to total of seconds of the ")

    expect_message(convert(lubridate::as.interval(lubridate::dhours(),
                                                  as.Date("2020-01-01")),
                           "date", quiet = FALSE),
                   "There's no sigle date to convert.")

    expect_message(convert(lubridate::as.interval(lubridate::dhours(),
                                                  as.Date("2020-01-01")),
                           "duration", quiet = FALSE),
                   "'x' was converted to the interval time span.")
    expect_message(convert(lubridate::as.interval(lubridate::dhours(),
                                                  as.Date("2020-01-01")),
                           "period", quiet = FALSE),
                   "'x' was converted to the interval time span.")
    expect_message(convert(lubridate::as.interval(lubridate::dhours(),
                                                  as.Date("2020-01-01")),
                           "difftime", quiet = FALSE),
                   "'x' was converted to the interval time span.")
    expect_message(convert(lubridate::as.interval(lubridate::dhours(),
                                                  as.Date("2020-01-01")),
                           "hms", quiet = FALSE),
                   "'x' was converted to the interval time span.")

    expect_message(convert(lubridate::as.interval(lubridate::dhours(),
                                                  as.Date("2020-01-01")),
                           "posixct", quiet = FALSE),
                   "'x' was converted to the interval time span with ")
    expect_message(convert(lubridate::as.interval(lubridate::dhours(),
                                                  as.Date("2020-01-01")),
                           "posixlt", quiet = FALSE),
                   "'x' was converted to the interval time span with ")
})

test_that("convert.Interval() | error test", {
    expect_error(convert(lubridate::as.interval(lubridate::dhours(),
                                                as.Date("2020-01-01")),
                         1, tz = "", quiet = TRUE),
                 "Assertion on 'tolower\\(class\\)' failed")
    expect_error(convert(lubridate::as.interval(lubridate::dhours(),
                                                as.Date("2020-01-01")),
                         "hms", tz = 1, quiet = TRUE),
                 "Assertion on 'tz' failed")
    expect_error(convert(lubridate::as.interval(lubridate::dhours(),
                                                as.Date("2020-01-01")),
                         "hms", tz = "", quiet = ""),
                 "Assertion on 'quiet' failed")
})
