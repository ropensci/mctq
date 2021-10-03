test_that("convert.POSIXt() | convert test", {
    expect_equal(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                         "logical", quiet = TRUE),
                 NA)
    expect_equal(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                         "character", quiet = TRUE),
                 "2000-01-01 01:00:00")
    expect_equal(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                         "integer", quiet = TRUE),
                 946688400L)
    expect_equal(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                         "double", quiet = TRUE),
                 946688400)
    expect_equal(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                         "numeric", quiet = TRUE),
                 946688400)
    expect_equal(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                         "Duration", quiet = TRUE),
                 lubridate::dhours())
    expect_equal(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                         "Period", quiet = TRUE),
                 lubridate::hours())
    expect_equal(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                         "difftime", quiet = TRUE),
                 as.difftime(3600, units = "secs"))
    expect_equal(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                         "hms", quiet = TRUE),
                 hms::parse_hm("01:00"))
    expect_equal(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                         "Date", quiet = TRUE),
                 as.Date("2000-01-01"))
    expect_equal(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                         "POSIXct", tz = "EST", quiet = TRUE),
                 lubridate::force_tz(
                     lubridate::as_datetime("2000-01-01 01:00:00"), "EST"))
    expect_equal(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                         "POSIXlt", tz = "EST", quiet = TRUE),
                 as.POSIXlt(lubridate::force_tz(
                     lubridate::as_datetime("2000-01-01 01:00:00"), "EST")))
})

test_that("convert.POSIXt() | transform test", {
    expect_identical(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                             "numeric", output_unit = "M", quiet = TRUE),
                     60)
})

test_that("convert.POSIXt() | warning test", {
    expect_message(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                           "logical", quiet = FALSE),
                   "'x' cannot be converted to 'logical'.")

    expect_message(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                           "integer", quiet = FALSE),
                   "'x' was converted to total of full seconds since ")

    expect_message(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                           "double", quiet = FALSE),
                   "'x' was converted to total of seconds since ")
    expect_message(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                           "numeric", quiet = FALSE),
                   "'x' was converted to total of seconds since ")

    expect_message(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                           "duration", quiet = FALSE),
                   "'x' date was discarded. Only 'x' time ")
    expect_message(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                           "difftime", quiet = FALSE),
                   "'x' date was discarded. Only 'x' time ")
    expect_message(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                           "hms", quiet = FALSE),
                   "'x' date was discarded. Only 'x' time ")
})

test_that("convert.POSIXt() | error test", {
    expect_error(convert(lubridate::as_datetime("2000-01-01 01:00:00"), 1,
                         tz = "", quiet = TRUE),
                 "Assertion on 'tolower\\(class\\)' failed")
    expect_error(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                         "hms", tz = 1, quiet = TRUE),
                 "Assertion on 'tz' failed")
    expect_error(convert(lubridate::as_datetime("2000-01-01 01:00:00"),
                         "hms", tz = "", quiet = ""),
                 "Assertion on 'quiet' failed")
})
