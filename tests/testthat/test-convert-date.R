test_that("convert.Date() | convert test", {
    expect_equal(convert(as.Date("2000-01-01"), "logical", quiet = TRUE), NA)
    expect_equal(convert(as.Date("2000-01-01"), "character", quiet = TRUE),
                 "2000-01-01")
    expect_equal(convert(as.Date("2000-01-01"), "integer", quiet = TRUE),
                 10957L)
    expect_equal(convert(as.Date("2000-01-01"), "double", quiet = TRUE), 10957)
    expect_equal(convert(as.Date("2000-01-01"), "numeric", quiet = TRUE), 10957)
    expect_equal(convert(as.Date("2000-01-01"), "Duration", quiet = TRUE),
                 lubridate::as.duration(NA))
    expect_equal(convert(as.Date("2000-01-01"), "Period", quiet = TRUE),
                 lubridate::as.period(NA))
    expect_equal(convert(as.Date("2000-01-01"), "difftime", quiet = TRUE),
                 lubridate::as.difftime(lubridate::as.duration(NA)))
    expect_equal(convert(as.Date("2000-01-01"), "hms", quiet = TRUE),
                 hms::as_hms(NA))
    expect_equal(convert(as.Date("2000-01-01"), "Date", quiet = TRUE),
                 as.Date("2000-01-01"))
    expect_equal(convert(as.Date("2000-01-01"), "POSIXct", tz = "EST",
                         quiet = TRUE),
                 lubridate::force_tz(lubridate::as_datetime("2000-01-01"),
                                     "EST"))
    expect_equal(convert(as.Date("2000-01-01"), "POSIXlt", tz = "EST",
                         quiet = TRUE),
                 as.POSIXlt(lubridate::force_tz(
                     lubridate::as_datetime("2000-01-01"), "EST")))
})

test_that("convert.Date() | warning test", {
    expect_message(convert(as.Date("2000-01-01"), "logical", quiet = FALSE),
                   "'x' cannot be converted to 'logical'.")

    expect_message(convert(as.Date("2000-01-01"), "integer", quiet = FALSE),
                   "'x' was converted to total of days since '1970-01-01'")
    expect_message(convert(as.Date("2000-01-01"), "double", quiet = FALSE),
                   "'x' was converted to total of days since '1970-01-01'")
    expect_message(convert(as.Date("2000-01-01"), "numeric", quiet = FALSE),
                   "'x' was converted to total of days since '1970-01-01'")

    expect_message(convert(as.Date("2000-01-01"), "duration", quiet = FALSE),
                   "There's no time to convert.")
    expect_message(convert(as.Date("2000-01-01"), "period", quiet = FALSE),
                   "There's no time to convert.")
    expect_message(convert(as.Date("2000-01-01"), "difftime", quiet = FALSE),
                   "There's no time to convert.")
    expect_message(convert(as.Date("2000-01-01"), "hms", quiet = FALSE),
                   "There's no time to convert.")
})

test_that("convert.Date() | error test", {
    expect_error(convert(as.Date("2000-01-01"), 1, tz = "", quiet = TRUE),
                 "Assertion on 'tolower\\(class\\)' failed")
    expect_error(convert(as.Date("2000-01-01"), "hms", tz = 1, quiet = TRUE),
                 "Assertion on 'tz' failed")
    expect_error(convert(as.Date("2000-01-01"), "hms", tz = "", quiet = ""),
                 "Assertion on 'quiet' failed")
})
