test_that("convert.character() | convert test", {
    expect_equal(convert("1", "logical", quiet = TRUE), NA)
    expect_equal(convert("1", "character", quiet = TRUE), "1")
    expect_equal(convert("1", "integer", quiet = TRUE), 1L)
    expect_equal(convert("1", "double", quiet = TRUE), 1)
    expect_equal(convert("1", "numeric", quiet = TRUE), 1)
    expect_equal(convert("1", "Duration", quiet = TRUE),
                 lubridate::duration("1"))
    expect_equal(convert("1", "Period", quiet = TRUE), lubridate::period("1"))
    expect_equal(convert("1", "difftime", quiet = TRUE),
                 lubridate::as.difftime("NA", units = "secs"))
    expect_equal(convert("1", "hms", quiet = TRUE), hms::hms(NA))
    expect_equal(convert("1", "Date", quiet = TRUE), lubridate::as_date(NA))
    expect_equal(convert("1", "POSIXct", tz = "EST", quiet = TRUE),
                 lubridate::force_tz(lubridate::as_datetime(NA), "EST"))
    expect_equal(convert("1", "POSIXlt", tz = "EST", quiet = TRUE),
                 as.POSIXlt(lubridate::force_tz(
                     lubridate::as_datetime(NA), "EST")))
})

test_that("convert.character() | transform test", {
    expect_identical(convert("1", "numeric", input_unit = "H",
                             output_unit = "M", quiet = TRUE),
                     60)
    expect_identical(convert("1", "hms", orders = "H"), hms::parse_hm("01:00"))
})

test_that("convert.character() | warning test", {
    expect_message(convert("1", "logical", quiet = FALSE),
                   "'x' was converted 'as is'.")
    expect_message(convert("1", "duration", quiet = FALSE),
                   "'x' was converted 'as is'.")
    expect_message(convert("1", "difftime", quiet = FALSE),
                   "'difftime' units was set to seconds.")
})

test_that("convert.character() | error test", {
    expect_error(convert("1", 1, tz = "", quiet = TRUE),
                 "Assertion on 'tolower\\(class\\)' failed")
    expect_error(convert("1", "hms", tz = 1, quiet = TRUE),
                 "Assertion on 'tz' failed")
    expect_error(convert("1", "hms", tz = "", quiet = ""),
                 "Assertion on 'quiet' failed")
})
