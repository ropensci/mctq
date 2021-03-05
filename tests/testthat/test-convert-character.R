test_that("convert.character() | convert test", {
    x <- "1"
    quiet <- TRUE

    expect_equal(convert(x, "logical", quiet = quiet), NA)
    expect_equal(convert(x, "character", quiet = quiet), "1")
    expect_equal(convert(x, "integer", quiet = quiet), 1L)
    expect_equal(convert(x, "double", quiet = quiet), 1)
    expect_equal(convert(x, "numeric", quiet = quiet), 1)

    expect_equal(convert(x, "Duration", quiet = quiet),
                 lubridate::duration("1"))
    expect_equal(convert(x, "Period", quiet = quiet), lubridate::period("1"))
    expect_equal(convert(x, "difftime", quiet = quiet),
                 lubridate::as.difftime("NA", units = "secs"))
    expect_equal(convert(x, "hms", quiet = quiet), hms::hms(NA))
    expect_equal(convert(x, "Date", quiet = quiet), lubridate::as_date(NA))

    tz <- "EST"
    object <- convert(x, "POSIXct", tz = tz, quiet = quiet)
    expected <- lubridate::force_tz(
        lubridate::as_datetime(NA), tz)
    expect_equal(object, expected)

    object <- convert(x, "POSIXlt", tz = tz, quiet = quiet)
    expected <- as.POSIXlt(lubridate::force_tz(
        lubridate::as_datetime(NA), tz))
    expect_equal(object, expected)
})

test_that("convert.character() | transform test", {
    x <- "1"

    object <- convert(x, "numeric", input_unit = "H", output_unit = "M",
                      quiet = TRUE)
    expect_identical(object, 60)

    object <- convert(x, "hms", orders = "H")
    expect_identical(object, hms::parse_hm("01:00"))
})

test_that("convert.character() | warning test", {
    x <- "1"

    # "'x' was converted 'as is'."
    expect_warning(convert(x, "logical", quiet = FALSE))
    # "'x' was converted 'as is'. This can produce [...]"
    expect_warning(convert(x, "duration", quiet = FALSE))
    # "'difftime' units was set to seconds."
    expect_warning(convert(x, "difftime", quiet = FALSE))
})

test_that("convert.character() | error test", {
    x <- "1"

    # Invalid values for `class, `tz`, and `quiet`
    expect_error(convert(x, 1, tz = "", quiet = TRUE))
    expect_error(convert(x, "hms", tz = 1, quiet = TRUE))
    expect_error(convert(x, "hms", tz = "", quiet = ""))
})
