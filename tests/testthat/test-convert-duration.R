test_that("convert.Duration() | convert test", {
    x <- lubridate::dhours()
    quiet <- TRUE

    expect_equal(convert(x, "character", quiet = quiet), "01:00:00")
    expect_equal(convert(x, "integer", quiet = quiet), 3600L)
    expect_equal(convert(x, "double", quiet = quiet), 3600)
    expect_equal(convert(x, "numeric", quiet = quiet), 3600)
    expect_equal(convert(x, "Duration", quiet = quiet), x)
    expect_equal(convert(x, "Period", quiet = quiet), lubridate::hours())
    expect_equal(convert(x, "difftime", quiet = quiet),
                 as.difftime(3600, units = "secs"))
    expect_equal(convert(x, "hms", quiet = quiet), hms::parse_hm("01:00"))
    expect_equal(convert(x, "Date", quiet = quiet), as.Date(NA))

    tz <- "EST"
    object <- convert(x, "POSIXct", tz = tz, quiet = quiet)
    expected <- lubridate::force_tz(
        lubridate::as_datetime("1970-01-01 01:00:00"), tz)
    expect_equal(object, expected)

    object <- convert(x, "POSIXlt", tz = tz, quiet = quiet)
    expected <- as.POSIXlt(lubridate::force_tz(
        lubridate::as_datetime("1970-01-01 01:00:00"), tz))
    expect_equal(object, expected)
})

test_that("convert.Duration() | error test", {
    # Invalid values for `class, `tz`, and `quiet`
    expect_error(
        convert.Duration(lubridate::dhours(), 1, tz = "", quiet = TRUE)
    )
    expect_error(
        convert.Duration(lubridate::dhours(), "hms", tz = 1, quiet = TRUE)
    )
    expect_error(
        convert.Duration(lubridate::dhours(), "hms", tz = "", quiet = "")
    )
})
