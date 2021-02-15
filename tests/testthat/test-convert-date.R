test_that("convert.Date() | convert test", {
    x <- as.Date("2000-01-01")
    quiet <- TRUE

    expect_equal(convert(x, "character", quiet = quiet), "2000-01-01")
    expect_equal(convert(x, "integer", quiet = quiet), 10957L)
    expect_equal(convert(x, "double", quiet = quiet), 10957)
    expect_equal(convert(x, "numeric", quiet = quiet), 10957)
    expect_equal(convert(x, "Duration", quiet = quiet),
                 lubridate::as.duration(NA))
    expect_equal(convert(x, "Period", quiet = quiet), lubridate::as.period(NA))
    expect_equal(convert(x, "difftime", quiet = quiet),
                 lubridate::as.difftime(lubridate::as.duration(NA)))
    expect_equal(convert(x, "hms", quiet = quiet), hms::as_hms(NA))
    expect_equal(convert(x, "Date", quiet = quiet), x)

    tz <- "EST"
    object <- convert(x, "POSIXct", tz = tz, quiet = quiet)
    expected <- lubridate::force_tz(lubridate::as_datetime("2000-01-01"), tz)
    expect_equal(object, expected)

    object <- convert(x, "POSIXlt", tz = tz, quiet = quiet)
    expected <- as.POSIXlt(lubridate::force_tz(
        lubridate::as_datetime("2000-01-01"), tz))
    expect_equal(object, expected)
})

test_that("convert.Date() | warning test", {
    x <- as.Date("2000-01-01")

    # "'x' was converted to total of days since  [...]"
    classes <- c("integer", "double", "numeric")
    for (i in classes) expect_warning(convert(x, i))

    # "There's no time to convert."
    classes <- c("duration", "period", "difftime", "hms")
    for (i in classes) expect_warning(convert(x, i))
})

test_that("convert.Date() | error test", {
    x <- as.Date("2000-01-01")

    # Invalid values for `class, `tz`, and `quiet`
    expect_error(convert(x, 1, tz = "", quiet = TRUE))
    expect_error(convert(x, "hms", tz = 1, quiet = TRUE))
    expect_error(convert(x, "hms", tz = "", quiet = ""))
})
