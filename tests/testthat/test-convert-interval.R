test_that("convert.Interval() | convert test", {
    x <- lubridate::as.interval(lubridate::dhours(), as.Date("2020-01-01"))
    quiet <- TRUE

    expect_equal(convert(x, "logical", quiet = quiet), NA)
    expect_equal(convert(x, "character", quiet = quiet),
                 "2020-01-01 UTC--2020-01-01 01:00:00 UTC")
    expect_equal(convert(x, "integer", quiet = quiet), 3600L)
    expect_equal(convert(x, "double", quiet = quiet), 3600)
    expect_equal(convert(x, "numeric", quiet = quiet), 3600)
    expect_equal(convert(x, "Duration", quiet = quiet), lubridate::dhours())
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

test_that("convert.Interval() | transform test", {
    x <- lubridate::as.interval(lubridate::dhours(), as.Date("2020-01-01"))
    object <- convert(x, "numeric", output_unit = "M", quiet = TRUE)
    expect_identical(object, 60)
})

test_that("convert.Interval() | warning test", {
    x <- lubridate::as.interval(lubridate::dhours(), as.Date("2020-01-01"))

    "'x' cannot be converted to 'logical'"
    expect_warning(convert(x, "logical", quiet = FALSE))
    # "'x' was converted to total of full seconds of the [...]"
    expect_warning(convert(x, "integer", quiet = FALSE))
    # "'x' was converted to total of seconds of the interval  [...]"
    expect_warning(convert(x, "double", quiet = FALSE))
    expect_warning(convert(x, "numeric", quiet = FALSE))
    # "There's no sigle date to convert."
    expect_warning(convert(x, "date", quiet = FALSE))

    # "'x' was converted to the interval time span."
    classes <- c("duration", "period", "difftime", "hms")
    for (i in classes) expect_warning(convert(x, i, quiet = FALSE))

    # "'x' was converted to the interval time span with [...]"
    classes <- c("posixct", "posixlt")
    for (i in classes) expect_warning(convert(x, i, quiet = FALSE))
})

test_that("convert.Interval() | error test", {
    x <- lubridate::as.interval(lubridate::dhours(), as.Date("2020-01-01"))

    # Invalid values for `class, `tz`, and `quiet`
    expect_error(convert(x, 1, tz = "", quiet = TRUE))
    expect_error(convert(x, "hms", tz = 1, quiet = TRUE))
    expect_error(convert(x, "hms", tz = "", quiet = ""))
})
