test_that("convert.POSIXt() | convert test", {
    x <- lubridate::as_datetime("2000-01-01 01:00:00")
    quiet <- TRUE

    expect_equal(convert(x, "character", quiet = quiet), "2000-01-01 01:00:00")
    expect_equal(convert(x, "integer", quiet = quiet), 946688400L)
    expect_equal(convert(x, "double", quiet = quiet), 946688400)
    expect_equal(convert(x, "numeric", quiet = quiet), 946688400)
    expect_equal(convert(x, "Duration", quiet = quiet), lubridate::dhours())
    expect_equal(convert(x, "Period", quiet = quiet), lubridate::hours())
    expect_equal(convert(x, "difftime", quiet = quiet),
                 as.difftime(3600, units = "secs"))
    expect_equal(convert(x, "hms", quiet = quiet), hms::parse_hm("01:00"))
    expect_equal(convert(x, "Date", quiet = quiet), as.Date("2000-01-01"))

    tz <- "EST"
    object <- convert(x, "POSIXct", tz = tz, quiet = quiet)
    expected <- lubridate::force_tz(
        lubridate::as_datetime("2000-01-01 01:00:00"), tz)
    expect_equal(object, expected)

    object <- convert(x, "POSIXlt", tz = tz, quiet = quiet)
    expected <- as.POSIXlt(lubridate::force_tz(
        lubridate::as_datetime("2000-01-01 01:00:00"), tz))
    expect_equal(object, expected)
})

test_that("convert.POSIXt() | transform test", {
    x <- lubridate::as_datetime("2000-01-01 01:00:00")
    object <- convert(x, "numeric", output_unit = "M", quiet = TRUE)
    expect_identical(object, 60)
})

test_that("convert.POSIXt() | warning test", {
    x <- lubridate::as_datetime("2000-01-01 01:00:00")

    # "'x' was converted to total of full seconds since [...]"
    expect_warning(convert(x, "integer"))
    # "'x' was converted to total of seconds since [...]"
    expect_warning(convert(x, "double"))
    expect_warning(convert(x, "numeric"))

    # "'x' date was discarded. Only 'x' time  [...]"
    classes <- c("duration", "period", "difftime", "hms")
    for (i in classes) expect_warning(convert(x, i))
})

test_that("convert.POSIXt() | error test", {
    x <- lubridate::as_datetime("2000-01-01 01:00:00")

    # Invalid values for `class, `tz`, and `quiet`
    expect_error(convert(x, 1, tz = "", quiet = TRUE))
    expect_error(convert(x, "hms", tz = 1, quiet = TRUE))
    expect_error(convert(x, "hms", tz = "", quiet = ""))
})
