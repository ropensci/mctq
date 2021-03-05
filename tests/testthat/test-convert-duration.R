test_that("convert.Duration() | convert test", {
    x <- lubridate::dhours()
    quiet <- TRUE

    expect_equal(convert(x, "logical", quiet = quiet), NA)
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

test_that("convert.Duration() | transform test", {
    x <- lubridate::dhours()
    object <- convert(x, "numeric", output_unit = "M", quiet = TRUE)
    expect_identical(object, 60)
})

test_that("convert.Duration() | warning test", {
    x <- lubridate::dhours()

    "'x' cannot be converted to 'logical'"
    expect_warning(convert(x, "logical", quiet = FALSE))
    # "'x' was formatted as HMS."
    expect_warning(convert(x, "character", quiet = FALSE))
    # "'x' was converted to total of full seconds."
    expect_warning(convert(x, "integer", quiet = FALSE))
    # "'x' was converted to total of seconds."
    expect_warning(convert(x, "double", quiet = FALSE))
    expect_warning(convert(x, "numeric", quiet = FALSE))
    # "'difftime' units was set to seconds."
    expect_warning(convert(x, "difftime", quiet = FALSE))
    # "There's no date to convert."
    expect_warning(convert(x, "date", quiet = FALSE))
})

test_that("convert.Duration() | error test", {
    x <- lubridate::dhours()

    # Invalid values for `class, `tz`, and `quiet`
    expect_error(convert(x, 1, tz = "", quiet = TRUE))
    expect_error(convert(x, "hms", tz = 1, quiet = TRUE))
    expect_error(convert(x, "hms", tz = "", quiet = ""))
})
