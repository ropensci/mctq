test_that("round_time() | scalar test", {
    object <- round_time(lubridate::dmilliseconds(123456789))
    expected <- lubridate::duration(123457)
    expect_equal(object, expected)

    object <- round_time(lubridate::microseconds(123456789))
    expected <- round(lubridate::microseconds(123456789))
    expect_equal(object, expected)

    object <- round_time(as.difftime(12345.6789, units = "secs"))
    expected <- as.difftime(12346, units = "secs")
    expect_equal(object, expected)

    object <- round_time(hms::as_hms(12345.6789))
    expected <- hms::hms(12346)
    expect_equal(object, expected)

    object <- round_time(lubridate::as_datetime(12345.6789, tz = "EST"))
    expected <- lubridate::as_datetime(12346, tz = "EST")
    expect_equal(object, expected)

    object <- round_time(as.POSIXlt(lubridate::as_datetime(12345.6789,
                                                           tz = "EST")))
    expected <- as.POSIXlt(lubridate::as_datetime(12346, tz = "EST"))
    expect_equal(object, expected)
})

test_that("round_time() | vector test", {
    x <- c(hms::hms(12345.6789), hms::as_hms(98765.4321))
    object <- round_time(x)
    expected <- c(hms::hms(12346), hms::as_hms(98765))
    expect_equal(object, expected)
})

test_that("round_time() | error test", {
    # Invalid values for `x`
    expect_error(round_time(1))
})
